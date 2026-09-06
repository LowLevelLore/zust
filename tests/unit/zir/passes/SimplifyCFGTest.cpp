#include <doctest/doctest.h>

#include "zir/PassManager.hpp"
#include "zir/Printer.hpp"
#include "zir/TextParser.hpp"
#include "zir/Verifier.hpp"
#include "zir/passes/SimplifyCFG.hpp"

using namespace zust::zir;

namespace {
    Module parseOrFail(const char *text) {
        std::string error;
        std::optional<Module> m = TextParser::parse(text, error);
        REQUIRE_MESSAGE(m.has_value(), "parse failed: " << error);
        return std::move(*m);
    }

    bool runToFixpoint(SimplifyCFGPass &pass, Function &fn, AnalysisManager &am) {
        bool changedAny = false;
        while (pass.run(fn, am)) changedAny = true;
        return changedAny;
    }
}  // namespace

TEST_CASE("SimplifyCFG turns a condbr on a known constant into an unconditional br") {
    Module m = parseOrFail(R"(module "t.zz" target = "generic"

fn @f() -> i64 {
^entry:
    %c1   = const u1 1
    condbr %c1, ^then, ^else

^then:
    %a    = const i64 1
    ret i64 %a

^else:
    %b    = const i64 2
    ret i64 %b
}
)");
    Function &fn = const_cast<Function &>(m.functions()[0]);
    SimplifyCFGPass pass;
    AnalysisManager am;
    CHECK(runToFixpoint(pass, fn, am));

    CHECK(Verifier::verify(m).empty());
    std::string printed = Printer::print(m);
    CAPTURE(printed);
    CHECK(printed.find("condbr") == std::string::npos);
    // The always-taken arm's `ret i64 1` should now be reachable directly
    // from entry (merged straight in), and the untaken `^else` arm's `ret
    // i64 2` should be gone from the reachable text entirely.
    bool keptTakenArm = printed.find("ret i64 %a") != std::string::npos ||
                       printed.find("const i64 1") != std::string::npos;
    CHECK(keptTakenArm);
    CHECK(printed.find("const i64 2") == std::string::npos);
}

TEST_CASE("SimplifyCFG merges a block into its sole predecessor") {
    Module m = parseOrFail(R"(module "t.zz" target = "generic"

fn @f() -> i64 {
^entry:
    br ^next

^next:
    %a    = const i64 42
    ret i64 %a
}
)");
    Function &fn = const_cast<Function &>(m.functions()[0]);
    SimplifyCFGPass pass;
    AnalysisManager am;
    CHECK(runToFixpoint(pass, fn, am));

    CHECK(Verifier::verify(m).empty());
    // entry itself should now carry the merged content -- only one block
    // should still have any real content in it.
    int nonEmptyBlocks = 0;
    for (std::size_t i = 0; i < fn.blockCount(); ++i) {
        if (!fn.block(BlockId(static_cast<BlockId::Value>(i))).insts().empty())
            ++nonEmptyBlocks;
    }
    CHECK(nonEmptyBlocks == 1);
}

TEST_CASE("SimplifyCFG substitutes a merged block's parameters with the branch's arguments") {
    // ^next has a block parameter that only entry ever feeds (a single
    // predecessor with one argument) -- merging must rewrite every use of
    // that parameter inside ^next to the value entry actually passed.
    Module m = parseOrFail(R"(module "t.zz" target = "generic"

fn @f() -> i64 {
^entry:
    %c1   = const i64 7
    br ^next(%c1)

^next(%p: i64):
    %doubled = add i64 %p, %p
    ret i64 %doubled
}
)");
    Function &fn = const_cast<Function &>(m.functions()[0]);
    SimplifyCFGPass pass;
    AnalysisManager am;
    CHECK(runToFixpoint(pass, fn, am));

    CHECK(Verifier::verify(m).empty());
    std::string printed = Printer::print(m);
    CAPTURE(printed);
    // The parameter is substituted away everywhere it would have been
    // *used*; the merged-away block's own now-unreachable header still
    // lexically declares it (harmless -- nothing branches there to supply
    // it any more), so check the actual computation, not just absence of
    // the text "%p".
    CHECK(printed.find("add i64 %c1, %c1") != std::string::npos);
    CHECK(printed.find("add i64 %p") == std::string::npos);
}

TEST_CASE("SimplifyCFG clears a whole chain of blocks orphaned by a folded condbr, not just the first") {
    // Once entry always takes ^then, ^else and everything ^else alone leads
    // to (^deadNext) become unreachable -- and must end up with *zero*
    // predecessors of their own, not just be skipped, or the Verifier's
    // predecessor check flags ^deadNext as "dangling" (it would still have
    // ^else as a predecessor even though ^else itself is dead).
    Module m = parseOrFail(R"(module "t.zz" target = "generic"

fn @f() -> i64 {
^entry:
    %c1   = const u1 1
    condbr %c1, ^then, ^else

^then:
    %a    = const i64 1
    ret i64 %a

^else:
    br ^deadNext

^deadNext:
    %b    = const i64 2
    ret i64 %b
}
)");
    Function &fn = const_cast<Function &>(m.functions()[0]);
    SimplifyCFGPass pass;
    AnalysisManager am;
    CHECK(runToFixpoint(pass, fn, am));

    CHECK(Verifier::verify(m).empty());
}
