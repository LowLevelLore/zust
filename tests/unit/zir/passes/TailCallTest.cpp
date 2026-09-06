#include <doctest/doctest.h>

#include "zir/PassManager.hpp"
#include "zir/Printer.hpp"
#include "zir/TextParser.hpp"
#include "zir/Verifier.hpp"
#include "zir/passes/TailCall.hpp"

using namespace zust::zir;

namespace {
    Module parseOrFail(const char *text) {
        std::string error;
        std::optional<Module> m = TextParser::parse(text, error);
        REQUIRE_MESSAGE(m.has_value(), "parse failed: " << error);
        return std::move(*m);
    }
}  // namespace

TEST_CASE("TailCall turns a self-recursive tail call into a loop back edge, keeping entry predecessor-free") {
    Module m = parseOrFail(R"(module "t.zz" target = "generic"

fn @count_down(%n: i64) -> i64 {
^entry(%n: i64):
    %zero = const i64 0
    %done = icmp eq i64 %n, %zero
    condbr %done, ^base, ^rec

^base:
    ret i64 %zero

^rec:
    %one  = const i64 1
    %next = sub i64 %n, %one
    %r    = call i64 @count_down(%next)
    ret i64 %r
}
)");
    Function &fn = const_cast<Function &>(m.functions()[0]);
    TailCallPass pass(m);
    AnalysisManager am;
    CHECK(pass.run(fn, am));
    CHECK(Verifier::verify(m).empty());

    std::string printed = Printer::print(m);
    CAPTURE(printed);
    CHECK(printed.find("call") == std::string::npos);  // the call is gone -- a plain branch now

    // Entry must still have zero real predecessors -- the only occurrence
    // of "^entry" in the printed text should be the block's own label line;
    // none of the branches (including the rewritten tail call) may target
    // it (LLVM's own hard rule this pass exists to respect; ZIR itself
    // doesn't check this, so this pins it at the ZIR level too).
    // "^entry(" (not the "^entry.tailrec(" header block) -- exactly once,
    // its own label line.
    std::size_t count = 0;
    for (std::size_t pos = printed.find("^entry("); pos != std::string::npos; pos = printed.find("^entry(", pos + 1))
        ++count;
    CHECK(count == 1);
}

TEST_CASE("TailCall leaves a non-tail-position self call alone (result used before returning)") {
    Module m = parseOrFail(R"(module "t.zz" target = "generic"

fn @factorial(%n: i64) -> i64 {
^entry(%n: i64):
    %one  = const i64 1
    %base = icmp sle i64 %n, %one
    condbr %base, ^baseCase, ^rec

^baseCase:
    ret i64 %one

^rec:
    %sub  = sub i64 %n, %one
    %rc   = call i64 @factorial(%sub)
    %mul  = mul i64 %n, %rc
    ret i64 %mul
}
)");
    Function &fn = const_cast<Function &>(m.functions()[0]);
    TailCallPass pass(m);
    AnalysisManager am;
    CHECK_FALSE(pass.run(fn, am));

    std::string printed = Printer::print(m);
    CAPTURE(printed);
    CHECK(printed.find("call i64 @factorial") != std::string::npos);
}

TEST_CASE("TailCall declines when the entry block still has an alloca") {
    Module m = parseOrFail(R"(module "t.zz" target = "generic"

fn @f(%n: i64) -> i64 {
^entry(%n: i64):
    %p    = alloca i64
    store i64 %n, %p
    %v    = load i64, %p
    %r    = call i64 @f(%v)
    ret i64 %r
}
)");
    Function &fn = const_cast<Function &>(m.functions()[0]);
    TailCallPass pass(m);
    AnalysisManager am;
    CHECK_FALSE(pass.run(fn, am));
}
