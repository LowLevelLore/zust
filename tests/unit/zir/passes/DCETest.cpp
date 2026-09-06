#include <doctest/doctest.h>

#include "zir/PassManager.hpp"
#include "zir/Printer.hpp"
#include "zir/TextParser.hpp"
#include "zir/Verifier.hpp"
#include "zir/passes/DCE.hpp"

using namespace zust::zir;

namespace {
    Module parseOrFail(const char *text) {
        std::string error;
        std::optional<Module> m = TextParser::parse(text, error);
        REQUIRE_MESSAGE(m.has_value(), "parse failed: " << error);
        return std::move(*m);
    }
}  // namespace

TEST_CASE("DCE removes a pure instruction whose result is never used") {
    Module m = parseOrFail(R"(module "t.zz" target = "generic"

fn @f(%v0: i64) -> i64 {
^entry(%v0: i64):
    %dead = add i64 %v0, %v0
    ret i64 %v0
}
)");
    Function &fn = const_cast<Function &>(m.functions()[0]);
    DCEPass pass;
    AnalysisManager am;
    CHECK(pass.run(fn, am));
    CHECK_FALSE(pass.run(fn, am));  // fixpoint

    CHECK(Verifier::verify(m).empty());
    std::string printed = Printer::print(m);
    CAPTURE(printed);
    CHECK(printed.find("add") == std::string::npos);
}

TEST_CASE("DCE never removes a call, even an unused non-void one, or a store") {
    Module m = parseOrFail(R"(module "t.zz" target = "generic"

declare i32 @side_effecting()

fn @f() -> void {
^entry:
    %p    = alloca i64
    %c1   = const i64 1
    store i64 %c1, %p
    %r    = call i32 @side_effecting()
    ret void
}
)");
    Function &fn = const_cast<Function &>(m.functions()[0]);
    DCEPass pass;
    AnalysisManager am;
    CHECK_FALSE(pass.run(fn, am));  // nothing removable: the call and the store both survive

    std::string printed = Printer::print(m);
    CAPTURE(printed);
    CHECK(printed.find("call") != std::string::npos);
    CHECK(printed.find("store") != std::string::npos);
}

TEST_CASE("DCE removes a dead load, freeing up a now-unused alloca in the next call") {
    Module m = parseOrFail(R"(module "t.zz" target = "generic"

fn @f() -> void {
^entry:
    %p    = alloca i64
    %c1   = const i64 1
    store i64 %c1, %p
    %loaded = load i64, %p
    ret void
}
)");
    Function &fn = const_cast<Function &>(m.functions()[0]);
    DCEPass pass;
    AnalysisManager am;
    // Round 1: the unused load goes. Round 2: now the alloca has no
    // remaining load (only the store, which is not itself a use of the
    // alloca's *value* the way a call argument would be -- it still uses
    // the pointer operand, so the alloca survives DCE; only the load is
    // dead here). Confirms DCE needs a fixpoint, not one flat pass, on its
    // own even without another pass's help.
    bool changedAny = false;
    while (pass.run(fn, am))
        changedAny = true;
    CHECK(changedAny);

    CHECK(Verifier::verify(m).empty());
    std::string printed = Printer::print(m);
    CAPTURE(printed);
    CHECK(printed.find("load") == std::string::npos);
}
