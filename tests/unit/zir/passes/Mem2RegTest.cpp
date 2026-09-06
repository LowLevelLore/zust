#include <doctest/doctest.h>

#include "zir/PassManager.hpp"
#include "zir/Printer.hpp"
#include "zir/TextParser.hpp"
#include "zir/Verifier.hpp"
#include "zir/passes/Mem2Reg.hpp"

using namespace zust::zir;

namespace {
    Module parseOrFail(const char *text) {
        std::string error;
        std::optional<Module> m = TextParser::parse(text, error);
        REQUIRE_MESSAGE(m.has_value(), "parse failed: " << error);
        return std::move(*m);
    }

    bool runToFixpoint(Mem2RegPass &pass, Function &fn, AnalysisManager &am) {
        bool changedAny = false;
        while (pass.run(fn, am)) changedAny = true;
        return changedAny;
    }
}  // namespace

TEST_CASE("Mem2Reg promotes a straight-line alloca with no branching") {
    Module m = parseOrFail(R"(module "t.zz" target = "generic"

fn @f() -> i64 {
^entry:
    %p    = alloca i64
    %c1   = const i64 5
    store i64 %c1, %p
    %v    = load i64, %p
    ret i64 %v
}
)");
    Function &fn = const_cast<Function &>(m.functions()[0]);
    Mem2RegPass pass;
    AnalysisManager am;
    CHECK(runToFixpoint(pass, fn, am));
    CHECK(Verifier::verify(m).empty());

    std::string printed = Printer::print(m);
    CAPTURE(printed);
    CHECK(printed.find("alloca") == std::string::npos);
    CHECK(printed.find("load") == std::string::npos);
    CHECK(printed.find("store") == std::string::npos);
}

TEST_CASE("Mem2Reg introduces a block parameter (merge) for a value defined differently on two arms") {
    Module m = parseOrFail(R"(module "t.zz" target = "generic"

fn @f(%cond: u1) -> i64 {
^entry(%cond: u1):
    %p    = alloca i64
    condbr %cond, ^then, ^else

^then:
    %a    = const i64 1
    store i64 %a, %p
    br ^merge

^else:
    %b    = const i64 2
    store i64 %b, %p
    br ^merge

^merge:
    %v    = load i64, %p
    ret i64 %v
}
)");
    Function &fn = const_cast<Function &>(m.functions()[0]);
    Mem2RegPass pass;
    AnalysisManager am;
    CHECK(runToFixpoint(pass, fn, am));
    CHECK(Verifier::verify(m).empty());

    std::string printed = Printer::print(m);
    CAPTURE(printed);
    CHECK(printed.find("alloca") == std::string::npos);
    CHECK(printed.find("load") == std::string::npos);
    // The merge block should have picked up a new parameter fed 1 from
    // ^then and 2 from ^else.
    CHECK(printed.find("br ^merge(%a)") != std::string::npos);
    CHECK(printed.find("br ^merge(%b)") != std::string::npos);
}

TEST_CASE("Mem2Reg promotes a loop induction variable via a header block parameter") {
    Module m = parseOrFail(R"(module "t.zz" target = "generic"

fn @f() -> i64 {
^entry:
    %p    = alloca i64
    %c0   = const i64 0
    store i64 %c0, %p
    br ^cond

^cond:
    %i    = load i64, %p
    %lim  = const i64 10
    %test = icmp slt i64 %i, %lim
    condbr %test, ^body, ^end

^body:
    %i2   = load i64, %p
    %one  = const i64 1
    %next = add i64 %i2, %one
    store i64 %next, %p
    br ^cond

^end:
    %r    = load i64, %p
    ret i64 %r
}
)");
    Function &fn = const_cast<Function &>(m.functions()[0]);
    Mem2RegPass pass;
    AnalysisManager am;
    CHECK(runToFixpoint(pass, fn, am));
    CHECK(Verifier::verify(m).empty());

    std::string printed = Printer::print(m);
    CAPTURE(printed);
    CHECK(printed.find("alloca") == std::string::npos);
    CHECK(printed.find("load") == std::string::npos);
    CHECK(printed.find("store") == std::string::npos);
    // ^cond should now carry a block parameter merging entry's initial 0
    // and ^body's back-edge increment.
    CHECK(printed.find("^cond(") != std::string::npos);
}

TEST_CASE("Mem2Reg leaves an alloca alone when a load is not dominated by any store") {
    // ^b never stores to %p before reading it -- the standard SSA
    // construction would need an `undef` value here, which ZIR does not
    // have, so this alloca must be left as alloca/load/store rather than
    // promoted (see Mem2Reg.hpp's class-level comment: this is exactly the
    // "block scoping is currently disabled" gap CLAUDE.md documents).
    Module m = parseOrFail(R"(module "t.zz" target = "generic"

fn @f(%cond: u1) -> i64 {
^entry(%cond: u1):
    %p    = alloca i64
    condbr %cond, ^a, ^b

^a:
    %c1   = const i64 1
    store i64 %c1, %p
    br ^merge

^b:
    br ^merge

^merge:
    %v    = load i64, %p
    ret i64 %v
}
)");
    Function &fn = const_cast<Function &>(m.functions()[0]);
    Mem2RegPass pass;
    AnalysisManager am;
    CHECK_FALSE(runToFixpoint(pass, fn, am));
    CHECK(Verifier::verify(m).empty());

    std::string printed = Printer::print(m);
    CAPTURE(printed);
    CHECK(printed.find("alloca") != std::string::npos);
    CHECK(printed.find("load") != std::string::npos);
}

TEST_CASE("Mem2Reg does not promote an alloca whose address escapes into a call argument") {
    Module m = parseOrFail(R"(module "t.zz" target = "generic"

declare void @takes_ptr(ptr)

fn @f() -> void {
^entry:
    %p    = alloca i64
    %c1   = const i64 1
    store i64 %c1, %p
    call void @takes_ptr(%p)
    ret void
}
)");
    Function &fn = const_cast<Function &>(m.functions()[0]);
    Mem2RegPass pass;
    AnalysisManager am;
    CHECK_FALSE(runToFixpoint(pass, fn, am));

    std::string printed = Printer::print(m);
    CAPTURE(printed);
    CHECK(printed.find("alloca") != std::string::npos);
}
