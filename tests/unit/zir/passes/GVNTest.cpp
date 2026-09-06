#include <doctest/doctest.h>

#include "zir/PassManager.hpp"
#include "zir/Printer.hpp"
#include "zir/TextParser.hpp"
#include "zir/Verifier.hpp"
#include "zir/passes/GVN.hpp"

using namespace zust::zir;

namespace {
    Module parseOrFail(const char *text) {
        std::string error;
        std::optional<Module> m = TextParser::parse(text, error);
        REQUIRE_MESSAGE(m.has_value(), "parse failed: " << error);
        return std::move(*m);
    }
}  // namespace

TEST_CASE("GVN reuses an earlier identical computation instead of recomputing it") {
    Module m = parseOrFail(R"(module "t.zz" target = "generic"

fn @f(%x: i64, %y: i64) -> i64 {
^entry(%x: i64, %y: i64):
    %a    = add i64 %x, %y
    %b    = add i64 %x, %y
    %sum  = add i64 %a, %b
    ret i64 %sum
}
)");
    Function &fn = const_cast<Function &>(m.functions()[0]);
    GVNPass pass;
    AnalysisManager am;
    CHECK(pass.run(fn, am));
    CHECK(Verifier::verify(m).empty());

    std::string printed = Printer::print(m);
    CAPTURE(printed);
    CHECK(printed.find("add i64 %a, %a") != std::string::npos);
}

TEST_CASE("GVN never unifies two different constants of the same type") {
    // A regression pin for a real bug: the signature used to omit the
    // constant's own bit pattern, so every `const i64` collided regardless
    // of value.
    Module m = parseOrFail(R"(module "t.zz" target = "generic"

fn @f() -> i64 {
^entry:
    %a    = const i64 10
    %b    = const i64 20
    %sum  = add i64 %a, %b
    ret i64 %sum
}
)");
    Function &fn = const_cast<Function &>(m.functions()[0]);
    GVNPass pass;
    AnalysisManager am;
    CHECK_FALSE(pass.run(fn, am));

    std::string printed = Printer::print(m);
    CAPTURE(printed);
    CHECK(printed.find("const i64 10") != std::string::npos);
    CHECK(printed.find("const i64 20") != std::string::npos);
}

TEST_CASE("GVN does not unify computations across sibling branches, only along a dominating path") {
    Module m = parseOrFail(R"(module "t.zz" target = "generic"

fn @f(%c: u1, %x: i64, %y: i64) -> i64 {
^entry(%c: u1, %x: i64, %y: i64):
    condbr %c, ^then, ^else

^then:
    %a    = add i64 %x, %y
    ret i64 %a

^else:
    %b    = add i64 %x, %y
    ret i64 %b
}
)");
    Function &fn = const_cast<Function &>(m.functions()[0]);
    GVNPass pass;
    AnalysisManager am;
    // Neither branch dominates the other, so nothing should be unified --
    // if this returned true while producing a dangling cross-branch
    // reference, the Verifier would catch it (checked below regardless).
    pass.run(fn, am);
    CHECK(Verifier::verify(m).empty());
}

TEST_CASE("GVN never treats a Load as pure (no unification across a possible aliasing store)") {
    Module m = parseOrFail(R"(module "t.zz" target = "generic"

fn @f(%p: ptr) -> i64 {
^entry(%p: ptr):
    %a    = load i64, %p
    %b    = load i64, %p
    %sum  = add i64 %a, %b
    ret i64 %sum
}
)");
    Function &fn = const_cast<Function &>(m.functions()[0]);
    GVNPass pass;
    AnalysisManager am;
    CHECK_FALSE(pass.run(fn, am));
}
