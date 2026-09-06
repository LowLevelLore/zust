#include <doctest/doctest.h>

#include "zir/PassManager.hpp"
#include "zir/Printer.hpp"
#include "zir/TextParser.hpp"
#include "zir/Verifier.hpp"
#include "zir/passes/ConstFold.hpp"

using namespace zust::zir;

namespace {
    Module parseOrFail(const char *text) {
        std::string error;
        std::optional<Module> m = TextParser::parse(text, error);
        REQUIRE_MESSAGE(m.has_value(), "parse failed: " << error);
        return std::move(*m);
    }
}  // namespace

TEST_CASE("ConstFold folds an integer add of two consts into a single const") {
    Module m = parseOrFail(R"(module "t.zz" target = "generic"

fn @f() -> i64 {
^entry:
    %a    = const i64 2
    %b    = const i64 3
    %sum  = add i64 %a, %b
    ret i64 %sum
}
)");
    Function &fn = const_cast<Function &>(m.functions()[0]);
    ConstFoldPass pass(m);
    AnalysisManager am;
    CHECK(pass.run(fn, am));
    CHECK_FALSE(pass.run(fn, am));  // fixpoint: nothing left to fold

    CHECK(Verifier::verify(m).empty());
    std::string printed = Printer::print(m);
    CAPTURE(printed);
    CHECK(printed.find("add i64") == std::string::npos);
    CHECK(printed.find("const i64 5") != std::string::npos);
}

TEST_CASE("ConstFold declines to fold an integer division by a constant zero") {
    Module m = parseOrFail(R"(module "t.zz" target = "generic"

fn @f() -> i64 {
^entry:
    %a    = const i64 7
    %z    = const i64 0
    %q    = sdiv i64 %a, %z
    ret i64 %q
}
)");
    Function &fn = const_cast<Function &>(m.functions()[0]);
    ConstFoldPass pass(m);
    AnalysisManager am;
    CHECK_FALSE(pass.run(fn, am));

    std::string printed = Printer::print(m);
    CAPTURE(printed);
    CHECK(printed.find("sdiv") != std::string::npos);
}

TEST_CASE("ConstFold folds a comparison of two consts into a boolean const") {
    Module m = parseOrFail(R"(module "t.zz" target = "generic"

fn @f() -> u1 {
^entry:
    %a    = const i64 10
    %b    = const i64 20
    %cmp  = icmp slt i64 %a, %b
    ret u1 %cmp
}
)");
    Function &fn = const_cast<Function &>(m.functions()[0]);
    ConstFoldPass pass(m);
    AnalysisManager am;
    CHECK(pass.run(fn, am));

    CHECK(Verifier::verify(m).empty());
    std::string printed = Printer::print(m);
    CAPTURE(printed);
    CHECK(printed.find("icmp") == std::string::npos);
    CHECK(printed.find("const u1 1") != std::string::npos);
}

TEST_CASE("ConstFold folds a chain (trunc of a folded add) across repeated fixpoint calls") {
    Module m = parseOrFail(R"(module "t.zz" target = "generic"

fn @f() -> u8 {
^entry:
    %a    = const i64 250
    %b    = const i64 10
    %sum  = add i64 %a, %b
    %t    = trunc %sum to u8
    ret u8 %t
}
)");
    Function &fn = const_cast<Function &>(m.functions()[0]);
    ConstFoldPass pass(m);
    AnalysisManager am;
    bool changedAny = false;
    while (pass.run(fn, am))
        changedAny = true;
    CHECK(changedAny);

    CHECK(Verifier::verify(m).empty());
    std::string printed = Printer::print(m);
    CAPTURE(printed);
    // 250 + 10 = 260, truncated to u8 wraps to 4.
    CHECK(printed.find("const u8 4") != std::string::npos);
}
