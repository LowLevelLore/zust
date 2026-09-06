#include <doctest/doctest.h>

#include "zir/PassManager.hpp"
#include "zir/Printer.hpp"
#include "zir/TextParser.hpp"
#include "zir/Verifier.hpp"
#include "zir/passes/SCCP.hpp"

using namespace zust::zir;

namespace {
    Module parseOrFail(const char *text) {
        std::string error;
        std::optional<Module> m = TextParser::parse(text, error);
        REQUIRE_MESSAGE(m.has_value(), "parse failed: " << error);
        return std::move(*m);
    }
}  // namespace

TEST_CASE("SCCP folds a merge parameter fed the same constant from every predecessor") {
    Module m = parseOrFail(R"(module "t.zz" target = "generic"

fn @f(%c: u1) -> i64 {
^entry(%c: u1):
    condbr %c, ^then, ^else

^then:
    %a    = const i64 7
    br ^merge(%a)

^else:
    %b    = const i64 7
    br ^merge(%b)

^merge(%p: i64):
    ret i64 %p
}
)");
    Function &fn = const_cast<Function &>(m.functions()[0]);
    SCCPPass pass;
    AnalysisManager am;
    CHECK(pass.run(fn, am));
    CHECK(Verifier::verify(m).empty());

    std::string printed = Printer::print(m);
    CAPTURE(printed);
    CHECK(printed.find("ret i64 %v") != std::string::npos);  // ret now reads the fresh const, not the old param
    CHECK(printed.find("const i64 7") != std::string::npos);
}

TEST_CASE("SCCP leaves a merge parameter alone when predecessors disagree") {
    Module m = parseOrFail(R"(module "t.zz" target = "generic"

fn @f(%c: u1) -> i64 {
^entry(%c: u1):
    condbr %c, ^then, ^else

^then:
    %a    = const i64 7
    br ^merge(%a)

^else:
    %b    = const i64 8
    br ^merge(%b)

^merge(%p: i64):
    ret i64 %p
}
)");
    Function &fn = const_cast<Function &>(m.functions()[0]);
    SCCPPass pass;
    AnalysisManager am;
    CHECK_FALSE(pass.run(fn, am));
    CHECK(Verifier::verify(m).empty());
}

TEST_CASE("SCCP does not touch entry's own parameters (real function arguments, never a merge)") {
    Module m = parseOrFail(R"(module "t.zz" target = "generic"

fn @f(%x: i64) -> i64 {
^entry(%x: i64):
    ret i64 %x
}
)");
    Function &fn = const_cast<Function &>(m.functions()[0]);
    SCCPPass pass;
    AnalysisManager am;
    CHECK_FALSE(pass.run(fn, am));
}
