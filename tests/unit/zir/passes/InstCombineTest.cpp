#include <doctest/doctest.h>

#include "zir/PassManager.hpp"
#include "zir/Printer.hpp"
#include "zir/TextParser.hpp"
#include "zir/Verifier.hpp"
#include "zir/passes/DCE.hpp"
#include "zir/passes/InstCombine.hpp"

using namespace zust::zir;

namespace {
    Module parseOrFail(const char *text) {
        std::string error;
        std::optional<Module> m = TextParser::parse(text, error);
        REQUIRE_MESSAGE(m.has_value(), "parse failed: " << error);
        return std::move(*m);
    }
}  // namespace

TEST_CASE("InstCombine simplifies x+0, x*1, and x*0") {
    Module m = parseOrFail(R"(module "t.zz" target = "generic"

fn @f(%x: i64) -> i64 {
^entry(%x: i64):
    %z    = const i64 0
    %o    = const i64 1
    %a    = add i64 %x, %z
    %b    = mul i64 %a, %o
    %c    = mul i64 %b, %z
    ret i64 %c
}
)");
    Function &fn = const_cast<Function &>(m.functions()[0]);
    InstCombinePass pass;
    AnalysisManager am;
    bool changedAny = false;
    while (pass.run(fn, am)) changedAny = true;
    CHECK(changedAny);
    CHECK(Verifier::verify(m).empty());

    // Run DCE too so the final printed form only shows what's left live --
    // easier to assert on than a pile of now-dead bitcasts.
    DCEPass dce;
    while (dce.run(fn, am)) {
    }
    std::string printed = Printer::print(m);
    CAPTURE(printed);
    CHECK(printed.find("const i64 0") != std::string::npos);
    CHECK(printed.find("ret i64 %c") != std::string::npos);
}

TEST_CASE("InstCombine folds x - x to a constant zero") {
    Module m = parseOrFail(R"(module "t.zz" target = "generic"

fn @f(%x: i64) -> i64 {
^entry(%x: i64):
    %d    = sub i64 %x, %x
    ret i64 %d
}
)");
    Function &fn = const_cast<Function &>(m.functions()[0]);
    InstCombinePass pass;
    AnalysisManager am;
    CHECK(pass.run(fn, am));
    CHECK(Verifier::verify(m).empty());

    std::string printed = Printer::print(m);
    CAPTURE(printed);
    CHECK(printed.find("sub") == std::string::npos);
    CHECK(printed.find("const i64 0") != std::string::npos);
}
