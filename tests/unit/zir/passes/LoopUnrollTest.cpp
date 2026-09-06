#include <doctest/doctest.h>

#include "zir/PassManager.hpp"
#include "zir/Printer.hpp"
#include "zir/TextParser.hpp"
#include "zir/Verifier.hpp"
#include "zir/passes/LoopUnroll.hpp"

using namespace zust::zir;

namespace {
    Module parseOrFail(const char *text) {
        std::string error;
        std::optional<Module> m = TextParser::parse(text, error);
        REQUIRE_MESSAGE(m.has_value(), "parse failed: " << error);
        return std::move(*m);
    }
}  // namespace

TEST_CASE("LoopUnroll fully unrolls a small constant-trip-count counting loop") {
    // for (i = 0; i < 3; i = i + 1) {} -- three iterations, no other
    // loop-carried state (LoopUnroll's own scope restriction).
    Module m = parseOrFail(R"(module "t.zz" target = "generic"

fn @f() -> i64 {
^entry:
    %c0   = const i64 0
    br ^cond(%c0)

^cond(%i: i64):
    %lim  = const i64 3
    %test = icmp slt i64 %i, %lim
    condbr %test, ^body, ^end

^body:
    %one  = const i64 1
    %next = add i64 %i, %one
    br ^cond(%next)

^end:
    ret i64 %i
}
)");
    Function &fn = const_cast<Function &>(m.functions()[0]);
    LoopUnrollPass pass(m);
    AnalysisManager am;
    CHECK(pass.run(fn, am));
    CHECK(Verifier::verify(m).empty());

    std::string printed = Printer::print(m);
    CAPTURE(printed);
    CHECK(printed.find("condbr") == std::string::npos);      // the loop's own test is gone -- trip count is known
    CHECK(printed.find("ret i64 %v") != std::string::npos);  // returns a fresh const, not the old %i
}

TEST_CASE("LoopUnroll declines a loop whose trip count is not compile-time constant") {
    Module m = parseOrFail(R"(module "t.zz" target = "generic"

fn @f(%n: i64) -> i64 {
^entry(%n: i64):
    %c0   = const i64 0
    br ^cond(%c0)

^cond(%i: i64):
    %test = icmp slt i64 %i, %n
    condbr %test, ^body, ^end

^body:
    %one  = const i64 1
    %next = add i64 %i, %one
    br ^cond(%next)

^end:
    ret i64 %i
}
)");
    Function &fn = const_cast<Function &>(m.functions()[0]);
    LoopUnrollPass pass(m);
    AnalysisManager am;
    CHECK_FALSE(pass.run(fn, am));
}

TEST_CASE("LoopUnroll declines a loop with more than one loop-carried value") {
    // A regression pin for a real bug: unrolling only substituted the
    // counter, leaving a second loop-carried header parameter (an
    // accumulator) referencing the now-unreachable header.
    Module m = parseOrFail(R"(module "t.zz" target = "generic"

fn @f() -> i64 {
^entry:
    %c0   = const i64 0
    br ^cond(%c0, %c0)

^cond(%i: i64, %acc: i64):
    %lim  = const i64 3
    %test = icmp slt i64 %i, %lim
    condbr %test, ^body, ^end

^body:
    %one    = const i64 1
    %next   = add i64 %i, %one
    %nextAcc = add i64 %acc, %i
    br ^cond(%next, %nextAcc)

^end:
    ret i64 %acc
}
)");
    Function &fn = const_cast<Function &>(m.functions()[0]);
    LoopUnrollPass pass(m);
    AnalysisManager am;
    CHECK_FALSE(pass.run(fn, am));
}

TEST_CASE("LoopUnroll declines a loop whose trip count exceeds the cap") {
    Module m = parseOrFail(R"(module "t.zz" target = "generic"

fn @f() -> i64 {
^entry:
    %c0   = const i64 0
    br ^cond(%c0)

^cond(%i: i64):
    %lim  = const i64 100000
    %test = icmp slt i64 %i, %lim
    condbr %test, ^body, ^end

^body:
    %one  = const i64 1
    %next = add i64 %i, %one
    br ^cond(%next)

^end:
    ret i64 %i
}
)");
    Function &fn = const_cast<Function &>(m.functions()[0]);
    LoopUnrollPass pass(m);
    AnalysisManager am;
    CHECK_FALSE(pass.run(fn, am));
}
