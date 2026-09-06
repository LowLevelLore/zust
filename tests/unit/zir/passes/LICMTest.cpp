#include <doctest/doctest.h>

#include "zir/PassManager.hpp"
#include "zir/Printer.hpp"
#include "zir/TextParser.hpp"
#include "zir/Verifier.hpp"
#include "zir/passes/LICM.hpp"

using namespace zust::zir;

namespace {
    Module parseOrFail(const char *text) {
        std::string error;
        std::optional<Module> m = TextParser::parse(text, error);
        REQUIRE_MESSAGE(m.has_value(), "parse failed: " << error);
        return std::move(*m);
    }
}  // namespace

TEST_CASE("LICM hoists a loop-invariant computation into the preheader") {
    Module m = parseOrFail(R"(module "t.zz" target = "generic"

fn @f(%x: i64, %y: i64) -> i64 {
^entry(%x: i64, %y: i64):
    %c0   = const i64 0
    br ^cond(%c0)

^cond(%i: i64):
    %lim  = const i64 10
    %test = icmp slt i64 %i, %lim
    condbr %test, ^body, ^end

^body:
    %inv  = add i64 %x, %y
    %one  = const i64 1
    %next = add i64 %i, %one
    br ^cond(%next)

^end:
    ret i64 %i
}
)");
    Function &fn = const_cast<Function &>(m.functions()[0]);
    LICMPass pass;
    AnalysisManager am;
    // `^cond` (the header) is as much a part of the loop body as `^body`
    // is, so the loop's own `%lim` constant is an equally legitimate first
    // hoist -- which one happens first is an implementation detail of
    // iterating an unordered_set, and differs between standard library
    // implementations. Run to a full fixpoint and check the property that
    // actually matters: `%inv` ends up out of `^body` either way.
    bool changedAny = false;
    while (pass.run(fn, am)) changedAny = true;
    CHECK(changedAny);
    CHECK(Verifier::verify(m).empty());

    std::string printed = Printer::print(m);
    CAPTURE(printed);
    auto bodyStart = printed.find("^body:");
    auto endStart = printed.find("^end:");
    REQUIRE(bodyStart != std::string::npos);
    REQUIRE(endStart != std::string::npos);
    auto invPos = printed.find("add i64 %x, %y");
    REQUIRE(invPos != std::string::npos);
    bool outsideBody = invPos < bodyStart || invPos > endStart;
    CHECK(outsideBody);
}

TEST_CASE("LICM does not hoist a computation that depends on the loop's own induction variable") {
    Module m = parseOrFail(R"(module "t.zz" target = "generic"

fn @f() -> i64 {
^entry:
    %c0   = const i64 0
    br ^cond(%c0)

^cond(%i: i64):
    %lim  = const i64 10
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
    LICMPass pass;
    AnalysisManager am;
    // The loop's own constant (`%one`) is legitimately loop-invariant (it
    // has no operands at all) and may get hoisted -- what must never
    // happen is `%next`, which depends on the induction variable itself,
    // moving anywhere.
    while (pass.run(fn, am)) {
    }
    std::string printed = Printer::print(m);
    CAPTURE(printed);
    auto bodyPos = printed.find("^body");
    auto nextPos = printed.find("add i64 %i, %one");
    REQUIRE(nextPos != std::string::npos);
    CHECK(nextPos > bodyPos);
}

TEST_CASE("LICM never hoists a call, even one whose operands are all loop-invariant") {
    Module m = parseOrFail(R"(module "t.zz" target = "generic"

declare i64 @pure_looking(i64)

fn @f(%x: i64) -> i64 {
^entry(%x: i64):
    %c0   = const i64 0
    br ^cond(%c0)

^cond(%i: i64):
    %lim  = const i64 10
    %test = icmp slt i64 %i, %lim
    condbr %test, ^body, ^end

^body:
    %r    = call i64 @pure_looking(%x)
    %one  = const i64 1
    %next = add i64 %i, %one
    br ^cond(%next)

^end:
    ret i64 %i
}
)");
    Function &fn = const_cast<Function &>(m.functions()[1]);
    LICMPass pass;
    AnalysisManager am;
    // `%one` (loop-invariant, no operands) may legitimately move; the call
    // itself -- despite `%x` also being loop-invariant -- never should.
    while (pass.run(fn, am)) {
    }
    std::string printed = Printer::print(m);
    CAPTURE(printed);
    auto bodyPos = printed.find("^body");
    auto callPos = printed.find("call i64 @pure_looking");
    REQUIRE(callPos != std::string::npos);
    CHECK(callPos > bodyPos);
}
