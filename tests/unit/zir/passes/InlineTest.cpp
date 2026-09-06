#include <doctest/doctest.h>

#include "zir/PassManager.hpp"
#include "zir/Printer.hpp"
#include "zir/TextParser.hpp"
#include "zir/Verifier.hpp"
#include "zir/passes/Inline.hpp"

using namespace zust::zir;

namespace {
    Module parseOrFail(const char *text) {
        std::string error;
        std::optional<Module> m = TextParser::parse(text, error);
        REQUIRE_MESSAGE(m.has_value(), "parse failed: " << error);
        return std::move(*m);
    }
}  // namespace

TEST_CASE("Inline splices a single-block callee's body into the call site") {
    Module m = parseOrFail(R"(module "t.zz" target = "generic"

fn @add_one(%x: i64) -> i64 {
^entry(%x: i64):
    %one  = const i64 1
    %sum  = add i64 %x, %one
    ret i64 %sum
}

fn @main() -> i64 {
^entry:
    %five = const i64 5
    %r    = call i64 @add_one(%five)
    ret i64 %r
}
)");
    Function &main = const_cast<Function &>(m.functions()[1]);
    InlinePass pass;
    AnalysisManager am;
    CHECK(pass.run(m, am));
    CHECK(Verifier::verify(m).empty());

    std::string printed = Printer::print(m);
    CAPTURE(printed);
    // The call is gone from @main; the addition now happens directly there.
    auto mainStart = printed.find("fn @main");
    REQUIRE(mainStart != std::string::npos);
    CHECK(printed.find("call", mainStart) == std::string::npos);
    CHECK(printed.find("add i64", mainStart) != std::string::npos);
    (void)main;
}

TEST_CASE("Inline never touches an extern declaration or a variadic function") {
    Module m = parseOrFail(R"(module "t.zz" target = "generic"

@.str = private constant [1 x i8] c"\00"

declare i32 @printf(ptr, ...)

fn @main() -> void {
^entry:
    %fmt  = globaladdr @.str
    call void @printf(%fmt)
    ret void
}
)");
    Function &fn = const_cast<Function &>(m.functions()[1]);
    InlinePass pass;
    AnalysisManager am;
    CHECK_FALSE(pass.run(m, am));
    (void)fn;
}

TEST_CASE("Inline declines a multi-block callee (it has control flow of its own)") {
    Module m = parseOrFail(R"(module "t.zz" target = "generic"

fn @abs(%x: i64) -> i64 {
^entry(%x: i64):
    %z    = const i64 0
    %neg  = icmp slt i64 %x, %z
    condbr %neg, ^flip, ^same

^flip:
    %f    = sub i64 %z, %x
    ret i64 %f

^same:
    ret i64 %x
}

fn @main() -> i64 {
^entry:
    %v    = const i64 5
    %r    = call i64 @abs(%v)
    ret i64 %r
}
)");
    InlinePass pass;
    AnalysisManager am;
    CHECK_FALSE(pass.run(m, am));
}

TEST_CASE("Inline substitutes the call's result everywhere it was used, not just at the call site") {
    Module m = parseOrFail(R"(module "t.zz" target = "generic"

fn @double(%x: i64) -> i64 {
^entry(%x: i64):
    %d    = add i64 %x, %x
    ret i64 %d
}

fn @main() -> i64 {
^entry:
    %v    = const i64 3
    %r    = call i64 @double(%v)
    %plus = add i64 %r, %v
    ret i64 %plus
}
)");
    Function &main = const_cast<Function &>(m.functions()[1]);
    InlinePass pass;
    AnalysisManager am;
    CHECK(pass.run(m, am));
    CHECK(Verifier::verify(m).empty());
    (void)main;
}
