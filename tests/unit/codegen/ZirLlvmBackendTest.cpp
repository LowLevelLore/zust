#include <memory>
#include <sstream>
#include <string>

#include <doctest/doctest.h>

#include "ast/ASTNode.hpp"
#include "codegen/ZirLlvmBackend.hpp"
#include "lexer/Lexer.hpp"
#include "parser/Parser.hpp"
#include "typechecker/TypeChecker.hpp"
#include "zir/Verifier.hpp"
#include "zirgen/ZirGen.hpp"

using namespace zust;
using namespace zust::zir;

namespace {
    // Drives a small .zz snippet all the way to the emitted .ll text, the
    // same pipeline the registered "llvm-ir" backend now always runs
    // (docs/PRD-ZIR.md Wave 4.2's "flip the default", src/codegen/
    // RegisterBackends.cpp). No `clang`/`llc` dependency here on purpose --
    // these tests check the emitted text is structurally what it should be;
    // whether that text actually assembles and links is what
    // tests/test_pipeline.py's `TARGET=llvm pytest -q` run (Wave 4.1's
    // original exit criterion, now the plain default) checks for real.
    std::string emitLlvm(const std::string &source) {
        Lexer lexer(source);
        Parser parser(lexer);
        std::unique_ptr<ASTNode> program = parser.parse();
        REQUIRE(parser.isCorrect());
        REQUIRE(program != nullptr);

        TypeChecker typeChecker;
        typeChecker.check(program);
        REQUIRE(typeChecker.shouldCodegen());

        ZirGen zirGen;
        Module module = zirGen.lower(*program, "test.zz");
        REQUIRE(Verifier::verify(module).empty());

        std::ostringstream out;
        ZirLlvmBackend::emit(module, out);
        return out.str();
    }
}  // namespace

TEST_CASE("ZirLlvmBackend emits opaque pointers, never a typed i8*") {
    const std::string source =
        "extern fn printf(fmt: string, ...) -> int32_t;\n"
        "fn main() {\n"
        "    printf(\"hi\\n\");\n"
        "}\n";
    std::string ll = emitLlvm(source);
    CAPTURE(ll);
    CHECK(ll.find("ptr") != std::string::npos);
    CHECK(ll.find("i8*") == std::string::npos);
    CHECK(ll.find("declare i32 @printf(ptr, ...)") != std::string::npos);
    CHECK(ll.find("define i32 @main()") != std::string::npos);
    CHECK(ll.find("ret i32 0") != std::string::npos);
}

TEST_CASE("ZirLlvmBackend elides a no-op ptr<->ptr bitcast instead of emitting one") {
    // A string literal's GlobalAddr decays pointer-to-array -> pointer-to-
    // byte through ZirGen's Bitcast, but both are just "ptr" in LLVM -- no
    // `bitcast` instruction should appear in the output at all.
    const std::string source =
        "extern fn printf(fmt: string, ...) -> int32_t;\n"
        "fn main() {\n"
        "    printf(\"hi\\n\");\n"
        "}\n";
    std::string ll = emitLlvm(source);
    CAPTURE(ll);
    CHECK(ll.find("bitcast") == std::string::npos);
    CHECK(ll.find("call i32 (ptr, ...) @printf(ptr @.str") != std::string::npos);
}

TEST_CASE("ZirLlvmBackend inlines constants rather than emitting a separate instruction") {
    const std::string source =
        "let x: int64_t = 41;\n"
        "fn main() {\n"
        "    let y: int64_t = x + 1;\n"
        "}\n";
    std::string ll = emitLlvm(source);
    CAPTURE(ll);
    // `1` is folded directly into the `add` operand text; there is no
    // separate SSA register holding the constant (ZIR's own Const
    // instruction never becomes a real LLVM instruction).
    CHECK(ll.find("add i64") != std::string::npos);
    CHECK(ll.find("= add i64 %") != std::string::npos);
}

TEST_CASE("ZirLlvmBackend encodes float constants as exact double-width hex, never decimal text") {
    const std::string source =
        "let x: double = 3.1415;\n"
        "fn main() {\n"
        "}\n";
    std::string ll = emitLlvm(source);
    CAPTURE(ll);
    CHECK(ll.find("0x") != std::string::npos);
    CHECK(ll.find("3.1415") == std::string::npos);
}

TEST_CASE("ZirLlvmBackend round-trips a string literal's raw bytes through LLVM's c\"...\" escaping") {
    const std::string source =
        "extern fn printf(fmt: string, ...) -> int32_t;\n"
        "fn main() {\n"
        "    printf(\"a\\nb\\\"c\");\n"
        "}\n";
    std::string ll = emitLlvm(source);
    CAPTURE(ll);
    // '\n' (0x0A) escapes to \0A, '"' (0x22) escapes to \22, the trailing
    // NUL terminator to \00, and the plain ASCII bytes pass through as-is.
    CHECK(ll.find("c\"a\\0Ab\\22c\\00\"") != std::string::npos);
}
