#include <sstream>

#include <doctest/doctest.h>

#include "ast/ASTNode.hpp"
#include "codegen/machine/AsmWriter.hpp"
#include "codegen/machine/FrameLayout.hpp"
#include "codegen/machine/LinearScan.hpp"
#include "codegen/machine/NativeBackend.hpp"
#include "codegen/machine/Win64Abi.hpp"
#include "codegen/machine/X86InstSel.hpp"
#include "lexer/Lexer.hpp"
#include "parser/Parser.hpp"
#include "typechecker/TypeChecker.hpp"
#include "zir/Verifier.hpp"
#include "zirgen/ZirGen.hpp"

using namespace zust;
using namespace zust::zir;
using namespace zust::codegen::machine;

namespace {
    Module lowerSource(const std::string &source) {
        Lexer lexer(source);
        Parser parser(lexer);
        std::unique_ptr<ASTNode> program = parser.parse();
        REQUIRE(parser.isCorrect());
        REQUIRE(program != nullptr);

        TypeChecker typeChecker;
        typeChecker.check(program);
        REQUIRE(typeChecker.shouldCodegen());

        ZirGen zirGen;
        Module m = zirGen.lower(*program, "test.zz");
        REQUIRE(Verifier::verify(m).empty());
        return m;
    }
}  // namespace

TEST_CASE("NativeBackend emits Intel MASM text for a trivial return-x-plus-1 function") {
    // docs/PRD-ZIR.md Wave 5's own exit criterion function.
    Module m = lowerSource(
        "fn f(x: int64_t) -> int64_t {\n"
        "    return x + 1;\n"
        "}\n"
        "fn main() {\n"
        "}\n");

    std::ostringstream out;
    emitNative(m, win64Abi(), /*intelSyntax=*/true, out);
    std::string text = out.str();
    CAPTURE(text);

    CHECK(text.find("zzfn_f PROC") != std::string::npos);
    CHECK(text.find("zzfn_f ENDP") != std::string::npos);
    CHECK(text.find("add") != std::string::npos);
    CHECK(text.find("ret") != std::string::npos);
    CHECK(text.find("main PROC") != std::string::npos);  // main is never mangled
    CHECK(text.find("END") != std::string::npos);
}

TEST_CASE("NativeBackend emits AT&T text for the same trivial function") {
    Module m = lowerSource(
        "fn f(x: int64_t) -> int64_t {\n"
        "    return x + 1;\n"
        "}\n"
        "fn main() {\n"
        "}\n");

    std::ostringstream out;
    emitNative(m, win64Abi(), /*intelSyntax=*/false, out);
    std::string text = out.str();
    CAPTURE(text);

    CHECK(text.find("zzfn_f:") != std::string::npos);
    CHECK(text.find('%') != std::string::npos);  // register prefix
    CHECK(text.find('$') != std::string::npos);  // immediate prefix (the `+ 1`)
    CHECK(text.find(".text") != std::string::npos);
}

TEST_CASE("NativeBackend mangles a user function whose name collides with an x86 mnemonic") {
    // A regression pin for a real bug: `fn add(...)` produced `add PROC` /
    // `add ENDP`, which MASM garbled against the `add` instruction mnemonic
    // in the very same file.
    Module m = lowerSource(
        "fn add(a: int64_t, b: int64_t) -> int64_t {\n"
        "    return a + b;\n"
        "}\n"
        "fn main() {\n"
        "}\n");

    std::ostringstream out;
    emitNative(m, win64Abi(), /*intelSyntax=*/true, out);
    std::string text = out.str();
    CAPTURE(text);

    // "zzfn_add PROC" itself contains "add PROC" as a substring, so check
    // for the *un*-mangled label on its own line instead of a bare
    // substring search.
    CHECK(text.find("\nadd PROC") == std::string::npos);
    CHECK(text.find("\nadd ENDP") == std::string::npos);
    CHECK(text.find("zzfn_add PROC") != std::string::npos);
    CHECK(text.find("zzfn_add ENDP") != std::string::npos);
}

TEST_CASE("NativeBackend sanitizes a global name and a block label containing a dot") {
    // ZirGen names every string literal ".strN" and every loop block
    // "for.condN"/"while.bodyN" -- both are syntax errors to MASM
    // (a leading '.' is a directive prefix; embedded, it still isn't a
    // legal identifier character) unless sanitized.
    Module m = lowerSource(
        "extern fn printf(fmt: string, ...) -> int32_t;\n"
        "fn main() {\n"
        "    let i: int64_t = 0;\n"
        "    while (i < 3) {\n"
        "        printf(\"%d\\n\", i);\n"
        "        i = i + 1;\n"
        "    }\n"
        "}\n");

    std::ostringstream out;
    emitNative(m, win64Abi(), /*intelSyntax=*/true, out);
    std::string text = out.str();
    CAPTURE(text);

    CHECK(text.find(".str") == std::string::npos);
    CHECK(text.find("while.") == std::string::npos);
    CHECK(text.find("_str0") != std::string::npos);
}

TEST_CASE("LinearScan spills when a block has more live values than the allocatable pool") {
    // Win64Abi's allocatable GPR pool has 7 registers -- 9 simultaneously
    // live values in one straight-line block forces at least one spill,
    // exercising LinearScan's spill path (not just the never-spills common
    // case every golden-suite program happens to hit).
    // Nine arguments to one call all have to be live *simultaneously* right
    // up to the call itself -- unlike a left-to-right sum, where each
    // loaded operand is immediately consumed by the running accumulator
    // and never more than two values are live at once.
    std::ostringstream src;
    src << "extern fn sink(";
    for (int i = 0; i < 9; ++i) src << (i ? ", " : "") << "a" << i << ": int64_t";
    src << ") -> none;\n";
    src << "fn sumMany() -> int64_t {\n";
    for (int i = 0; i < 9; ++i) src << "    let v" << i << ": int64_t = " << i << ";\n";
    src << "    sink(";
    for (int i = 0; i < 9; ++i) src << (i ? ", v" : "v") << i;
    src << ");\n";
    src << "    return v0;\n";
    src << "}\n";
    src << "fn main() {\n"
           "    let r: int64_t = sumMany();\n"
           "}\n";

    Module m = lowerSource(src.str());
    X86InstSel sel(m, win64Abi());
    Function &fn = m.function(FuncId(1));  // sink(extern)=0, sumMany=1, main=2
    MachineFunction mf = sel.select(fn);
    LinearScan(win64Abi()).run(mf);
    FrameLayout::compute(mf, win64Abi());

    bool sawSpillSlot = false;
    for (const FrameSlot &slot : mf.frameSlots)
        if (slot.isSpill)
            sawSpillSlot = true;
    CHECK(sawSpillSlot);

    std::ostringstream out;
    AsmWriterIntel::emit(m, {}, {mf}, sel.floatConstants(), win64Abi(), out);
    CHECK(out.str().find("zzfn_sumMany PROC") != std::string::npos);
}
