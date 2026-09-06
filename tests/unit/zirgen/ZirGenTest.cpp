#include <filesystem>
#include <fstream>
#include <optional>
#include <sstream>
#include <string>
#include <vector>

#include <doctest/doctest.h>

#include "ast/ASTNode.hpp"
#include "lexer/Lexer.hpp"
#include "parser/Parser.hpp"
#include "typechecker/TypeChecker.hpp"
#include "zir/Printer.hpp"
#include "zir/TextParser.hpp"
#include "zir/Verifier.hpp"
#include "zirgen/ZirGen.hpp"

using namespace zust;
using namespace zust::zir;

namespace {
    std::string readFile(const std::filesystem::path &p) {
        std::ifstream f(p, std::ios::binary);
        REQUIRE_MESSAGE(f.good(), "could not open fixture: " << p);
        std::ostringstream ss;
        ss << f.rdbuf();
        return ss.str();
    }

    // Every real golden-suite input under `dir`, recursively -- both
    // tests/runtime (must succeed end to end) and tests/runtime_fail (still
    // compiles cleanly; it's the *program's own* exit code/output that's
    // wrong, not its compilation) type-check without error, so both are fair
    // game for ZirGen. tests/compile_fail is deliberately excluded: those
    // sources fail type-checking on purpose and never reach this stage.
    std::vector<std::filesystem::path> collectZzFiles(const std::filesystem::path &dir) {
        std::vector<std::filesystem::path> out;
        for (const auto &entry : std::filesystem::recursive_directory_iterator(dir)) {
            if (entry.path().extension() == ".zz")
                out.push_back(entry.path());
        }
        return out;
    }
}  // namespace

// docs/PRD-ZIR.md Wave 3.1's exit criterion: "all 40 cases lower and verify
// clean at -O0; --emit=zir round-trips." This drives every real .zz file in
// tests/runtime and tests/runtime_fail through the full
// Lexer->Parser->TypeChecker->ZirGen pipeline and checks both halves of that
// criterion for each one, rather than trusting design review alone.
TEST_CASE("ZirGen lowers every real .zz test case and verifies clean, round-tripping through the text form") {
    std::vector<std::filesystem::path> files;
    for (const char *sub : {"runtime", "runtime_fail"}) {
        std::filesystem::path dir = std::filesystem::path(ZUST_TESTS_DIR) / sub;
        REQUIRE_MESSAGE(std::filesystem::exists(dir), "tests directory missing: " << dir);
        auto here = collectZzFiles(dir);
        files.insert(files.end(), here.begin(), here.end());
    }
    REQUIRE(files.size() > 0);

    int checked = 0;
    for (const auto &path : files) {
        CAPTURE(path.string());
        std::string source = readFile(path);

        Lexer lexer(source);
        Parser parser(lexer);
        std::unique_ptr<ASTNode> program = parser.parse();
        REQUIRE_MESSAGE(parser.isCorrect(), "parse failed: " << path.string());
        REQUIRE(program != nullptr);

        TypeChecker typeChecker;
        typeChecker.check(program);
        REQUIRE_MESSAGE(typeChecker.shouldCodegen(), "type-check failed: " << path.string());

        ZirGen zirGen;
        Module module = zirGen.lower(*program, path.string());

        std::vector<VerifierFailure> failures = Verifier::verify(module);
        for (const VerifierFailure &f : failures) {
            MESSAGE("verifier failure [", toString(f.check), "] in @", f.function, ": ", f.detail);
        }
        CHECK(failures.empty());

        std::string printed = Printer::print(module);
        std::string error;
        std::optional<Module> reparsed = TextParser::parse(printed, error);
        REQUIRE_MESSAGE(reparsed.has_value(), "round-trip parse failed for " << path.string() << ": " << error);
        std::string reprinted = Printer::print(*reparsed);
        CHECK(reprinted == printed);

        checked++;
    }
    CHECK(checked == static_cast<int>(files.size()));
}

// docs/PRD-ZIR.md behavior inventory, "`&&` / `||` are non-short-circuiting":
// all three legacy backends lower them to a plain and/or that evaluates both
// operands unconditionally, and no existing golden has a side-effecting RHS
// to catch a regression to short-circuit `condbr` lowering -- this pins it
// directly by asserting both calls appear as plain instructions with no
// intervening branch, rather than relying on process exit code alone. Not a
// `tests/zir/` fixture (that directory holds the *text-form* round-trip
// fixtures, upstream of anything ZirGen produces) -- this is ZirGen's own
// input, so it lives alongside it here instead.
TEST_CASE("ZirGen lowers && and || as plain and/or, evaluating both operands unconditionally") {
    const std::string source =
        "extern fn printf(fmt: string, ...) -> int32_t;\n"
        "fn effect(x: boolean) -> boolean {\n"
        "    printf(\"%d\\n\", x);\n"
        "    return x;\n"
        "}\n"
        "fn main() {\n"
        "    let r: boolean = effect(false) && effect(true);\n"
        "}\n";

    Lexer lexer(source);
    Parser parser(lexer);
    std::unique_ptr<ASTNode> program = parser.parse();
    REQUIRE(parser.isCorrect());
    REQUIRE(program != nullptr);

    TypeChecker typeChecker;
    typeChecker.check(program);
    REQUIRE(typeChecker.shouldCodegen());

    ZirGen zirGen;
    Module module = zirGen.lower(*program, "effectful_and.zz");
    std::vector<VerifierFailure> failures = Verifier::verify(module);
    for (const VerifierFailure &f : failures) {
        MESSAGE("verifier failure [", toString(f.check), "] in @", f.function, ": ", f.detail);
    }
    CHECK(failures.empty());

    std::string printed = Printer::print(module);
    CAPTURE(printed);
    // Both calls to `effect` must appear -- neither operand is skipped --
    // and there must be no conditional branch inside @main, since a
    // short-circuiting lowering would need one to skip the RHS call.
    std::size_t firstCall = printed.find("call u8 @effect");
    REQUIRE(firstCall != std::string::npos);
    std::size_t secondCall = printed.find("call u8 @effect", firstCall + 1);
    REQUIRE(secondCall != std::string::npos);
    CHECK(printed.find(" and ") != std::string::npos);
    std::size_t mainStart = printed.find("fn @main");
    REQUIRE(mainStart != std::string::npos);
    CHECK(printed.find("condbr", mainStart) == std::string::npos);
}

// docs/PRD-ZIR.md behavior inventory, "Missing return": a non-`none`
// function whose body falls off the end without an explicit `return` on
// every path gets `unreachable`, matching today's undefined-behavior
// fallthrough on the legacy backends -- deliberately not a synthesized
// `ret 0`, which would be a real (if minor) language-level behavior change.
TEST_CASE("ZirGen lowers a missing return as unreachable, not a synthesized ret") {
    const std::string source =
        "fn maybe(x: boolean) -> int64_t {\n"
        "    if (x) {\n"
        "        return 1;\n"
        "    }\n"
        "}\n"
        "fn main() {\n"
        "    maybe(true);\n"
        "}\n";

    Lexer lexer(source);
    Parser parser(lexer);
    std::unique_ptr<ASTNode> program = parser.parse();
    REQUIRE(parser.isCorrect());
    REQUIRE(program != nullptr);

    TypeChecker typeChecker;
    typeChecker.check(program);
    REQUIRE(typeChecker.shouldCodegen());  // definite-return analysis is warning-only (Wave 2.4)

    ZirGen zirGen;
    Module module = zirGen.lower(*program, "missing_return.zz");
    std::vector<VerifierFailure> failures = Verifier::verify(module);
    for (const VerifierFailure &f : failures) {
        MESSAGE("verifier failure [", toString(f.check), "] in @", f.function, ": ", f.detail);
    }
    CHECK(failures.empty());

    std::string printed = Printer::print(module);
    CAPTURE(printed);
    CHECK(printed.find("unreachable") != std::string::npos);
}
