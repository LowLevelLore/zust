#include <cstdlib>
#include <fstream>
#include <iostream>
#include <memory>
#include <optional>
#include <string>

#include "ast/ASTNode.hpp"
#include "codegen/Backend.hpp"
#include "common/Errors.hpp"
#include "common/Logging.hpp"
#include "lexer/Lexer.hpp"
#include "parser/Parser.hpp"
#include "parser/ScopeContext.hpp"
#include "support/CommandLine.hpp"
#include "support/File.hpp"
#include "typechecker/TypeChecker.hpp"
#include "zir/PassManager.hpp"
#include "zir/Printer.hpp"
#include "zir/Verifier.hpp"
#include "zir/passes/Pipeline.hpp"
#include "zirgen/ZirGen.hpp"

using namespace zust;

namespace {
    // Used by `--emit=zir` (docs/PRD-ZIR.md Wave 3) to get a verified ZIR
    // module before printing it. Returns nullopt (having already reported
    // the problem) on a lowering exception or a verifier failure.
    std::optional<zir::Module> lowerAndVerify(const ASTNode &program, const std::string &sourceName) {
        try {
            ZirGen zirGen;
            zir::Module mod = zirGen.lower(program, sourceName);
            std::vector<zir::VerifierFailure> failures = zir::Verifier::verify(mod);
            if (!failures.empty()) {
                for (const zir::VerifierFailure &f : failures) {
                    std::cerr << "ZIR verification failed [" << zir::toString(f.check) << "] in @" << f.function << ": "
                              << f.detail << "\n";
                }
                return std::nullopt;
            }
            return mod;
        } catch (std::exception const &exc) {
            std::cerr << "ERROR: " << exc.what() << "\n";
            return std::nullopt;
        }
    }
}  // namespace

int main(int argc, char *argv[]) {
    if (argc < 2) {
        CommandLine::printUsage(argv[0]);
        return 0;
    }

    CommandLine cli(argc, argv);

    if (cli.hasError()) {
        return 1;
    }

    if (cli.showHelp()) {
        CommandLine::printUsage(argv[0]);
        return 0;
    }

    BackendRegistry &registry = BackendRegistry::instance();
    registerBuiltinBackends(registry);

    if (cli.showFormats()) {
        if (cli.wantsJson()) {
            registry.printFormatsJson(std::cout);
        } else {
            registry.printFormats(std::cout);
        }
        return 0;
    }

    // Resolve and validate the target before doing any compilation work or
    // touching the output file: an unknown --format should be rejected
    // immediately, not after parsing/type-checking the input and potentially
    // truncating/creating the output file.
    std::string targetName = cli.getFormat();
    if (targetName.empty() || targetName == "default") {
        targetName = BackendRegistry::hostDefaultName();
    }

    std::unique_ptr<Backend> backend = registry.create(targetName);
    if (!backend) {
        logError(Error(ErrorType::Generic, "Unrecognized format: " + targetName));
        std::cerr << "Run with --formats to see the available targets.\n";
        return 1;
    }

    const std::string inputFile = cli.getInputFile();

    if (!inputFile.ends_with(".zz")) {
        logError(zust::Error(zust::ErrorType::Generic, "Input file must have .zz extension."));
        CommandLine::printUsage(argv[0]);
        return 1;
    }

    if (inputFile.empty()) {
        logError(zust::Error(zust::ErrorType::Generic, "No input files."));
        CommandLine::printUsage(argv[0]);
        return 1;
    }

    std::optional<std::string> source = zust::File::readAllText(inputFile);
    if (!source) {
        logError(zust::Error(zust::ErrorType::Generic, "Failed to read from " + inputFile));
        return 1;
    }

    // Parse source
    Lexer lexer(source.value());
    Parser parser(lexer);
    logMessage("Parsing");
    std::unique_ptr<ASTNode> program = parser.parse();

    if (!parser.isCorrect()) {
        return 1;
    }

    if (!program.get()) {
        zust::logError(Error(ErrorType::Generic, "Parsing Failed"));
        return 1;
    }
    if (cli.printAST()) {
        program.get()->print(std::cout);
    }

    logMessage("TypeChecking");

    // Type checking
    TypeChecker typeChecker;
    typeChecker.check(program);

    logMessage("Code Genning");

    if (!typeChecker.shouldCodegen())
        return 1;

    try {
        program->scope->lookupFunction("main");
    } catch (...) {
        logError(Error(ErrorType::Generic, "Main Function does'nt exist in program scope (GLOBALLY)"));
        exit(1);
    }
    std::ostream *outstream = &std::cout;
    std::ofstream ofs;

    // Only open the file if requested:
    if (!cli.getOutputFile().empty()) {
        ofs.open(cli.getOutputFile());
        if (!ofs) {
            std::cerr << "Error: cannot open output file: " << cli.getOutputFile() << "\n";
            std::exit(1);
        }
        outstream = &ofs;  // now point at the file
    }

    if (cli.getEmit() == "zir") {
        std::optional<zir::Module> mod = lowerAndVerify(*program, inputFile);
        if (!mod)
            return 1;
        zir::AnalysisManager am;
        zir::PassManager pm = zir::buildPipeline(cli.getOptLevel(), *mod);
        pm.run(*mod, am);
        std::vector<zir::VerifierFailure> failures = zir::Verifier::verify(*mod);
        if (!failures.empty()) {
            for (const zir::VerifierFailure &f : failures) {
                std::cerr << "ZIR verification failed after optimization [" << zir::toString(f.check) << "] in @"
                          << f.function << ": " << f.detail << "\n";
            }
            return 1;
        }
        zir::Printer::print(*mod, *outstream);
        return 0;
    }

    try {
        backend->emit(std::move(program), *outstream, cli.getOptLevel());
    } catch (std::exception const &exc) {
        std::cerr << "ERROR: " << exc.what() << "\n";
        return 1;
    }

    return 0;
}
