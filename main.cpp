#include "all.hpp"

using namespace zlang;

int main(int argc, char *argv[])
{
    if (argc < 2)
    {
        CommandLine::printUsage(argv[0]);
        return 0;
    }

    CommandLine cli(argc, argv);
    if (cli.hasError())
    {
        return 1;
    }

    if (cli.showHelp())
    {
        CommandLine::printUsage(argv[0]);
        return 0;
    }

    if (cli.showFormats())
    {
        CommandLine::printFormats();
        return 0;
    }

    const std::string inputFile = cli.getInputFile();
    if (inputFile.empty())
    {
        logError(zlang::Error(zlang::ErrorType::Generic, "No input files."));
        CommandLine::printUsage(argv[0]);
        return 1;
    }

    std::optional<std::string> source = zlang::File::readAllText(inputFile);
    if (!source)
    {
        logError(zlang::Error(zlang::ErrorType::Generic, "Failed to read from " + inputFile));
        return 1;
    }

    std::cout << source.value() << std::endl;

    // Parse source
    Lexer lexer(source.value());
    Parser parser(lexer);

    std::unique_ptr<ASTNode> program = std::make_unique<ASTNode>();
    program.get()->type = NodeType::Program;
    parser.parseProgram(program);

    if (cli.getVerbosity() == 1)
    {
        if (!program.get())
        {
            zlang::logError(Error(ErrorType::Generic, "Parsing Failed"));
            return 1;
        }
    }
    program.get()->print(std::cout);

    // // Type checking
    // TypeChecker typeChecker;
    // Error typeErr = typeChecker.check(*program);
    // if (typeErr)
    // {
    //     logError(typeErr);
    //     return 2;
    // }

    // // Code generation
    // CodeGenerator codegen;
    // const std::string outputFile = cli.getOutputFile().empty() ? "code.S" : cli.getOutputFile();
    // Error codegenErr = codegen.generate(*program, cli.getFormat(), outputFile);
    // if (codegenErr)
    // {
    //     logError(codegenErr);
    //     return 3;
    // }

    // std::cout << "\nGenerated code at output filepath: \"" << outputFile << "\"\n";
    return 0;
}
