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

    assert(inputFile.ends_with(".zz"));

    if (inputFile.empty())
    {
        logError(zlang::Error(zlang::ErrorType::Generic, "No input files."));
        CommandLine::printUsage(argv[0]);
        return 1;
    }

    std::optional<std::string> source = zlang::File::readAllText(inputFile);
    if (!source)
    {
        logError(zlang::Error(zlang::ErrorType::Generic,
                              "Failed to read from " + inputFile));
        return 1;
    }

    // Parse source
    Lexer lexer(source.value());
    Parser parser(lexer);

    std::unique_ptr<ASTNode> program = parser.parse();

    if (cli.getVerbosity() == 1)
    {
        if (!program.get())
        {
            zlang::logError(Error(ErrorType::Generic, "Parsing Failed"));
            return 1;
        }
    }
    program.get()->print(std::cout);

    // Type checking
    TypeChecker typeChecker;
    typeChecker.check(program);

    std::unique_ptr<zlang::CodeGen> cg =
        CodeGen::create(TargetTriple::X86_64_LINUX);

    switch (cli.getFormat())
    {
    case CodegenOutputFormat::Default:
#ifdef _WIN64
        cg = CodeGen::create(TargetTriple::X86_64_WINDOWS);
#endif
#ifdef __linux__
        cg = CodeGen::create(TargetTriple::X86_64_LINUX);
#endif
        break;

    case CodegenOutputFormat::X86_64_MSWIN:
    {
        cg = CodeGen::create(TargetTriple::X86_64_WINDOWS);
        break;
    }

    case CodegenOutputFormat::X86_64_LINUX:
    {
        cg = CodeGen::create(TargetTriple::X86_64_LINUX);
        break;
    }

    case CodegenOutputFormat::LLVM_IR:
    {
        cg = CodeGen::create(TargetTriple::LLVM_IR);
        break;
    }

    default:
        std::cerr << "This should not happen, ACP Pradhyumn...\n";
        exit(1);
    }

    if (cli.getOutputFile().empty())
    {
        cg->generate(program.get(), std::cout);
    }
    else
    {
        std::ofstream ofs(cli.getOutputFile());
        if (!ofs)
        {
            std::cerr << "Error: cannot open output file: "
                      << cli.getOutputFile() << "\n";
            exit(1);
        }
        std::cout << "HERE" << std::endl;
        cg->generate(program.get(), ofs);
    }

    return 0;
}
