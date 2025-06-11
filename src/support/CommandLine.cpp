#include "support/CommandLine.hpp"

#include <iostream>
#include <cstring>

namespace zlang
{

    CommandLine::CommandLine(int argc, char *argv[])
    {
        parseArgs(argc, argv);
    }

    void CommandLine::parseArgs(int argc, char *argv[])
    {
        for (int i = 1; i < argc; ++i)
        {
            const char *arg = argv[i];

            if (std::strcmp(arg, "-h") == 0 || std::strcmp(arg, "--help") == 0)
            {
                helpFlag = true;
                return; // no need to parse further
            }
            else if (std::strcmp(arg, "--formats") == 0)
            {
                formatsFlag = true;
                return;
            }
            else if (std::strcmp(arg, "-v") == 0 || std::strcmp(arg, "--verbose") == 0)
            {
                verbosity = 1;
            }
            else if (std::strcmp(arg, "-o") == 0 || std::strcmp(arg, "--output") == 0)
            {
                if (++i >= argc)
                {
                    errorFlag = true;
                    error.message = "Expected filepath after output command line argument";
                    return;
                }
                if (argv[i][0] == '-')
                {
                    errorFlag = true;
                    error.message = std::string("Expected filepath after output argument, got: ") + argv[i];
                    return;
                }
                outputFile = argv[i];
            }
            else if (std::strcmp(arg, "-f") == 0 || std::strcmp(arg, "--format") == 0)
            {
                if (++i >= argc)
                {
                    errorFlag = true;
                    error.message = "Expected format after format command line argument";
                    return;
                }
                if (argv[i][0] == '-')
                {
                    errorFlag = true;
                    error.message = std::string("Expected format after format argument, got: ") + argv[i];
                    return;
                }
                std::string fmt = argv[i];
                if (fmt == "default")
                {
                    format = CodegenOutputFormat::Default;
                }
                else if (fmt == "x86_64-mswin")
                {
                    format = CodegenOutputFormat::X86_64_MSWIN;
                }
                else if (fmt == "x86_64-linux")
                {
                    format = CodegenOutputFormat::X86_64_LINUX;
                }
                else if (fmt == "llvm-ir")
                {
                    format = CodegenOutputFormat::LLVM_IR;
                }
                else
                {
                    errorFlag = true;
                    error.message = "Unrecognized format: " + fmt;
                    return;
                }
            }
            else
            {
                // treat as input file
                if (!inputFile.empty())
                {
                    std::cerr << "Warning: Multiple input files detected; using the last one: " << arg << "\n";
                }
                inputFile = arg;
            }
        }
    }

    bool CommandLine::hasError() const noexcept
    {
        return errorFlag;
    }

    const CliError &CommandLine::getError() const noexcept
    {
        return error;
    }

    bool CommandLine::showHelp() const noexcept
    {
        return helpFlag;
    }

    bool CommandLine::showFormats() const noexcept
    {
        return formatsFlag;
    }

    std::string CommandLine::getInputFile() const noexcept
    {
        return inputFile;
    }

    std::string CommandLine::getOutputFile() const noexcept
    {
        return outputFile;
    }

    CodegenOutputFormat CommandLine::getFormat() const noexcept
    {
        return format;
    }

    int CommandLine::getVerbosity() const noexcept
    {
        return verbosity;
    }

    void CommandLine::printUsage(const std::string &programName)
    {
        std::cout << "\nUSAGE: " << programName << " [FLAGS] [OPTIONS] <path to file to compile>\n"
                  << "Flags:\n"
                  << "   `-h`, `--help`    :: Show this help and usage information.\n"
                  << "   `--formats`       :: List acceptable output formats.\n"
                  << "   `-v`, `--verbose` :: Print out more information.\n"
                  << "Options:\n"
                  << "    `-o`, `--output`  :: Set the output filepath.\n"
                  << "    `-f`, `--format`  :: Set the output format.\n"
                  << "Anything else is treated as the input file path.\n";
    }

    void CommandLine::printFormats()
    {
        std::cout << "Acceptable formats include:\n"
                  << " -> default\n"
                  << " -> x86_64-mswin\n"
                  << " -> x86_64-linux\n";
    }

} // namespace zlang
