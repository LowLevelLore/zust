#include "support/CommandLine.hpp"

#include <cstring>
#include <iostream>

namespace zust {

    CommandLine::CommandLine(int argc, char *argv[]) {
        parseArgs(argc, argv);
    }

    bool CommandLine::printAST() const noexcept {
        return printAST_;
    }

    void CommandLine::parseArgs(int argc, char *argv[]) {
        for (int i = 1; i < argc; ++i) {
            const char *arg = argv[i];

            if (std::strcmp(arg, "-h") == 0 || std::strcmp(arg, "--help") == 0) {
                helpFlag = true;
                return;  // no need to parse further
            }
            if (std::strcmp(arg, "-p") == 0 || std::strcmp(arg, "--printAST") == 0) {
                printAST_ = true;
            } else if (std::strcmp(arg, "--formats") == 0) {
                formatsFlag = true;
            } else if (std::strcmp(arg, "--json") == 0) {
                jsonFlag = true;
            } else if (std::strcmp(arg, "-v") == 0 || std::strcmp(arg, "--verbose") == 0) {
                verbosity = 1;
            } else if (std::strcmp(arg, "-o") == 0 || std::strcmp(arg, "--output") == 0) {
                if (++i >= argc) {
                    errorFlag = true;
                    error.message = "Expected filepath after " + std::string(arg);
                    return;
                }
                outputFile = argv[i];
            } else if (std::strcmp(arg, "-f") == 0 || std::strcmp(arg, "--format") == 0) {
                if (++i >= argc) {
                    errorFlag = true;
                    error.message = "Expected format after format command line argument";
                    return;
                }
                if (argv[i][0] == '-') {
                    errorFlag = true;
                    error.message = std::string("Expected format after format argument, got: ") + argv[i];
                    return;
                }
                // Syntactic acceptance only -- this class does not know which
                // names are valid targets. Semantic validation against the
                // BackendRegistry happens in main.cpp, the one place that is
                // allowed to know about targets.
                format = argv[i];
            } else {
                // treat as input file
                if (!inputFile.empty()) {
                    std::cerr << "Warning: Multiple input files detected; using the last one: " << arg << "\n";
                }
                inputFile = arg;
            }
        }
        if (helpFlag || formatsFlag) {
            return;
        }
        if (inputFile.empty()) {
            errorFlag = true;
            error.message = "No input file specified";
        }
    }

    bool CommandLine::hasError() const noexcept {
        return errorFlag;
    }

    const CliError &CommandLine::getError() const noexcept {
        return error;
    }

    bool CommandLine::showHelp() const noexcept {
        return helpFlag;
    }

    bool CommandLine::showFormats() const noexcept {
        return formatsFlag;
    }

    bool CommandLine::wantsJson() const noexcept {
        return jsonFlag;
    }

    std::string CommandLine::getInputFile() const noexcept {
        return inputFile;
    }

    std::string CommandLine::getOutputFile() const noexcept {
        return outputFile;
    }

    std::string CommandLine::getFormat() const noexcept {
        return format;
    }

    int CommandLine::getVerbosity() const noexcept {
        return verbosity;
    }

    void CommandLine::printUsage(const std::string &programName) {
        std::cout << "\nUSAGE: " << programName << " [FLAGS] [OPTIONS] <path to file to compile>\n"
                  << "Flags:\n"
                  << "   `-h`, `--help`    :: Show this help and usage information.\n"
                  << "   `--formats`       :: List acceptable output formats (add `--json` for machine-readable "
                     "output).\n"
                  << "   `-v`, `--verbose` :: Print out more information.\n"
                  << "Options:\n"
                  << "    `-o`, `--output`  :: Set the output filepath.\n"
                  << "    `-f`, `--format`  :: Set the output format.\n"
                  << "Anything else is treated as the input file path.\n";
    }

}  // namespace zust
