#pragma once
#include <string>
#include <vector>

namespace zust {

    struct CliError {
        bool isError = false;
        std::string message;
    };

    class CommandLine {
    public:
        CommandLine(int argc, char *argv[]);

        bool hasError() const noexcept;
        const CliError &getError() const noexcept;

        bool showHelp() const noexcept;
        bool showFormats() const noexcept;
        bool wantsJson() const noexcept;

        std::string getInputFile() const noexcept;
        std::string getOutputFile() const noexcept;
        // The raw --format value ("x86_64-linux", "x86_64-mswin", "llvm-ir", …),
        // "default" if the user asked for that explicitly, or "" if -f/--format
        // was never given. This class does not know which names are valid --
        // that is the BackendRegistry's job (see include/codegen/Backend.hpp);
        // resolving "" / "default" to a concrete target and validating the
        // result belongs to the caller.
        std::string getFormat() const noexcept;
        int getVerbosity() const noexcept;
        bool printAST() const noexcept;
        static void printUsage(const std::string &programName);

    private:
        void parseArgs(int argc, char *argv[]);

        bool errorFlag = false;
        CliError error;

        bool helpFlag = false;
        bool printAST_ = false;
        bool formatsFlag = false;
        bool jsonFlag = false;
        int verbosity = 1;

        std::string inputFile;
        std::string outputFile;
        std::string format;
    };

}  // namespace zust
