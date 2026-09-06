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
        // The raw --emit value ("zir", currently the only one), or "" if
        // --emit was never given. When set, main.cpp prints the ZIR module
        // (via ZirGen + zir::Printer) instead of running a codegen backend --
        // see docs/PRD-ZIR.md Wave 3.1's exit criterion.
        std::string getEmit() const noexcept;
        // docs/PRD-ZIR.md Wave 4.1: routes `--format llvm-ir` through
        // ZirGen + ZirLlvmBackend instead of the legacy AST-consuming
        // LlvmBackend registered in the BackendRegistry. A no-op for every
        // other format. Temporary -- Wave 4.2 ("flip the default") deletes
        // this flag along with the legacy backend it bypasses.
        bool wantsZirCodegen() const noexcept;
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
        bool zirCodegenFlag = false;
        int verbosity = 1;

        std::string inputFile;
        std::string outputFile;
        std::string format;
        std::string emit;
    };

}  // namespace zust
