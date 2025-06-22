#pragma once
#include <string>
#include <vector>

namespace zust
{

    enum class CodegenOutputFormat
    {
        Default,
        X86_64_MSWIN,
        X86_64_LINUX,
        LLVM_IR
    };

    struct CliError
    {
        bool isError = false;
        std::string message;
    };

    class CommandLine
    {
    public:
        CommandLine(int argc, char *argv[]);

        bool hasError() const noexcept;
        const CliError &getError() const noexcept;

        bool showHelp() const noexcept;
        bool showFormats() const noexcept;

        std::string getInputFile() const noexcept;
        std::string getOutputFile() const noexcept;
        CodegenOutputFormat getFormat() const noexcept;
        int getVerbosity() const noexcept;
        bool printAST() const noexcept;
        static void printUsage(const std::string &programName);
        static void printFormats();

    private:
        void parseArgs(int argc, char *argv[]);

        bool errorFlag = false;
        CliError error;

        bool helpFlag = false;
        bool printAST_ = false;
        bool formatsFlag = false;
        int verbosity = 1;

        std::string inputFile;
        std::string outputFile;
        CodegenOutputFormat format = CodegenOutputFormat::Default;
    };

} // namespace zust
