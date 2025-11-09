#include "all.hpp"

namespace zust
{
    CodeGen::~CodeGen() = default;
    std::unique_ptr<CodeGen> CodeGen::create(TargetTriple target, std::ostream &outstream)
    {
        switch (target)
        {
        case TargetTriple::X86_64_LINUX:
            return std::make_unique<CodeGenLinux>(outstream);
        case TargetTriple::X86_64_WINDOWS:
            return std::make_unique<CodeGenWindows>(outstream);
        case TargetTriple::LLVM_IR:
            return std::make_unique<CodeGenLLVM>(outstream);
        }
        throw std::runtime_error("Unknown target");
    }

}
