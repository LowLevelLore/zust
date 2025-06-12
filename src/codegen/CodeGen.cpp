#include "all.hpp"

namespace zlang
{
    std::unique_ptr<CodeGen> CodeGen::create(TargetTriple target)
    {
        switch (target)
        {
        case TargetTriple::X86_64_LINUX:
            return std::make_unique<CodeGenLinux>();
        case TargetTriple::X86_64_WINDOWS:
            return std::make_unique<CodeGenWindows>();
        case TargetTriple::LLVM_IR:
            return std::make_unique<CodeGenLLVM>();
        }
        throw std::runtime_error("Unknown target");
    }

}
