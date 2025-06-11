#ifndef ZLANG_CODEGEN_HPP
#define ZLANG_CODEGEN_HPP

#include <memory>
#include <ostream>
#include "ast/ASTNode.hpp"
#include "RegisterAllocator.hpp"

namespace zlang
{
    enum class TargetTriple
    {
        X86_64_LINUX,
        X86_64_WINDOWS,
        LLVM_IR
    };

    class CodeGen
    {
    public:
        virtual ~CodeGen() = default;
        virtual void generate(const ASTNode *program, std::ostream &out) = 0;
        static std::unique_ptr<CodeGen> create(TargetTriple target);
    };

    class CodeGenLinux : public CodeGen
    {
    public:
        void generate(const ASTNode *program, std::ostream &out) override;

    private:
        /// Emit expression into regs for Linux
        std::string emitExpr(const ASTNode *node, std::ostream &out, RegisterAllocator &alloc);
    };

    class CodeGenWindows : public CodeGen
    {
    public:
        void generate(const ASTNode *program, std::ostream &out) override;

    private:
        /// Emit expression into regs for Windows
        std::string emitExpr(const ASTNode *node, std::ostream &out, RegisterAllocator &alloc);
    };

    class CodeGenLLVM : public CodeGen
    {
    public:
        void generate(const ASTNode *program, std::ostream &out) override;

    private:
        int stringCounter = 0;
        /// Emit expression for LLVM IR
        void emitExpr(const ASTNode *node, std::ostream &out, int &tempCounter);
    };

    std::string escapeString(const std::string &input);
}

#endif // ZLANG_CODEGEN_HPP