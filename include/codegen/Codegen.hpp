#pragma once

#include <memory>
#include <ostream>
#include <map>
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
    protected:
        std::map<std::string, std::string> assembly_operations = {
            {">=", "setge"}, // Set if greater or equal
            {"<=", "setle"}, // Set if less or equal
            {">", "setg"},   // Set if greater
            {"<", "setl"},   // Set if less
            {"==", "sete"},  // Set if equal
            {"!=", "setne"}  // Set if not equal
        };

    public:
        virtual ~CodeGen() = default;
        virtual void generate(const ASTNode *program, std::ostream &out, bool isFirst = false) = 0;
        static std::unique_ptr<CodeGen> create(TargetTriple target);
    };

    class CodeGenLinux : public CodeGen
    {
    public:
        void generate(const ASTNode *program, std::ostream &out, bool isFirst = false) override;

    private:
        /// Emit expression into regs for Linux
        std::string emitExpr(const ASTNode *node, std::ostream &out, RegisterAllocator &alloc);
        void generateStatement(const ASTNode *s, std::ostream &out);
        int labelCounter = 0;
        RegisterAllocator alloc = RegisterAllocator::forSysV();
    };

    class CodeGenWindows : public CodeGen
    {
    public:
        void generate(const ASTNode *program, std::ostream &out, bool isFirst = false) override;

    private:
        /// Emit expression into regs for Windows
        std::string emitExpr(const ASTNode *node, std::ostream &out, RegisterAllocator &alloc);
        void generateStatement(const ASTNode *s, std::ostream &out);
        int labelCounter = 0;
        RegisterAllocator alloc = RegisterAllocator::forMSVC();
    };

    class CodeGenLLVM : public CodeGen
    {
    public:
        void generate(const ASTNode *program, std::ostream &out, bool isFirst = false) override;

    private:
        int labelCounter = 0;
        int stringCounter = 0;
        std::unordered_map<std::string /*literal*/, std::string /*label*/> strings_;
        void emitExpr(const ASTNode *node, std::ostream &out, int &tempCounter);
        void generateStatement(const ASTNode *s, std::ostream &out, int &tempCounter);
    };

    std::string escapeString(const std::string &input);
}
