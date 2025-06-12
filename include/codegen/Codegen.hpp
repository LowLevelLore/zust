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
        static TypeInfo promoteType(const TypeInfo &a, const TypeInfo &b)
        {
            if (a.isFloat || b.isFloat)
            {
                if (a.isFloat && b.isFloat)
                    return a.bits > b.bits ? a : b;
                return a.isFloat ? a : b;
            }
            if (a.bits != b.bits)
                return a.bits > b.bits ? a : b;
            return a;
        }

        static char getIntSuffix(uint64_t bits)
        {
            switch (bits)
            {
            case 64:
                return 'q';
            case 32:
                return 'l';
            case 16:
                return 'w';
            case 8:
                return 'b';
            default:
                throw std::runtime_error("Invalid integer size");
            }
        }
        static std::string getCorrectMove(const uint64_t size_bytes);
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
