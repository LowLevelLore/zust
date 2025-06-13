#pragma once

#include <memory>
#include <ostream>
#include <map>
#include <array>
#include "ast/ASTNode.hpp"
#include "RegisterAllocator.hpp"
#include "typechecker/TypeChecker.hpp"

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
        virtual ~CodeGen();
        virtual void generate(const ASTNode *program, std::ostream &out, bool isFirst = false) = 0;
        static std::unique_ptr<CodeGen> create(TargetTriple target);
    };

    class CodeGenLinux : public CodeGen
    {
    public:
        CodeGenLinux();
        ~CodeGenLinux() override;
        void generate(const ASTNode *program, std::ostream &out, bool isFirst = false) override;

    private:
        /// Emit expression into regs for Linux
        std::string emitExpr(const ASTNode *node, std::ostream &out, RegisterAllocator &alloc);
        void generateStatement(const ASTNode *s, std::ostream &out);
        int labelCounter = 0;
        RegisterAllocator alloc = RegisterAllocator::forSysV();

        static std::string getCorrectMove(uint64_t s, bool f);
        char getIntSuffix(uint64_t bits) const;

        static std::string adjustReg(const std::string &r64, uint64_t bits)
        {
            static const std::unordered_map<std::string, std::array<std::string, 4>> registers_based_on_bytes = {
                {"rax", {"rax", "eax", "ax", "al"}},
                {"rbx", {"rbx", "ebx", "bx", "bl"}},
                {"rcx", {"rcx", "ecx", "cx", "cl"}},
                {"rdx", {"rdx", "edx", "dx", "dl"}},
                {"rsi", {"rsi", "esi", "si", "sil"}},
                {"rdi", {"rdi", "edi", "di", "dil"}},
                {"rbp", {"rbp", "ebp", "bp", "bpl"}},
                {"rsp", {"rsp", "esp", "sp", "spl"}},
                {"r8", {"r8", "r8d", "r8w", "r8b"}},
                {"r9", {"r9", "r9d", "r9w", "r9b"}},
                {"r10", {"r10", "r10d", "r10w", "r10b"}},
                {"r11", {"r11", "r11d", "r11w", "r11b"}},
                {"r12", {"r12", "r12d", "r12w", "r12b"}},
                {"r13", {"r13", "r13d", "r13w", "r13b"}},
                {"r14", {"r14", "r14d", "r14w", "r14b"}},
                {"r15", {"r15", "r15d", "r15w", "r15b"}}};

            auto it = registers_based_on_bytes.find(r64);
            if (it == registers_based_on_bytes.end())
                throw std::runtime_error("Unknown register '" + r64 + "'");
            const auto &ents = it->second;

            switch (bits)
            {
            case 64:
                return ents[0];
            case 32:
                return ents[1];
            case 16:
                return ents[2];
            case 8:
                return ents[3];
            default:
                throw std::runtime_error("Unsupported register size: " + std::to_string(bits));
            }
        }
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
