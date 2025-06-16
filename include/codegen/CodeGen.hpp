#pragma once

#include <memory>
#include <ostream>
#include <map>
#include <array>
#include "ast/ASTNode.hpp"
#include "codegen/RegisterAllocator.hpp"
#include "typechecker/TypeChecker.hpp"
#include <sstream>

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
        std::map<std::string, std::string> assembly_comparison_operations = {
            {">=", "setge"},
            {"<=", "setle"},
            {">", "setg"},
            {"<", "setl"},
            {"==", "sete"},
            {"!=", "setne"}};

        std::unordered_map<std::string, TypeInfo> regType;
        std::uint64_t stringLabelCount = 0;
        std::uint64_t blockLabelCount = 0;
        std::uint64_t doubleLabelCount = 0;
        std::uint64_t floatLabelCount = 0;
        RegisterAllocator alloc;
        std::ostream &outfinal;

        std::ostringstream outGlobal;
        std::ostringstream out;

        std::string adjustReg(std::string &r64, uint64_t bits)
        {
            r64 = RegisterAllocator::getBaseReg(r64);
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
                throw std::runtime_error("Unknown register '" + r64 + "'\n\n" + out.str());
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

        static std::string getCorrectMove(uint64_t size_bytes, bool isfloat)
        {
            if (isfloat)
            {
                if (size_bytes == 4)
                    return "movss";
                else if (size_bytes == 8)
                    return "movsd";
                else
                    throw std::runtime_error("Bad float move size");
            }
            else
            {
                switch (size_bytes)
                {
                case 8:
                    return "movq";
                case 4:
                    return "movl";
                case 2:
                    return "movw";
                case 1:
                    return "movb";
                }
                throw std::runtime_error("Bad integer move size");
            }
        }

        void noteType(const std::string &register_, const TypeInfo &type_) { regType[register_] = type_; }

        virtual std::string intToXmm(const std::string &r_int, uint32_t bits) = 0;

        virtual void generateStatement(std::unique_ptr<ASTNode> statement) = 0;

        virtual std::string emitExpression(std::unique_ptr<ASTNode> node) = 0;

        virtual void emitEpilogue() = 0;
        virtual void emitPrologue(std::unique_ptr<ASTNode> blockNode) = 0;

        virtual std::string generateIntegerLiteral(std::unique_ptr<ASTNode> node) = 0;
        virtual std::string generateFloatLiteral(std::unique_ptr<ASTNode> node) = 0;
        virtual std::string generateStringLiteral(std::unique_ptr<ASTNode> node) = 0;
        virtual std::string generateBooleanLiteral(std::unique_ptr<ASTNode> node) = 0;
        virtual std::string generateVariableAccess(std::unique_ptr<ASTNode> node) = 0;
        virtual void generateVariableReassignment(std::unique_ptr<ASTNode> node) = 0;
        virtual void generateVariableDeclaration(std::unique_ptr<ASTNode> node) = 0;
        virtual void generateIfStatement(std::unique_ptr<ASTNode> node) = 0;
        virtual std::string generateBinaryOperation(std::unique_ptr<ASTNode> node) = 0;
        virtual std::string generateUnaryOperation(std::unique_ptr<ASTNode> node) = 0;

    public:
        CodeGen(RegisterAllocator alloc, std::ostream &outstream) : stringLabelCount(0), blockLabelCount(0), doubleLabelCount(0), floatLabelCount(0), alloc(alloc), outfinal(outstream) {}
        virtual ~CodeGen();
        virtual void generate(std::unique_ptr<ASTNode> program) = 0;
        static std::unique_ptr<CodeGen> create(TargetTriple target, std::ostream &outstream);
    };

    class CodeGenLinux : public CodeGen
    {
    private:
        std::unordered_map<std::uint32_t, char> integer_suffixes = {{8, 'b'}, {16, 'w'}, {32, 'l'}, {64, 'q'}};
        std::string intToXmm(const std::string &r_int, uint32_t bits) override;

        void generateStatement(std::unique_ptr<ASTNode> statement) override;

        std::string emitExpression(std::unique_ptr<ASTNode> node) override;

        void emitEpilogue() override;
        void emitPrologue(std::unique_ptr<ASTNode> blockNode) override;

        std::string generateIntegerLiteral(std::unique_ptr<ASTNode> node) override;
        std::string generateFloatLiteral(std::unique_ptr<ASTNode> node) override;
        std::string generateStringLiteral(std::unique_ptr<ASTNode> node) override;
        std::string generateBooleanLiteral(std::unique_ptr<ASTNode> node) override;
        std::string generateVariableAccess(std::unique_ptr<ASTNode> node) override;
        void generateVariableReassignment(std::unique_ptr<ASTNode> node) override;
        void generateVariableDeclaration(std::unique_ptr<ASTNode> node) override;
        void generateIfStatement(std::unique_ptr<ASTNode> node) override;
        std::string generateBinaryOperation(std::unique_ptr<ASTNode> node) override;
        std::string generateUnaryOperation(std::unique_ptr<ASTNode> node) override;

    public:
        ~CodeGenLinux() override = default;
        CodeGenLinux(std::ostream &outstream) : CodeGen(RegisterAllocator::forSysV(), outstream){};
        void generate(std::unique_ptr<ASTNode> program) override;
    };

    class CodeGenWindows : public CodeGen
    {
    private:
        std::unordered_map<std::uint32_t, char> integer_suffixes = {{8, 'b'}, {16, 'w'}, {32, 'l'}, {64, 'q'}};
        std::string intToXmm(const std::string &r_int, uint32_t bits) override;

        void generateStatement(std::unique_ptr<ASTNode> statement) override;

        std::string emitExpression(std::unique_ptr<ASTNode> node) override;

        void emitEpilogue() override;
        void emitPrologue(std::unique_ptr<ASTNode> blockNode) override;

        std::string generateIntegerLiteral(std::unique_ptr<ASTNode> node) override;
        std::string generateFloatLiteral(std::unique_ptr<ASTNode> node) override;
        std::string generateStringLiteral(std::unique_ptr<ASTNode> node) override;
        std::string generateBooleanLiteral(std::unique_ptr<ASTNode> node) override;
        std::string generateVariableAccess(std::unique_ptr<ASTNode> node) override;
        void generateVariableReassignment(std::unique_ptr<ASTNode> node) override;
        void generateVariableDeclaration(std::unique_ptr<ASTNode> node) override;
        void generateIfStatement(std::unique_ptr<ASTNode> node) override;
        std::string generateBinaryOperation(std::unique_ptr<ASTNode> node) override;
        std::string generateUnaryOperation(std::unique_ptr<ASTNode> node) override;

    public:
        ~CodeGenWindows() override = default;
        CodeGenWindows(std::ostream &outstream) : CodeGen(RegisterAllocator::forMSVC(), outstream){};
        void generate(std::unique_ptr<ASTNode> program) override;
    };

    class CodeGenLLVM : public CodeGen
    {
    private:
        std::unordered_map<std::uint32_t, char> integer_suffixes = {{8, 'b'}, {16, 'w'}, {32, 'l'}, {64, 'q'}};
        std::unordered_map<std::string, std::string> stringLiterals;
        std::string intToXmm(const std::string &r_int, uint32_t bits) override;

        void generateStatement(std::unique_ptr<ASTNode> statement) override;

        std::string emitExpression(std::unique_ptr<ASTNode> node) override;

        void emitEpilogue() override;
        void emitPrologue(std::unique_ptr<ASTNode> blockNode) override;

        std::string generateIntegerLiteral(std::unique_ptr<ASTNode> node) override;
        std::string generateFloatLiteral(std::unique_ptr<ASTNode> node) override;
        std::string generateStringLiteral(std::unique_ptr<ASTNode> node) override;
        std::string generateBooleanLiteral(std::unique_ptr<ASTNode> node) override;
        std::string generateVariableAccess(std::unique_ptr<ASTNode> node) override;
        void generateVariableReassignment(std::unique_ptr<ASTNode> node) override;
        void generateVariableDeclaration(std::unique_ptr<ASTNode> node) override;
        void generateIfStatement(std::unique_ptr<ASTNode> node) override;
        std::string generateBinaryOperation(std::unique_ptr<ASTNode> node) override;
        std::string generateUnaryOperation(std::unique_ptr<ASTNode> node) override;

    public:
        ~CodeGenLLVM() override = default;
        CodeGenLLVM(std::ostream &outstream) : CodeGen(RegisterAllocator(), outstream){};
        void generate(std::unique_ptr<ASTNode> program) override;
    };
} // namespace zlang