#pragma once

#include <array>
#include <functional>
#include <map>
#include <memory>
#include <ostream>
#include <sstream>

#include "ast/ASTNode.hpp"
#include "codegen/RegisterAllocator.hpp"
#include "typechecker/TypeChecker.hpp"

namespace zust {
    enum class TargetTriple {
        X86_64_LINUX,
        X86_64_WINDOWS,
        LLVM_IR
    };

    class CodeGen {
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

        std::ostringstream outGlobalStream;
        std::ostringstream outStream;

        std::string adjustReg(const std::string &r64, uint64_t bits) {
            std::string baseRegister = RegisterAllocator::getBaseReg(r64);
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

            auto it = registers_based_on_bytes.find(baseRegister);
            if (it == registers_based_on_bytes.end())
                throw std::runtime_error("Unknown register '" + baseRegister + "'\n\n");
            const auto &ents = it->second;

            switch (bits) {
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

        static std::string getCorrectMove(uint64_t size_bytes, bool isfloat) {
            if (isfloat) {
                if (size_bytes == 4)
                    return "movss";
                else if (size_bytes == 8)
                    return "movsd";
                else
                    throw std::runtime_error("Bad float move size");
            } else {
                switch (size_bytes) {
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

        virtual void generateStatement(std::unique_ptr<ASTNode> statement, std::ostringstream &out) = 0;

        virtual std::string emitExpression(std::unique_ptr<ASTNode> node, std::ostringstream &out) = 0;

        virtual void emitEpilogue(std::shared_ptr<ScopeContext> scope, std::ostringstream &out, bool clearRax = false) = 0;
        virtual void emitPrologue(std::shared_ptr<ScopeContext> scope, std::ostringstream &out) = 0;

        virtual std::string generateIntegerLiteral(std::unique_ptr<ASTNode> node, std::ostringstream &out) = 0;
        virtual std::string generateFloatLiteral(std::unique_ptr<ASTNode> node, std::ostringstream &out) = 0;
        virtual std::string generateStringLiteral(std::unique_ptr<ASTNode> node, std::ostringstream &out) = 0;
        virtual std::string generateBooleanLiteral(std::unique_ptr<ASTNode> node, std::ostringstream &out) = 0;
        virtual std::string generateVariableAccess(std::unique_ptr<ASTNode> node, std::ostringstream &out) = 0;

        virtual void generateVariableReassignment(std::unique_ptr<ASTNode> node, std::ostringstream &out) = 0;
        virtual void generateVariableDeclaration(std::unique_ptr<ASTNode> node, std::ostringstream &out) = 0;
        virtual void generateIfStatement(std::unique_ptr<ASTNode> node, std::ostringstream &out) = 0;

        virtual std::string generateBinaryOperation(std::unique_ptr<ASTNode> node, std::ostringstream &out) = 0;
        virtual std::string generateUnaryOperation(std::unique_ptr<ASTNode> node, std::ostringstream &out) = 0;

        virtual std::string generateFunctionCall(std::unique_ptr<ASTNode> node, std::ostringstream &out) = 0;                      // Expression
        virtual void generateFunctionDeclaration(std::unique_ptr<ASTNode> node, std::ostringstream &out, bool force = false) = 0;  // Statement
        virtual void generateExternFunctionDeclaration(std::unique_ptr<ASTNode> node, std::ostringstream &out) = 0;                // Statement

    public:
        CodeGen(RegisterAllocator alloc, std::ostream &outstream) : stringLabelCount(0), blockLabelCount(0), doubleLabelCount(0), floatLabelCount(0), alloc(alloc), outfinal(outstream) {}
        virtual ~CodeGen();
        virtual void generate(std::unique_ptr<ASTNode> program) = 0;
        static std::unique_ptr<CodeGen> create(TargetTriple target, std::ostream &outstream);
    };

    class CodeGenLinux : public CodeGen {
    private:
        std::unordered_map<std::uint32_t, char> integer_suffixes = {{8, 'b'}, {16, 'w'}, {32, 'l'}, {64, 'q'}};

        std::string allocateOrSpill(bool isXMM, std::shared_ptr<ScopeContext> scope, std::ostringstream &out);
        void restoreIfSpilled(const std::string &reg, std::shared_ptr<ScopeContext> scope, std::ostringstream &out);

        void generateStatement(std::unique_ptr<ASTNode> statement, std::ostringstream &out) override;

        std::string emitExpression(std::unique_ptr<ASTNode> node, std::ostringstream &out) override;

        void emitEpilogue(std::shared_ptr<ScopeContext> scope, std::ostringstream &out, bool clearRax = false) override;
        void emitPrologue(std::shared_ptr<ScopeContext> scope, std::ostringstream &out) override;

        std::string generateIntegerLiteral(std::unique_ptr<ASTNode> node, std::ostringstream &out) override;
        std::string generateFloatLiteral(std::unique_ptr<ASTNode> node, std::ostringstream &out) override;
        std::string generateStringLiteral(std::unique_ptr<ASTNode> node, std::ostringstream &out) override;
        std::string generateBooleanLiteral(std::unique_ptr<ASTNode> node, std::ostringstream &out) override;
        std::string generateVariableAccess(std::unique_ptr<ASTNode> node, std::ostringstream &out) override;

        void generateVariableReassignment(std::unique_ptr<ASTNode> node, std::ostringstream &out) override;
        void generateVariableDeclaration(std::unique_ptr<ASTNode> node, std::ostringstream &out) override;
        void generateIfStatement(std::unique_ptr<ASTNode> node, std::ostringstream &out) override;

        std::string generateBinaryOperation(std::unique_ptr<ASTNode> node, std::ostringstream &out) override;
        std::string generateUnaryOperation(std::unique_ptr<ASTNode> node, std::ostringstream &out) override;

        std::string castValue(const std::string &val, const TypeInfo &fromType, const TypeInfo &toType, const std::shared_ptr<ScopeContext> currentScope, std::ostringstream &out);

        std::string generateFunctionCall(std::unique_ptr<ASTNode> node, std::ostringstream &out) override;                      // Expression
        void generateFunctionDeclaration(std::unique_ptr<ASTNode> node, std::ostringstream &out, bool force = false) override;  // Statement
        void generateExternFunctionDeclaration(std::unique_ptr<ASTNode> node, std::ostringstream &out) override;                // Statement
        void generateReturnstatement(std::unique_ptr<ASTNode> node, std::ostringstream &out);                                   // Statement

    public:
        ~CodeGenLinux() override = default;
        CodeGenLinux(std::ostream &outstream) : CodeGen(RegisterAllocator::forSysV(), outstream) {};
        void generate(std::unique_ptr<ASTNode> program) override;
    };

    class CodeGenWindows : public CodeGen {
    private:
        std::unordered_map<std::uint32_t, char> integer_suffixes = {{8, 'b'}, {16, 'w'}, {32, 'l'}, {64, 'q'}};

        std::string allocateOrSpill(bool isXMM, std::shared_ptr<ScopeContext> scope, std::ostringstream &out);
        void restoreIfSpilled(const std::string &reg, std::shared_ptr<ScopeContext> scope, std::ostringstream &out);

        void generateStatement(std::unique_ptr<ASTNode> statement, std::ostringstream &out) override;
        std::string emitExpression(std::unique_ptr<ASTNode> node, std::ostringstream &out) override;

        void emitEpilogue(std::shared_ptr<ScopeContext> scope, std::ostringstream &out, bool clearRax = false) override;
        void emitPrologue(std::shared_ptr<ScopeContext> scope, std::ostringstream &out) override;

        std::string generateIntegerLiteral(std::unique_ptr<ASTNode> node, std::ostringstream &out) override;
        std::string generateFloatLiteral(std::unique_ptr<ASTNode> node, std::ostringstream &out) override;
        std::string generateStringLiteral(std::unique_ptr<ASTNode> node, std::ostringstream &out) override;
        std::string generateBooleanLiteral(std::unique_ptr<ASTNode> node, std::ostringstream &out) override;
        std::string generateVariableAccess(std::unique_ptr<ASTNode> node, std::ostringstream &out) override;

        void generateVariableReassignment(std::unique_ptr<ASTNode> node, std::ostringstream &out) override;
        void generateVariableDeclaration(std::unique_ptr<ASTNode> node, std::ostringstream &out) override;
        void generateIfStatement(std::unique_ptr<ASTNode> node, std::ostringstream &out) override;

        std::string generateBinaryOperation(std::unique_ptr<ASTNode> node, std::ostringstream &out) override;
        std::string generateUnaryOperation(std::unique_ptr<ASTNode> node, std::ostringstream &out) override;

        std::string castValue(const std::string &val, const TypeInfo &fromType, const TypeInfo &toType, const std::shared_ptr<ScopeContext> currentScope, std::ostringstream &out);

        std::string generateFunctionCall(std::unique_ptr<ASTNode> node, std::ostringstream &out) override;                      // Expression
        void generateFunctionDeclaration(std::unique_ptr<ASTNode> node, std::ostringstream &out, bool force = false) override;  // Statement
        void generateExternFunctionDeclaration(std::unique_ptr<ASTNode> node, std::ostringstream &out) override;                // Statement
        void generateReturnstatement(std::unique_ptr<ASTNode> node, std::ostringstream &out);                                   // Statement

    public:
        ~CodeGenWindows() override = default;
        CodeGenWindows(std::ostream &outstream) : CodeGen(RegisterAllocator::forMSVC(), outstream) {};
        void generate(std::unique_ptr<ASTNode> program) override;
    };

    class CodeGenLLVM : public CodeGen {
    private:
        std::unordered_map<std::string, std::string> stringLiterals;

        void generateStatement(std::unique_ptr<ASTNode> statement, std::ostringstream &out) override;
        std::string emitExpression(std::unique_ptr<ASTNode> node, std::ostringstream &out) override;

        void emitEpilogue(std::shared_ptr<ScopeContext> scope, std::ostringstream &out, bool clearRax = false) override;
        void emitPrologue(std::shared_ptr<ScopeContext> scope, std::ostringstream &out) override;

        std::string generateIntegerLiteral(std::unique_ptr<ASTNode> node, std::ostringstream &out) override;
        std::string generateFloatLiteral(std::unique_ptr<ASTNode> node, std::ostringstream &out) override;
        std::string generateStringLiteral(std::unique_ptr<ASTNode> node, std::ostringstream &out) override;
        std::string generateBooleanLiteral(std::unique_ptr<ASTNode> node, std::ostringstream &out) override;
        std::string generateVariableAccess(std::unique_ptr<ASTNode> node, std::ostringstream &out) override;

        void generateVariableReassignment(std::unique_ptr<ASTNode> node, std::ostringstream &out) override;
        void generateVariableDeclaration(std::unique_ptr<ASTNode> node, std::ostringstream &out) override;
        void generateIfStatement(std::unique_ptr<ASTNode> node, std::ostringstream &out) override;

        std::string generateBinaryOperation(std::unique_ptr<ASTNode> node, std::ostringstream &out) override;
        std::string generateUnaryOperation(std::unique_ptr<ASTNode> node, std::ostringstream &out) override;
        std::string castValue(const std::string &val, const TypeInfo &fromType, const TypeInfo &toType, std::ostringstream &out);

        std::string generateFunctionCall(std::unique_ptr<ASTNode> node, std::ostringstream &out) override;                      // Expression
        void generateFunctionDeclaration(std::unique_ptr<ASTNode> node, std::ostringstream &out, bool force = false) override;  // Statement
        void generateExternFunctionDeclaration(std::unique_ptr<ASTNode> node, std::ostringstream &out) override;                // Statement
        void generateReturnstatement(std::unique_ptr<ASTNode> node, std::ostringstream &out);                                   // Statement

    public:
        ~CodeGenLLVM() override = default;
        CodeGenLLVM(std::ostream &outstream) : CodeGen(RegisterAllocator(), outstream) {};
        void generate(std::unique_ptr<ASTNode> program) override;
    };
}  // namespace zust