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

    // ---------------- Linux ----------------
    std::string CodeGenLinux::emitExpr(const ASTNode *node, std::ostream &out, RegisterAllocator &alloc)
    {
        using NT = NodeType;
        static int constCounter = 0;
        if (node->type == NT::IntegerLiteral)
        {
            auto r = alloc.allocate();
            out << "    movq $" << node->value << ", %" << r << "\n";
            return r;
        }
        if (node->type == NT::FloatLiteral)
        {
            // emit double constant in rodata
            std::string lbl = ".LC" + std::to_string(constCounter++);
            out << ".section .rodata\n";
            std::string dblValue = node->value;
            // Remove 'f' suffix if present
            if (dblValue.size() > 0 && (dblValue.back() == 'f' || dblValue.back() == 'F'))
            {
                dblValue = dblValue.substr(0, dblValue.size() - 1);
            }
            out << lbl << ": .double " << dblValue << "\n";
            out << ".section .text\n";

            // Use XMM register for floating-point operations
            std::string xmm_reg = alloc.allocateXMM();
            out << "    movsd " << lbl << "(%rip), %" << xmm_reg << "\n";

            // Move to general-purpose register via memory
            out << "    subq $8, %rsp\n";
            out << "    movsd %" << xmm_reg << ", (%rsp)\n";
            auto r = alloc.allocate();
            out << "    movq (%rsp), %" << r << "\n";
            out << "    addq $8, %rsp\n";
            return r;
        }
        if (node->type == NT::StringLiteral)
        {
            std::string lbl = ".LC" + std::to_string(constCounter++);
            out << ".section .rodata\n"; // Fixed: added newline
            out << lbl << ": .string \"" << node->value << "\"\n";
            out << ".section .text\n"; // Fixed: added newline
            auto r = alloc.allocate();
            out << "    leaq " << lbl << "(%rip), %" << r << "\n";
            return r;
        }
        if (node->type == NT::VariableAccess)
        {
            auto r = alloc.allocate();
            out << "    movq " << node->value << "(%rip), %" << r << "\n";
            return r;
        }
        if (node->type == NT::BinaryOp)
        {
            auto l = emitExpr(node->children[0].get(), out, alloc);
            auto r = emitExpr(node->children[1].get(), out, alloc);
            const auto &op = node->value;
            if (op == "+")
                out << "    addq %" << r << ", %" << l << "\n";
            else if (op == "-")
                out
                    << "    subq %" << r << ", %" << l << "\n";
            else if (op == "*")
                out
                    << "    imulq %" << r << ", %" << l << "\n";
            else if (op == "/")
            {
                out << "    movq %" << l << ", %rax ";
                out
                    << "    cqo ";
                out
                    << "    idivq %" << r << "\n";
                alloc.free(l);
                alloc.free(r);
                return std::string("rax");
            }
            else if (op == "&&" || op == "&")
                out << "    andq %" << r << ", %" << l << "\n";
            else if (op == "||" || op == "|")
                out
                    << "    orq %" << r << ", %" << l << "\n";
            alloc.free(r);
            return l;
        }
        if (node->type == NT::UnaryOp)
        {
            const auto &op = node->value;
            if (op == "!")
            {
                auto r = emitExpr(node->children[0].get(), out, alloc);
                out << "    cmpq $0, %" << r << "\n";
                out
                    << "    sete %al ";
                out
                    << "    movzbq %al, %" << r << "\n";
                return r;
            }
            if (op == "++" || op == "--")
            {
                auto var = node->children[0]->value;
                auto r = alloc.allocate();
                out << "    movq " << var << "(%rip), %" << r << "\n";
                out
                    << (op == "++" ? "    incq " : "    decq ") << "%" << r << "\n";
                out
                    << "    movq %" << r << ", " << var << "(%rip)\n";
                return r;
            }
        }
        node->print(std::cout, 10);
        throw std::runtime_error("Unsupported expr");
    }

    void CodeGenLinux::generate(const ASTNode *program, std::ostream &out)
    {
        std::vector<std::string> globals;
        for (auto &s : program->children)
            if (s->type == NodeType::VariableDeclaration)
                globals.push_back(s->value);
        out << ".data\n\n";
        for (auto &g : globals)
            out << g << ": .quad 0\n";
        out << "\n.section .rodata\n"; // Ensure rodata section exists
        out << ".text\n.global _start\n_start:\n\n";

        RegisterAllocator alloc = RegisterAllocator::forSysV();
        for (auto &s : program->children)
        {
            if (s->type == NodeType::VariableDeclaration)
            {
                if (s->children.size() == 2)
                {
                    auto r = emitExpr(s->children[1].get(), out, alloc);
                    out << "    movq %" << r << ", " << s->value << "(%rip)\n";
                    alloc.free(r);
                }
                // else case remains
            }
            if (s->type == NodeType::VariableReassignment && !s->children.empty())
            {
                auto r = emitExpr(s->children[0].get(), out, alloc);
                out << "    movq %" << r << ", " << s->value << "(%rip)\n";
                alloc.free(r);
            }
            out << "\n";
        }
        out << "    movq $60, %rax\n    movq $0, %rdi\n    syscall\n";
    }

    // --------------- Windows ---------------
    std::string CodeGenWindows::emitExpr(const ASTNode *node, std::ostream &out, RegisterAllocator &alloc)
    {
        using NT = NodeType;
        static int constCounter = 0;
        if (node->type == NT::IntegerLiteral)
        {
            auto r = alloc.allocate();
            out << "    mov " << r << ", " << node->value << "\r\n";
            return r;
        }
        if (node->type == NT::FloatLiteral)
        {
            std::string lbl = "dblConst_" + std::to_string(constCounter++);
            std::string dblValue = node->value;
            // Remove 'f' suffix if present
            if (dblValue.size() > 0 && (dblValue.back() == 'f' || dblValue.back() == 'F'))
            {
                dblValue = dblValue.substr(0, dblValue.size() - 1);
            }
            out << ".data\r\n";
            out << lbl << " dq " << dblValue << "\r\n";
            out << ".code\r\n";
            auto r = alloc.allocate();
            out << "    mov " << r << ", [" << lbl << "]\r\n";
            return r;
        }
        if (node->type == NT::StringLiteral)
        {
            std::string lbl = "strConst_" + std::to_string(constCounter++);
            out << ".data\r\n";
            out << lbl << " db \"" << escapeString(node->value) << "\", 0\r\n";
            out << ".code\r\n";
            auto r = alloc.allocate();
            out << "    mov " << r << ", OFFSET " << lbl << "\r\n";
            return r;
        }
        if (node->type == NT::VariableAccess)
        {
            auto r = alloc.allocate();
            out << "    mov " << r << ", [" << node->value << "]\r\n";
            return r;
        }
        if (node->type == NT::BinaryOp)
        {
            auto l = emitExpr(node->children[0].get(), out, alloc);
            auto r = emitExpr(node->children[1].get(), out, alloc);
            auto &op = node->value;
            if (op == "+")
                out << "    add ";
            else if (op == "-")
                out << "    sub ";
            else if (op == "*")
                out << "    imul ";
            else if (op == "/")
            { /*DIV needs rax*/
            }
            else if (op == "&&" || op == "&")
                out << "    and ";
            else if (op == "||" || op == "|")
                out << "    or  ";
            out << r << ", " << l << "\r\n";
            alloc.free(r);
            return l;
        }
        if (node->type == NT::UnaryOp)
        {
            auto &op = node->value;
            if (op == "!")
            {
                auto r = emitExpr(node->children[0].get(), out, alloc);
                out << "    cmp " << r << ", 0\r\n    sete al\r\n    movzx r" << r << ", al\r\n";
                return r;
            }
            if (op == "++" || op == "--")
            {
                auto var = node->children[0]->value;
                auto r = alloc.allocate();
                out << "    mov " << r << ", [" << var << "]\r\n";
                out << (op == "++" ? "    inc " : "    dec ") << r << "\r\n";
                out << "    mov [" << var << "], " << r << "\r\n";
                return r;
            }
        }
        throw std::runtime_error("Unsupported expr");
    }

    void CodeGenWindows::generate(const ASTNode *program, std::ostream &out)
    {
        std::vector<std::string> globals;
        for (auto &s : program->children)
            if (s->type == NodeType::VariableDeclaration)
                globals.push_back(s->value);
        out << ".data\r\n";
        for (auto &g : globals)
            out << g << ": dq 0\r\n";
        out << ".code\r\nmain PROC\r\n";

        RegisterAllocator alloc = RegisterAllocator::forMSVC();
        for (auto &s : program->children)
        {
            if (s->type == NodeType::VariableDeclaration || s->type == NodeType::VariableReassignment)
            {
                if (!s->children.empty())
                {
                    auto r = emitExpr(s->children[0].get(), out, alloc);
                    out << "    mov [" << s->value << "], " << r << "\r\n";
                    alloc.free(r);
                }
            }
        }
        out << "    xor rax, rax\r\n    ret\r\nmain ENDP\r\n";
    }

    // --------- LLVM IR ----------

    void CodeGenLLVM::emitExpr(const ASTNode *node, std::ostream &out, int &tempCounter)
    {
        using NT = NodeType;
        if (node->type == NT::IntegerLiteral)
        {
            out << "  %" << tempCounter << " = add i64 0, " << node->value << "\n";
            tempCounter++;
        }
        else if (node->type == NT::FloatLiteral)
        {
            std::string dblValue = node->value;
            // Remove 'f' suffix if present
            if (dblValue.size() > 0 && (dblValue.back() == 'f' || dblValue.back() == 'F'))
            {
                dblValue = dblValue.substr(0, dblValue.size() - 1);
            }
            out << "  %" << tempCounter << " = bitcast double " << dblValue << " to i64\n";
            tempCounter++;
        }
        else if (node->type == NT::StringLiteral)
        {
            std::string strConst = "@.str" + std::to_string(stringCounter++);
            out << strConst << " = private unnamed_addr constant ["
                << (node->value.size() + 1) << " x i8] c\""
                << escapeString(node->value) << "\\00\"\n";
            out << "  %" << tempCounter << " = ptrtoint ["
                << (node->value.size() + 1) << " x i8]* " << strConst << " to i64\n";
            tempCounter++;
        }
        else if (node->type == NT::VariableAccess)
        {
            out << "  %" << tempCounter << " = load i64, i64* @" << node->value << "\n";
            tempCounter++;
        }
        else if (node->type == NT::BinaryOp)
        {
            emitExpr(node->children[0].get(), out, tempCounter);
            int left = tempCounter - 1;
            emitExpr(node->children[1].get(), out, tempCounter);
            int right = tempCounter - 1;

            const auto &op = node->value;
            if (op == "+")
                out << "  %" << tempCounter << " = add i64 %" << left << ", %" << right << "\n";
            else if (op == "-")
                out << "  %" << tempCounter << " = sub i64 %" << left << ", %" << right << "\n";
            else if (op == "*")
                out << "  %" << tempCounter << " = mul i64 %" << left << ", %" << right << "\n";
            else if (op == "/")
                out << "  %" << tempCounter << " = sdiv i64 %" << left << ", %" << right << "\n";
            else if (op == "&&")
                out << "  %" << tempCounter << " = and i64 %" << left << ", %" << right << "\n";
            else if (op == "||")
                out << "  %" << tempCounter << " = or i64 %" << left << ", %" << right << "\n";
            else if (op == "&")
                out << "  %" << tempCounter << " = and i64 %" << left << ", %" << right << "\n";
            else if (op == "|")
                out << "  %" << tempCounter << " = or i64 %" << left << ", %" << right << "\n";

            tempCounter++;
        }
        else if (node->type == NT::UnaryOp)
        {
            emitExpr(node->children[0].get(), out, tempCounter);
            int operand = tempCounter - 1;
            const auto &op = node->value;

            if (op == "!")
            {
                out << "  %" << tempCounter << " = icmp eq i64 %" << operand << ", 0\n";
                tempCounter++;
                out << "  %" << tempCounter << " = zext i1 %" << (tempCounter - 1) << " to i64\n";
            }
            else if (op == "++" || op == "--")
            {
                out << "  %" << tempCounter << " = " << (op == "++" ? "add" : "sub")
                    << " i64 %" << operand << ", 1\n";
                tempCounter++;
                out << "  store i64 %" << (tempCounter - 1) << ", i64* @" << node->children[0]->value << "\n";
            }
            tempCounter++;
        }
        else
        {
            throw std::runtime_error("Unsupported expr in LLVM");
        }
    }

    void CodeGenLLVM::generate(const ASTNode *program, std::ostream &out)
    {
        std::vector<std::string> globals;
        for (auto &s : program->children)
            if (s->type == NodeType::VariableDeclaration)
                globals.push_back(s->value);

        // Declare string constants
        out << "; String constants\n";
        for (auto &s : program->children)
        {
            if (s->type == NodeType::StringLiteral)
            {
                out << "@.str" << stringCounter++ << " = private unnamed_addr constant ["
                    << (s->value.size() + 1) << " x i8] c\"" << escapeString(s->value) << "\\00\"\n";
            }
        }

        // Declare global variables
        out << "; Global variables\n";
        for (auto &g : globals)
            out << "@" << g << " = global i64 0\n";

        out << "define i32 @main() {\n";
        int tempCounter = 0;

        for (auto &s : program->children)
        {
            if (s->type == NodeType::VariableDeclaration && s->children.size() == 2)
            {
                emitExpr(s->children[1].get(), out, tempCounter);
                out << "  store i64 %" << (tempCounter - 1) << ", i64* @" << s->value << "\n";
            }
            else if (s->type == NodeType::VariableReassignment && !s->children.empty())
            {
                emitExpr(s->children[0].get(), out, tempCounter);
                out << "  store i64 %" << (tempCounter - 1) << ", i64* @" << s->value << "\n";
            }
        }
        out << "  ret i32 0\n}\n";
    }

    std::string escapeString(const std::string &input)
    {
        std::string output;
        for (char c : input)
        {
            switch (c)
            {
            case '\n':
                output += "\\0A";
                break;
            case '\t':
                output += "\\09";
                break;
            case '\"':
                output += "\\22";
                break;
            case '\\':
                output += "\\5C";
                break;
            default:
                output += c;
            }
        }
        return output;
    }
}
