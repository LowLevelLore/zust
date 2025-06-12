#include "all.hpp"

namespace zlang
{
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
            else if (op == ">=" || op == ">" || op == "<=" || op == "<" || op == "==" || op == "!=")
            {
                out << "    cmpq %" << r << ", %" << l << "\n";       // compare lhs and rhs
                out << "    " << assembly_operations[op] << " %al\n"; // set result in al
                out << "    movzbq %al, %" << l << "\n";              // zero-extend al to full reg
                alloc.free(r);
                return l; // l now holds 0 or 1
            }
            alloc.free(r);
            return l;
        }
        if (node->type == NT::BooleanLiteral)
        {
            if (node->value == "true")
            {
                auto r = alloc.allocate();
                out << "    movq $1" << ", %" << r << "\n";
                return r;
            }
            else
            {
                auto r = alloc.allocate();
                out << "    movq $0" << ", %" << r << "\n";
                return r;
            }
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
        throw std::runtime_error("Unsupported expr");
    }

    void CodeGenLinux::generateStatement(const ASTNode *s, std::ostream &out)
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
        if (s->type == NodeType::IfStatement)
        {
            int id = labelCounter++;
            std::string elseLbl = ".Lelse" + std::to_string(id);
            std::string endLbl = ".Lend" + std::to_string(id);
            auto condR = emitExpr(s->children[0].get(), out, alloc);
            out << "    cmpq $0, %" << condR << "\n";
            out << "    je " << elseLbl << "\n";
            alloc.free(condR);
            for (auto &stmt : s->children[1]->children)
                generateStatement(stmt.get(), out);
            out << "    jmp " << endLbl << "\n";
            out
                << elseLbl << ":\n";
            ASTNode *branch = s->getElseBranch();
            while (branch)
            {
                if (branch->type == NodeType::ElseIfStatement)
                {
                    auto cR = emitExpr(branch->children[0].get(), out, alloc);
                    out << "    cmpq $0, %" << cR << "\n";
                    alloc.free(cR);
                    for (auto &st : branch->children[1]->children)
                        generateStatement(st.get(), out);
                    out << "    jmp " << endLbl << "\n";
                    branch = branch->getElseBranch();
                }
                else if (branch->type == NodeType::ElseStatement)
                {
                    for (auto &st : branch->children[0]->children)
                        generateStatement(st.get(), out);
                    break;
                }
            }
            out
                << endLbl << ":\n\n";
            return;
        }
        out << "\n";
    }

    void CodeGenLinux::generate(const ASTNode *program, std::ostream &out, bool isFirst)
    {
        if (isFirst)
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
        }

        for (auto &s : program->children)
        {
            generateStatement(s.get(), out);
        }
        if (isFirst)
        {
            out << "    movq $60, %rax\n    movq $0, %rdi\n    syscall\n";
        }
    }
}