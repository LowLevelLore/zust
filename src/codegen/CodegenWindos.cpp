#include "all.hpp"

namespace zlang
{
    // --------------- Windows ---------------
    std::string CodeGenWindows::emitExpr(
        const ASTNode *node,
        std::ostream &out,
        RegisterAllocator &alloc)
    {
        using NT = NodeType;
        static int constCounter = 0;
        if (node->type == NT::BooleanLiteral)
        {
            auto r = alloc.allocate();
            if (node->value == "true")
                out << "    mov " << r << ", 1\r\n";
            else
                out << "    mov " << r << ", 0\r\n";
            return r;
        }
        if (node->type == NT::IntegerLiteral)
        {
            auto r = alloc.allocate();
            out << "    mov " << r << ", " << node->value << "\r\n";
            return r;
        }
        if (node->type == NT::FloatLiteral)
        {
            std::string lbl = "dblConst_" + std::to_string(constCounter++);
            std::string dblVal = node->value;
            if (!dblVal.empty() && (dblVal.back() == 'f' || dblVal.back() == 'F'))
                dblVal.pop_back();
            // emit in data
            out << ".data\r\n"
                << lbl << " dq " << dblVal << "\r\n"
                << ".code\r\n";
            auto r = alloc.allocate();
            out << "    movsd " << r << ", [" << lbl << "]\r\n";
            return r;
        }
        if (node->type == NT::StringLiteral)
        {
            std::string lbl = "strConst_" + std::to_string(constCounter++);
            out << ".data\r\n"
                << lbl << " db \"" << escapeString(node->value) << "\",0\r\n"
                << ".code\r\n";
            auto r = alloc.allocate();
            out << "    lea " << r << ", OFFSET FLAT:" << lbl << "\r\n";
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
            const auto &op = node->value;

            if (op == "+")
                out << "    add " << r << ", " << l << "\r\n";
            else if (op == "-")
                out << "    sub " << r << ", " << l << "\r\n";
            else if (op == "*")
                out << "    imul " << r << ", " << l << "\r\n";
            else if (op == "/")
            {
                out << "    mov rax, " << l << "\r\n"
                    << "    cqo\r\n"
                    << "    idiv " << r << "\r\n";
                alloc.free(l);
                alloc.free(r);
                return "rax";
            }
            else if (op == "&&")
                out << "    and " << r << ", " << l << "\r\n";
            else if (op == "||")
                out << "    or  " << r << ", " << l << "\r\n";
            else if (op == "&")
                out << "    and " << r << ", " << l << "\r\n";
            else if (op == "|")
                out << "    or  " << r << ", " << l << "\r\n";

            // Comparison operators
            else if (op == "==" || op == "!=" || op == "<" || op == "<=" || op == ">" || op == ">=")
            {
                out << "    cmp " << r << ", " << l << "\r\n"
                    << "    " << assembly_operations[op] << " al\r\n"
                    << "    movzx " << l << ", al\r\n";
            }

            alloc.free(r);
            return l;
        }
        if (node->type == NT::UnaryOp)
        {
            const auto &op = node->value;

            // Logical NOT
            if (op == "!")
            {
                auto r = emitExpr(node->children[0].get(), out, alloc);
                out << "    cmp " << r << ", 0\r\n"
                    << "    sete al\r\n"
                    << "    movzx " << r << ", al\r\n";
                return r;
            }

            // Pre/Post ++/--
            if (op == "++" || op == "--")
            {
                auto var = node->children[0]->value;
                auto r = alloc.allocate();
                out << "    mov " << r << ", [" << var << "]\r\n"
                    << (op == "++" ? "    inc " : "    dec ") << r << "\r\n"
                    << "    mov [" << var << "], " << r << "\r\n";
                return r;
            }
        }

        throw std::runtime_error("Unsupported expr in Windows codegen");
    }

    void CodeGenWindows::generateStatement(const ASTNode *s, std::ostream &out)
    {
        if (s->type == NodeType::VariableDeclaration)
        {
            if (s->children.size() == 2)
            {
                auto r = emitExpr(s->children[1].get(), out, alloc);
                out << "    mov [" << s->value << "], " << r << "\r\n";
                alloc.free(r);
            }
        }
        if (s->type == NodeType::VariableReassignment && !s->children.empty())
        {
            auto r = emitExpr(s->children[0].get(), out, alloc);
            out << "    mov [" << s->value << "], " << r << "\r\n";
            alloc.free(r);
        }
        if (s->type == NodeType::IfStatement)
        {
            int id = labelCounter++;
            std::string elseLbl = "ELSE" + std::to_string(id);
            std::string endLbl = "END" + std::to_string(id);

            // condition
            auto cR = emitExpr(s->children[0].get(), out, alloc);
            out << "    cmp " << cR << ", 0\r\n";
            out << "    je " << elseLbl << "\r\n";
            alloc.free(cR);

            // then‑block
            for (auto &st : s->children[1]->children)
                generateStatement(st.get(), out);
            out << "    jmp " << endLbl << "\r\n";

            // else/elif
            out << elseLbl << ":\r\n";
            ASTNode *branch = s->getElseBranch();
            while (branch)
            {
                if (branch->type == NodeType::ElseIfStatement)
                {
                    auto r2 = emitExpr(branch->children[0].get(), out, alloc);
                    out << "    cmp " << r2 << ", 0\r\n";
                    alloc.free(r2);
                    for (auto &st : branch->children[1]->children)
                        generateStatement(st.get(), out);
                    out << "    jmp " << endLbl << "\r\n";
                    branch = branch->getElseBranch();
                }
                else // else
                {
                    for (auto &st : branch->children[0]->children)
                        generateStatement(st.get(), out);
                    break;
                }
            }

            out << endLbl << ":\r\n\r\n";
            return;
        }
    }

    void CodeGenWindows::generate(const ASTNode *program, std::ostream &out, bool isFirst)
    {
        if (isFirst)
        {
            std::vector<std::string> globals;
            for (auto &s : program->children)
                if (s->type == NodeType::VariableDeclaration)
                    globals.push_back(s->value);
            out << ".data\r\n";
            for (auto &g : globals)
                out << g << ": dq 0\r\n";
            out << ".code\r\nmain PROC\r\n";
        }
        RegisterAllocator alloc = RegisterAllocator::forMSVC();
        for (auto &s : program->children)
        {
            generateStatement(s.get(), out);
        }
        if (isFirst)
            out << "    xor rax, rax\r\n    ret\r\nmain ENDP\r\n";
    }
}
