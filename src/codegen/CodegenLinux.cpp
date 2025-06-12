#include "all.hpp"

namespace zlang
{
    std::string CodeGenLinux::emitExpr(const ASTNode *node,
                                       std::ostream &out,
                                       RegisterAllocator &alloc)
    {
        using NT = NodeType;
        static int constCounter = 0;
        static std::unordered_map<std::string, TypeInfo> regType;

        auto noteType = [&](const std::string &r, const TypeInfo &t)
        {
            regType[r] = t;
        };

        auto intToXmm = [&](const std::string &r_int, const TypeInfo &t_int)
        {
            (void)t_int;
            std::string r_xmm = alloc.allocateXMM();
            out << "    cvtsi2sd %" << r_int << ", %" << r_xmm << "\n";
            alloc.free(r_int);
            noteType(r_xmm, {/*bits=*/64, /*isFloat=*/true, /*isSigned=*/true});
            return r_xmm;
        };

        auto xmmToGpr = [&](const std::string &r_xmm)
        {
            // spill to stack + reload
            out << "    subq $8, %rsp\n"
                   "    movsd %"
                << r_xmm << ", (%rsp)\n";
            alloc.free(r_xmm);
            auto r = alloc.allocate();
            out << "    movq   (%rsp), %" << r << "\n"
                                                  "    addq   $8, %rsp\n";
            noteType(r, {/*bits=*/64, /*isFloat=*/true, /*isSigned=*/true});
            return r;
        };

        // === Literals ===
        if (node->type == NT::IntegerLiteral)
        {
            auto r = alloc.allocate();
            out << "    movq $" << node->value << ", %" << r << "\n";
            noteType(r, {64, /*isFloat=*/false, /*isSigned=*/true});
            return r;
        }
        if (node->type == NT::FloatLiteral)
        {
            std::string lbl = ".LC" + std::to_string(constCounter++);
            std::string val = node->value;
            if (!val.empty() && (val.back() == 'f' || val.back() == 'F'))
                val.pop_back();
            out << ".section .rodata\n"
                << lbl << ": .double " << val << "\n"
                << ".section .text\n";
            // load into XMM
            std::string r_xmm = alloc.allocateXMM();
            out << "    movsd " << lbl << "(%rip), %" << r_xmm << "\n";
            noteType(r_xmm, {64, /*isFloat=*/true, /*isSigned=*/true});
            // spill to GPR
            return xmmToGpr(r_xmm);
        }
        if (node->type == NT::StringLiteral)
        {
            std::string lbl = ".LC" + std::to_string(constCounter++);
            out << ".section .rodata\n"
                << lbl << ": .string \"" << node->value << "\"\n"
                << ".section .text\n";
            auto r = alloc.allocate();
            out << "    leaq " << lbl << "(%rip), %" << r << "\n";
            // treat pointers as 64‑bit ints
            noteType(r, {64, /*isFloat=*/false, /*isSigned=*/false});
            return r;
        }

        // === Variable access ===
        if (node->type == NT::VariableAccess)
        {
            auto &scope = *node->scope;
            auto name = node->value;
            auto ti = scope.lookupType(scope.lookupVariable(name).type);
            uint64_t sz = ti.bits / 8;
            bool isF = ti.isFloat;
            std::string r;
            if (isF)
            {
                r = alloc.allocateXMM();
                out << "    " << getCorrectMove(sz)
                    << " " << name << "(%rip), %" << r << "\n";
            }
            else
            {
                r = alloc.allocate();
                out << "    " << getCorrectMove(sz)
                    << " " << name << "(%rip), %" << r << "\n";
            }
            noteType(r, ti);
            return isF ? xmmToGpr(r) : r;
        }

        // === Binary operations ===
        if (node->type == NT::BinaryOp)
        {
            auto rl = emitExpr(node->children[0].get(), out, alloc);
            auto rr = emitExpr(node->children[1].get(), out, alloc);
            auto t1 = regType.at(rl);
            auto t2 = regType.at(rr);
            auto tr = promoteType(t1, t2);
            std::string op = node->value;

            // if we need float ops:
            if (tr.isFloat)
            {
                // promote integer operands
                std::string xl = (t1.isFloat ? rl : intToXmm(rl, t1));
                std::string xr = (t2.isFloat ? rr : intToXmm(rr, t2));

                if (op == "+")
                    out << "    addsd %" << xr << ", %" << xl << "\n";
                else if (op == "-")
                    out << "    subsd %" << xr << ", %" << xl << "\n";
                else if (op == "*")
                    out << "    mulsd %" << xr << ", %" << xl << "\n";
                else if (op == "/")
                    out << "    divsd %" << xr << ", %" << xl << "\n";
                else if (op == "==" || op == "!=" || op == ">=" || op == ">" || op == "<=" || op == "<")
                {
                    // unordered compare
                    out << "    ucomisd %" << xr << ", %" << xl << "\n"
                        << "    " << assembly_operations[op] << " %al\n"
                        << "    movzbq %al, %" << xl << "\n";
                    // result in GPR
                    return xmmToGpr(xl);
                }
                else
                {
                    throw std::runtime_error("Unsupported FP op " + op);
                }
                // got result in xl(XMM) → spill back
                return xmmToGpr(xl);
            }

            // integer path
            char suf = getIntSuffix(tr.bits);
            if (op == "+")
                out << "    add" << suf << " %" << rr << ", %" << rl << "\n";
            else if (op == "-")
                out << "    sub" << suf << " %" << rr << ", %" << rl << "\n";
            else if (op == "*")
                out << "    imul" << suf << " %" << rr << ", %" << rl << "\n";
            else if (op == "/")
            {
                out << "    mov" << suf << " %" << rl << ", %rax\n"
                                                         "    cqo\n"
                                                         "    idiv"
                    << suf << " %" << rr << "\n";
                alloc.free(rl);
                alloc.free(rr);
                noteType("rax", tr);
                return std::string("rax");
            }
            else if (op == "==" || op == "!=" || op == ">=" || op == ">" || op == "<=" || op == "<")
            {
                out << "    cmp" << suf << " %" << rr << ", %" << rl << "\n"
                    << "    " << assembly_operations[op] << " %al\n"
                    << "    movzbq %al, %" << rl << "\n";
                alloc.free(rr);
                noteType(rl, {64, false, true});
                return rl;
            }
            else if (op == "&&" || op == "&")
                out << "    and" << suf << " %" << rr << ", %" << rl << "\n";
            else if (op == "||" || op == "|")
                out << "    or" << suf << " %" << rr << ", %" << rl << "\n";
            else
                throw std::runtime_error("Unsupported int op " + op);

            alloc.free(rr);
            noteType(rl, tr);
            return rl;
        }

        // === Boolean literal & unary ops stay identical ===
        if (node->type == NT::BooleanLiteral)
        {
            auto r = alloc.allocate();
            out << "    movq $" << (node->value == "true" ? "1" : "0")
                << ", %" << r << "\n";
            noteType(r, {64, false, false});
            return r;
        }
        if (node->type == NT::UnaryOp)
        {
            auto op = node->value;
            if (op == "!")
            {
                auto r = emitExpr(node->children[0].get(), out, alloc);
                out << "    cmpq $0, %" << r << "\n"
                                                "    sete %al\n"
                                                "    movzbq %al, %"
                    << r << "\n";
                noteType(r, {64, false, false});
                return r;
            }
            if (op == "++" || op == "--")
            {
                auto var = node->children[0]->value;
                auto r = alloc.allocate();
                out << "    movq " << var << "(%rip), %" << r << "\n"
                                                                 "    "
                    << (op == "++" ? "incq " : "decq ") << "%" << r << "\n"
                                                                       "    movq %"
                    << r << ", " << var << "(%rip)\n";
                noteType(r, {64, false, true});
                return r;
            }
        }

        throw std::runtime_error("Unsupported expr");
    }

    std::string CodeGenLinux::getCorrectMove(const uint64_t size_bytes)
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
        default:
            throw std::runtime_error("Unsupported size for move");
        }
    }

    void CodeGenLinux::generateStatement(const ASTNode *s, std::ostream &out)
    {
        if (s->type == NodeType::VariableDeclaration)
        {
            if (s->children.size() == 2)
            {
                auto &scope = *s->scope;
                auto varName = s->value;
                auto varTypeName = scope.lookupVariable(varName).type;
                std::uint64_t size = scope.lookupType(varTypeName).bits / 8;

                std::string r = emitExpr(s->children[1].get(), out, alloc);

                if (scope.isGlobalScope())
                {
                    out << "    " << getCorrectMove(size)
                        << " %" << r << ", " << varName << "(%rip)\n";
                }
                else
                {
                    // Stack-based variable: store at [rbp + offset]
                    int offset = scope.getVariableOffset(varName);
                    out << "    " << getCorrectMove(size)
                        << " %" << r << ", " << offset << "(%rbp)\n";
                }
                alloc.free(r);
            }
            // else case remains
        }
        if (s->type == NodeType::VariableReassignment && !s->children.empty())
        {
            auto &scope = *s->scope;
            auto varName = s->value;
            auto varTypeName = scope.lookupVariable(varName).type;
            std::uint64_t size = scope.lookupType(varTypeName).bits / 8;

            std::string r = emitExpr(s->children[0].get(), out, alloc);

            if (scope.isGlobalScope())
            {
                out << "    " << getCorrectMove(size)
                    << " %" << r << ", " << varName << "(%rip)\n";
            }
            else
            {
                int offset = scope.getVariableOffset(varName);
                out << "    " << getCorrectMove(size)
                    << " %" << r << ", " << offset << "(%rbp)\n";
            }
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
            auto emitPrologue = [&](const ASTNode *blockNode)
            {
                auto blkScope = blockNode->scope;
                int allocSize = -blkScope->stackOffset + 8;
                // +8 to cover the pushed RBP
                out << "    ; Block Starts (scope enter)\n";
                out << "    push   %rbp\n";
                out << "    mov    %rsp, %rbp\n";
                out << "    sub    $" << allocSize << ", %rsp\n";
            };
            auto emitEpilogue = [&]()
            {
                out << "    ; Block Ends (scope exit)\n";
                out << "    leave\n";
            };
            const ASTNode *ifBlock = s->children[1].get();
            emitPrologue(ifBlock);
            for (auto &stmt : ifBlock->children)
                generateStatement(stmt.get(), out);
            emitEpilogue();
            out << "    jmp   " << endLbl << "\n";
            out << elseLbl << ":\n";
            ASTNode *branch = s->getElseBranch();
            while (branch)
            {
                if (branch->type == NodeType::ElseIfStatement)
                {
                    // test
                    auto r2 = emitExpr(branch->children[0].get(), out, alloc);
                    out << "    cmpq $0, %" << r2 << "\n";
                    out << "    je   " << elseLbl << "\n";
                    // on false, fall through to next
                    alloc.free(r2);

                    const ASTNode *elifBlock = branch->children[1].get();
                    emitPrologue(elifBlock);
                    for (auto &stmt : elifBlock->children)
                        generateStatement(stmt.get(), out);
                    emitEpilogue();
                    out << "    jmp   " << endLbl << "\n";

                    branch = branch->getElseBranch();
                }
                else if (branch->type == NodeType::ElseStatement)
                {
                    const ASTNode *elseBlock = branch->children[0].get();
                    emitPrologue(elseBlock);
                    for (auto &stmt : elseBlock->children)
                        generateStatement(stmt.get(), out);
                    emitEpilogue();
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
            std::vector<ASTNode *> globals;
            for (auto &s : program->children)
                if (s->type == NodeType::VariableDeclaration)
                    globals.push_back(s.get());
            out << ".data\n\n";
            for (auto &g : globals)
            {
                TypeInfo info = g->scope->lookupType(g->children[0]->value);
                switch (info.bits / 8)
                {
                case 8:
                    out << g->value << ": .quad 0\n";
                    break;
                case 4:
                    out << g->value << ": .long 0\n";
                    break;
                case 2:
                    out << g->value << ": .word 0\n";
                    break;
                case 1:
                    out << g->value << ": .byte 0\n";
                    break;
                default:
                    throw std::runtime_error("Unsupported global size");
                }
            }
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