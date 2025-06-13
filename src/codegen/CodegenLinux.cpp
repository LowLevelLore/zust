#include "all.hpp"

namespace zlang
{

    CodeGenLinux::CodeGenLinux()
        : labelCounter(0)
    {
    }

    CodeGenLinux::~CodeGenLinux() = default;

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

        // Convert integer GPR -> XMM of given width
        auto intToXmm = [&](const std::string &r_int, uint32_t bits)
        {
            std::string r_xmm = alloc.allocateXMM();
            const char *cvt = (bits == 32 ? "cvtsi2ss" : "cvtsi2sd");
            out << "    " << cvt << " %" << r_int << ", %" << r_xmm << "\n";
            alloc.free(r_int);
            noteType(r_xmm, {bits, bits / 8, true, true});
            return r_xmm;
        };

        // === Literals ===
        if (node->type == NT::IntegerLiteral)
        {
            auto r = alloc.allocate();
            out << "    movq $" << node->value << ", %" << r << "\n";
            noteType(r, node->scope->lookupType("int64_t"));
            return r;
        }

        if (node->type == NT::FloatLiteral)
        {
            std::string lbl = ".LC" + std::to_string(constCounter++);
            std::string val = node->value;
            bool isF32 = (!val.empty() && (val.back() == 'f' || val.back() == 'F'));
            if (isF32)
                val.pop_back();

            // emit constant
            out << "# rodata\n"
                   ".section .rodata\n"
                << lbl << ": ." << (isF32 ? "float" : "double") << " " << val << "\n"
                                                                                 ".section .text\n";

            auto r_xmm = alloc.allocateXMM();
            out << "    " << (isF32 ? "movss " : "movsd ")
                << lbl << "(%rip), %" << r_xmm << "\n";
            noteType(r_xmm, (isF32 ? node->scope->lookupType("float") : node->scope->lookupType("double")));
            return r_xmm;
        }
        if (node->type == NT::StringLiteral)
        {
            std::string lbl = ".LC" + std::to_string(constCounter++);
            out << "# rodata\n"
                   ".section .rodata\n"
                << lbl << ": .string \"" << node->value << "\"\n"
                                                           ".section .text\n";
            auto r = alloc.allocate();
            out << "    leaq " << lbl << "(%rip), %" << r << "\n";
            noteType(r, node->scope->lookupType("string"));
            return r;
        }

        // === VariableAccess ===
        if (node->type == NT::VariableAccess)
        {
            auto &scope = *node->scope;
            auto name = node->value;
            auto ti = scope.lookupType(scope.lookupVariable(name).type);
            uint64_t sz = ti.bits / 8;

            if (scope.isGlobalVariable(name))
            {
                if (ti.isFloat)
                {
                    std::string r_xmm = alloc.allocateXMM();
                    out << "    " << (sz == 4 ? "movss " : "movsd ")
                        << name << "(%rip), %" << r_xmm << "\n";
                    noteType(r_xmm, ti);
                    return r_xmm;
                }
                else
                {
                    std::string r = alloc.allocate();
                    std::string adj = adjustReg(r, ti.bits);
                    out << "    " << getCorrectMove(sz, false)
                        << " " << name << "(%rip), %" << adj << "\n";
                    noteType(r, ti);
                    return r;
                }
            }
            else
            {
                int64_t off = scope.getVariableOffset(name);
                if (ti.isFloat)
                {
                    std::string r = alloc.allocateXMM();
                    out << "    "
                        << (sz == 4 ? "movss " : "movsd ")
                        << off << "(%rbp), %" << r << "\n";
                    noteType(r, (sz == 4 ? node->scope->lookupType("float") : node->scope->lookupType("double")));
                    return r;
                }
                else
                {
                    std::string r = alloc.allocate();
                    std::string adj = adjustReg(r, ti.bits);
                    out << "    " << getCorrectMove(sz, false)
                        << " " << off << "(%rbp), %" << adj << "\n";
                    noteType(r, ti);
                    return r;
                }
            }
        }

        // === BinaryOp ===
        if (node->type == NT::BinaryOp)
        {
            // Evaluate both operands
            auto rl = emitExpr(node->children[0].get(), out, alloc);
            auto rr = emitExpr(node->children[1].get(), out, alloc);

            // Lookup operand types and compute result type
            auto t1 = regType.at(rl);
            auto t2 = regType.at(rr);
            auto tr = TypeChecker::promoteType(t1, t2);
            const auto &op = node->value;

            logMessage("Operation: " + op);
            logMessage("Left Type: " + TypeChecker::typeName(t1));
            logMessage(t1.to_string());
            logMessage("Right Type: " + TypeChecker::typeName(t2));
            logMessage(t2.to_string());

            // --- Floating‑point path ---
            if (tr.isFloat)
            {
                bool isF32 = (tr.bits == 32);
                std::string xr = rr;
                std::string xl = rl;

                std::string suf = isF32 ? "ss" : "sd";

                if (!t1.isFloat)
                {
                    xl = intToXmm(rl, tr.bits);
                }

                if (!t2.isFloat)
                {
                    xr = intToXmm(rr, tr.bits);
                }

                if (op == "+")
                    out << "    add" << suf << " %" << xr << ", %" << xl << "\n";
                else if (op == "-")
                    out << "    sub" << suf << " %" << xr << ", %" << xl << "\n";
                else if (op == "*")
                    out << "    mul" << suf << " %" << xr << ", %" << xl << "\n";
                else if (op == "/")
                    out << "    div" << suf << " %" << xr << ", %" << xl << "\n";
                else if (assembly_operations.count(op))
                {
                    out << "    ucomi" << suf << " %" << xr << ", %" << xl << "\n"
                        << "    " << assembly_operations[op] << " %al\n";
                    auto r_bool = alloc.allocate();
                    out << "    movzbq %al, %" << r_bool << "\n";
                    noteType(r_bool, node->scope->lookupType("boolean"));
                    alloc.freeXMM(xr);
                    alloc.freeXMM(xl);
                    return r_bool;
                }
                else
                {
                    throw std::runtime_error("Unsupported FP op " + op);
                }

                noteType(xl, tr);
                alloc.freeXMM(xr);
                return xl;
            }

            // --- Integer path ---
            char suf = getIntSuffix(tr.bits);
            auto r_l = adjustReg(rl, tr.bits);
            auto r_r = adjustReg(rr, tr.bits);

            if (op == "+")
            {
                out << "    add" << suf << " %" << r_r << ", %" << r_l << "\n";
            }
            else if (op == "-")
            {
                out << "    sub" << suf << " %" << r_r << ", %" << r_l << "\n";
            }
            else if (op == "*")
            {
                out << "    imul" << suf << " %" << r_r << ", %" << r_l << "\n";
            }
            else if (op == "/")
            {
                // Signed division
                if (tr.bits == 32)
                {
                    // dividend in EAX, sign‑extend to EDX:EAX
                    out << "    movl %" << r_l << ", %eax\n"
                                                  "    cltd\n"
                                                  "    idivl %"
                        << r_r << "\n"
                                  "    movl %eax, %"
                        << r_l << "\n";
                }
                else
                {
                    // dividend in RAX, sign‑extend to RDX:RAX
                    out << "    movq %" << r_l << ", %rax\n"
                                                  "    cqo\n"
                                                  "    idivq %"
                        << r_r << "\n"
                                  "    movq %rax, %"
                        << r_l << "\n";
                }
                alloc.free(rr);
                noteType(r_l, tr);
                return rl;
            }
            else if (assembly_operations.count(op))
            {
                out << "    cmp" << suf << " %" << r_r << ", %" << r_l << "\n"
                    << "    " << assembly_operations[op] << " %al\n"
                    << "    movzbq %al, %" << r_l << "\n";
                alloc.free(rr);
                noteType(rl, node->scope->lookupType("boolean"));
                return rl;
            }
            else if (op == "&&" || op == "||" || op == "&" || op == "|")
            {
                std::string instr;
                if (op == "&&")
                    instr = "and";
                else if (op == "||")
                    instr = "or";
                else
                    instr = op; // "&" or "|"
                out << "    " << instr << suf << " %" << r_r << ", %" << r_l << "\n";
            }
            else
            {
                throw std::runtime_error("Unsupported int op " + op);
            }

            alloc.free(rr);
            noteType(rl, tr);
            return rl;
        }

        // === BooleanLiteral & UnaryOp ===
        if (node->type == NT::BooleanLiteral)
        {
            auto r = alloc.allocate();
            out << "    movq $" << (node->value == "true" ? "1" : "0")
                << ", %" << r << "\n";
            noteType(r, node->scope->lookupType("boolean"));
            return r;
        }
        // --- UnaryOp ---
        if (node->type == NT::UnaryOp)
        {
            const auto &op = node->value;
            if (op == "!" || op == "++" || op == "--")
            {
                // Fetch the operand AST and its variable name (must be VariableAccess)
                const ASTNode *child = node->children[0].get();
                if (child->type != NodeType::VariableAccess)
                    throw std::runtime_error(op + " can only be applied to variables");

                std::string varName = child->value;
                auto &scp = *child->scope;
                TypeInfo ti = scp.lookupType(scp.lookupVariable(varName).type);

                if (op == "!")
                {
                    auto r = emitExpr(child, out, alloc);
                    out << "    cmpq $0, %" << r << "\n"
                                                    "    sete %al\n"
                                                    "    movzbq %al, %"
                        << r << "\n";
                    noteType(r, node->scope->lookupType("boolean"));
                    return r;
                }

                if (ti.isFloat)
                    throw std::runtime_error("Operator '" + op + "' not supported on float");

                uint64_t sz = ti.bits / 8;
                char suf = getIntSuffix(ti.bits);

                std::string r = alloc.allocate();
                std::string adj = adjustReg(r, ti.bits);
                if (scp.isGlobalVariable(varName))
                {
                    out << "    " << getCorrectMove(sz, /*isFloat=*/false)
                        << " " << varName << "(%rip), %" << adj << "\n";
                }
                else
                {
                    int64_t off = scp.getVariableOffset(varName);
                    out << "    " << getCorrectMove(sz, /*isFloat=*/false)
                        << " " << off << "(%rbp), %" << adj << "\n";
                }

                out << "    " << (op == "++" ? "inc" : "dec") << suf
                    << " %" << adj << "\n";

                if (scp.isGlobalVariable(varName))
                {
                    out << "    " << getCorrectMove(sz, /*isFloat=*/false)
                        << " %" << adj << ", " << varName << "(%rip)\n";
                }
                else
                {
                    int64_t off = scp.getVariableOffset(varName);
                    out << "    " << getCorrectMove(sz, /*isFloat=*/false)
                        << " %" << adj << ", " << off << "(%rbp)\n";
                }
                noteType(adj, {ti.bits, ti.align, /*isFloat=*/false, ti.isSigned});
                return adj;
            }
        }

        throw std::runtime_error("Unsupported expr");
    }

    char CodeGenLinux::getIntSuffix(uint64_t bits) const
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

    std::string CodeGenLinux::getCorrectMove(uint64_t s, bool f)
    {
        if (f)
        {
            if (s == 4)
                return "movss";
            else if (s == 8)
                return "movsd";
            else
                throw std::runtime_error("Bad float move size");
        }
        else
        {
            switch (s)
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
            throw std::runtime_error("Bad int move size");
        }
    }

    void CodeGenLinux::generateStatement(const ASTNode *s,
                                         std::ostream &out)
    {
        using NT = NodeType;

        if (s->type == NT::VariableReassignment)
        {
            auto &scp = *s->scope;
            auto nm = s->value;
            auto ti = scp.lookupType(scp.lookupVariable(nm).type);
            uint64_t sz = ti.bits / 8;
            auto r = emitExpr(s->children.back().get(), out, alloc);

            if (ti.isFloat)
            {
                out << "    " << (sz == 4 ? "movss %" : "movsd %") << r
                    << ", "
                    << (scp.isGlobalVariable(nm) ? nm + "(%rip)"
                                                 : std::to_string(scp.getVariableOffset(nm)) + "(%rbp)")
                    << "\n";
                alloc.freeXMM(r);
            }
            else
            {
                std::string adj = adjustReg(r, ti.bits);
                out << "    " << getCorrectMove(sz, false)
                    << " %" << adj << ", "
                    << (scp.isGlobalVariable(nm) ? nm + "(%rip)"
                                                 : std::to_string(scp.getVariableOffset(nm)) + "(%rbp)")
                    << "\n";
                alloc.free(r);
            }
            return;
        }

        if (s->type == NT::VariableDeclaration)
        {
            auto &scp = *s->scope;
            auto nm = s->value;
            auto ti = scp.lookupType(scp.lookupVariable(nm).type);
            uint64_t sz = ti.bits / 8;

            if (!scp.isGlobalVariable(nm))
            {
                out << "    # Making space for variable named: " << nm << "\n";
                out << "    subq $" << sz << ", %rsp\n";
                if (s->children.size() >= 2)
                {
                    auto r = emitExpr(s->children.back().get(), out, alloc);
                    if (ti.isFloat)
                    {
                        out << "    " << (sz == 4 ? "movss %" : "movsd %") << r
                            << ", "
                            << std::to_string(scp.getVariableOffset(nm)) + "(%rbp)"
                            << "\n";
                        alloc.freeXMM(r);
                    }
                    else
                    {
                        std::string adj = adjustReg(r, ti.bits);
                        out << "    " << getCorrectMove(sz, false)
                            << " %" << adj << ", "
                            << std::to_string(scp.getVariableOffset(nm)) + "(%rbp)"
                            << "\n";
                        alloc.free(r);
                    }
                }
                else
                {
                    if (ti.isFloat)
                    {
                        out << "    " << (sz == 4 ? "movss $0.0, (%rsp)" : "movsd $0.0, (%rsp)") << "\n";
                    }
                    else
                    {
                        out << "    " << getCorrectMove(sz, false) << " $0, (%rsp)" << "\n";
                    }
                }
            }
            else
            {
                if (s->children.size() >= 2)
                {
                    auto r = emitExpr(s->children.back().get(), out, alloc);
                    if (ti.isFloat)
                    {
                        out << "    " << (sz == 4 ? "movss %" : "movsd %") << r
                            << ", "
                            << nm + "(%rip)"
                            << "\n";
                        alloc.freeXMM(r);
                    }
                    else
                    {
                        std::string adj = adjustReg(r, ti.bits);
                        out << "    " << getCorrectMove(sz, false)
                            << " %" << adj << ", "
                            << nm + "(%rip)"
                            << "\n";
                        alloc.free(r);
                    }
                }
                else
                {
                    if (ti.isFloat)
                    {
                        out << "    " << (sz == 4 ? "movss $0.0, " + nm + "(%rip)" : "movsd $0.0, " + nm + "(%rip)") << "\n";
                    }
                    else
                    {
                        out << "    " << getCorrectMove(sz, false) << " $0, " + nm + "(%rip)" << "\n";
                    }
                }
            }

            return;
        }
        if (s->type == NT::IfStatement)
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
                out << "    # Block Starts (scope enter)\n";
                out << "    push   %rbp\n";
                out << "    mov    %rsp, %rbp\n";
                out << "    sub    $" << allocSize << ", %rsp\n";
            };
            auto emitEpilogue = [&]()
            {
                out << "    # Block Ends (scope exit)\n";
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
                    auto r2 = emitExpr(branch->children[0].get(), out, alloc);
                    out << "    cmpq $0, %" << r2 << "\n";
                    out << "    je   " << elseLbl << "\n";
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
} // namespace zlang
