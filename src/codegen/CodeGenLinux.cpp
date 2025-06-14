#include "all.hpp"

namespace zlang
{
    std::string CodeGenLinux::intToXmm(const std::string &register_int,
                                       uint32_t bits)
    {
        std::string r_xmm = alloc.allocateXMM();
        const char *cvt = (bits == 32 ? "cvtsi2ss" : "cvtsi2sd");
        out << "    " << cvt << " %" << register_int << ", %" << r_xmm << "\n";
        alloc.free(register_int);
        noteType(r_xmm, {bits, bits / 8, true, true});
        return r_xmm;
    }

    std::string
    CodeGenLinux::generateIntegerLiteral(std::unique_ptr<ASTNode> node)
    {
        auto r = alloc.allocate();
        out << "    movq $" << node->value << ", %" << r << "\n";
        noteType(r, node->scope->lookupType("int64_t"));
        return r;
    }
    std::string CodeGenLinux::generateFloatLiteral(std::unique_ptr<ASTNode> node)
    {
        std::string lbl = ".Lfloat" + std::to_string(floatLabelCount++);
        std::string val = node->value;
        bool isF32 = (!val.empty() && (val.back() == 'f' || val.back() == 'F'));
        if (isF32)
            val.pop_back();

        out << "# rodata\n"
               ".section .rodata\n"
            << lbl << ": ." << (isF32 ? "float" : "double") << " " << val
            << "\n"
               ".section .text\n";

        auto r_xmm = alloc.allocateXMM();
        out << "    " << (isF32 ? "movss " : "movsd ") << lbl << "(%rip), %"
            << r_xmm << "\n";
        noteType(r_xmm, (isF32 ? node->scope->lookupType("float")
                               : node->scope->lookupType("double")));
        return r_xmm;
    }
    std::string CodeGenLinux::generateStringLiteral(std::unique_ptr<ASTNode> node)
    {
        std::string lbl = ".Lstr" + std::to_string(stringLabelCount++);
        out << "# rodata\n"
               ".section .rodata\n"
            << lbl << ": .string \"" << node->value
            << "\"\n"
               ".section .text\n";
        auto r = alloc.allocate();
        out << "    leaq " << lbl << "(%rip), %" << r << "\n";
        noteType(r, node->scope->lookupType("string"));
        return r;
    }
    std::string
    CodeGenLinux::generateBooleanLiteral(std::unique_ptr<ASTNode> node)
    {
        auto r = alloc.allocate();
        out << "    movq $" << (node->value == "true" ? "1" : "0") << ", %" << r
            << "\n";
        noteType(r, node->scope->lookupType("boolean"));
        return r;
    }
    std::string
    CodeGenLinux::generateVariableAccess(std::unique_ptr<ASTNode> node)
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
                out << "    " << (sz == 4 ? "movss " : "movsd ") << name
                    << "(%rip), %" << r_xmm << "\n";
                noteType(r_xmm, ti);
                return r_xmm;
            }
            else
            {
                std::string r = alloc.allocate();
                std::string adj = adjustReg(r, ti.bits);
                out << "    " << getCorrectMove(sz, false) << " " << name
                    << "(%rip), %" << adj << "\n";
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
                out << "    " << (sz == 4 ? "movss " : "movsd ") << off
                    << "(%rbp), %" << r << "\n";
                noteType(r, (sz == 4 ? node->scope->lookupType("float")
                                     : node->scope->lookupType("double")));
                return r;
            }
            else
            {
                std::string r = alloc.allocate();
                std::string adj = adjustReg(r, ti.bits);
                out << "    " << getCorrectMove(sz, false) << " " << off
                    << "(%rbp), %" << adj << "\n";
                noteType(r, ti);
                return r;
            }
        }
    }
    std::string
    CodeGenLinux::generateBinaryOperation(std::unique_ptr<ASTNode> node)
    {
        auto rl = emitExpression(std::move(node->children[0]));
        auto rr = emitExpression(std::move(node->children[1]));

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
            else if (assembly_comparison_operations.count(op))
            {
                out << "    ucomi" << suf << " %" << xr << ", %" << xl << "\n"
                    << "    " << assembly_comparison_operations[op] << " %al\n";
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
        char suf = integer_suffixes[tr.bits];
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
                out << "    movl %" << r_l
                    << ", %eax\n"
                       "    cltd\n"
                       "    idivl %"
                    << r_r
                    << "\n"
                       "    movl %eax, %"
                    << r_l << "\n";
            }
            else
            {
                // dividend in RAX, sign‑extend to RDX:RAX
                out << "    movq %" << r_l
                    << ", %rax\n"
                       "    cqo\n"
                       "    idivq %"
                    << r_r
                    << "\n"
                       "    movq %rax, %"
                    << r_l << "\n";
            }
            alloc.free(rr);
            noteType(r_l, tr);
            return rl;
        }
        else if (assembly_comparison_operations.count(op))
        {
            out << "    cmp" << suf << " %" << r_r << ", %" << r_l << "\n"
                << "    " << assembly_comparison_operations[op] << " %al\n"
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
    std::string
    CodeGenLinux::generateUnaryOperation(std::unique_ptr<ASTNode> node)
    {
        const auto &op = node->value;
        if (op == "!" || op == "++" || op == "--")
        {
            // Fetch the operand AST and its variable name (must be VariableAccess)
            std::unique_ptr<ASTNode> child = std::move(node->children[0]);
            if (child->type != NodeType::VariableAccess)
                throw std::runtime_error(op + " can only be applied to variables");

            std::string varName = child->value;
            auto &scp = *child->scope;
            TypeInfo ti = scp.lookupType(scp.lookupVariable(varName).type);

            if (op == "!")
            {
                auto r = emitExpression(std::move(child));
                out << "    cmpq $0, %" << r
                    << "\n"
                       "    sete %al\n"
                       "    movzbq %al, %"
                    << r << "\n";
                noteType(r, node->scope->lookupType("boolean"));
                return r;
            }

            if (ti.isFloat)
                throw std::runtime_error("Operator '" + op +
                                         "' not supported on float");

            uint64_t sz = ti.bits / 8;
            char suf = integer_suffixes[ti.bits];

            std::string r = alloc.allocate();
            std::string adj = adjustReg(r, ti.bits);
            if (scp.isGlobalVariable(varName))
            {
                out << "    " << getCorrectMove(sz, /*isFloat=*/false) << " "
                    << varName << "(%rip), %" << adj << "\n";
            }
            else
            {
                int64_t off = scp.getVariableOffset(varName);
                out << "    " << getCorrectMove(sz, /*isFloat=*/false) << " " << off
                    << "(%rbp), %" << adj << "\n";
            }

            out << "    " << (op == "++" ? "inc" : "dec") << suf << " %" << adj
                << "\n";

            if (scp.isGlobalVariable(varName))
            {
                out << "    " << getCorrectMove(sz, /*isFloat=*/false) << " %"
                    << adj << ", " << varName << "(%rip)\n";
            }
            else
            {
                int64_t off = scp.getVariableOffset(varName);
                out << "    " << getCorrectMove(sz, /*isFloat=*/false) << " %"
                    << adj << ", " << off << "(%rbp)\n";
            }
            noteType(adj, {ti.bits, ti.align, /*isFloat=*/false, ti.isSigned});
            return adj;
        }
        throw std::runtime_error("Unknown Unary Operator");
    }

    std::string CodeGenLinux::emitExpression(std::unique_ptr<ASTNode> node)
    {
        switch (node->type)
        {
        case NodeType::IntegerLiteral:
            return generateIntegerLiteral(std::move(node));
        case NodeType::FloatLiteral:
            return generateFloatLiteral(std::move(node));
        case NodeType::StringLiteral:
            return generateStringLiteral(std::move(node));
        case NodeType::BooleanLiteral:
            return generateBooleanLiteral(std::move(node));
        case NodeType::VariableAccess:
            return generateVariableAccess(std::move(node));
        case NodeType::BinaryOp:
            return generateBinaryOperation(std::move(node));
        case NodeType::UnaryOp:
            return generateUnaryOperation(std::move(node));
        default:
            throw std::runtime_error("Unknown statement encountered.");
        }
    }

    void CodeGenLinux::emitEpilogue()
    {
        out << "    # Block Ends (scope exit)\n";
        out << "    leave\n";
    }
    void CodeGenLinux::emitPrologue(std::unique_ptr<ASTNode> blockNode)
    {
        auto blkScope = blockNode->scope;
        int allocSize = -blkScope->stackOffset + 8;
        // +8 to cover the pushed RBP
        out << "    # Block Starts (scope enter)\n";
        out << "    push   %rbp\n";
        out << "    mov    %rsp, %rbp\n";
        out << "    sub    $" << allocSize << ", %rsp\n";
    }

    void CodeGenLinux::generateStatement(std::unique_ptr<ASTNode> statement)
    {
        switch (statement->type)
        {
        case NodeType::VariableReassignment:
        {
            generateVariableReassignment(std::move(statement));
            break;
        }
        case NodeType::VariableDeclaration:
        {
            generateVariableDeclaration(std::move(statement));
            break;
        }
        case NodeType::IfStatement:
        {
            generateIfStatement(std::move(statement));
            break;
        }
        default:
            throw std::runtime_error("Unknown statement encountered.");
        }
    }

    void CodeGenLinux::generateVariableReassignment(
        std::unique_ptr<ASTNode> statement)
    {
        logMessage("Inside Reassignment");
        auto &scp = *statement->scope;
        auto nm = statement->value;
        auto ti = scp.lookupType(scp.lookupVariable(nm).type);
        uint64_t sz = ti.bits / 8;
        auto r = emitExpression(std::move(statement->children.back()));

        if (ti.isFloat)
        {
            out << "    " << (sz == 4 ? "movss %" : "movsd %") << r << ", "
                << (scp.isGlobalVariable(nm)
                        ? nm + "(%rip)"
                        : std::to_string(scp.getVariableOffset(nm)) + "(%rbp)")
                << "\n";
            alloc.freeXMM(r);
        }
        else
        {
            std::string adj = adjustReg(r, ti.bits);
            out << "    " << getCorrectMove(sz, false) << " %" << adj << ", "
                << (scp.isGlobalVariable(nm)
                        ? nm + "(%rip)"
                        : std::to_string(scp.getVariableOffset(nm)) + "(%rbp)")
                << "\n";
            alloc.free(r);
        }
    }
    void CodeGenLinux::generateVariableDeclaration(
        std::unique_ptr<ASTNode> statement)
    {
        auto &scp = *statement->scope;
        auto nm = statement->value;
        auto ti = scp.lookupType(scp.lookupVariable(nm).type);
        uint64_t sz = ti.bits / 8;

        if (!scp.isGlobalVariable(nm))
        {
            out << "    # Making space for variable named: " << nm << "\n";
            out << "    subq $" << sz << ", %rsp\n";
            if (statement->children.size() >= 2)
            {
                auto r = emitExpression(std::move(statement->children.back()));
                if (ti.isFloat)
                {
                    out << "    " << (sz == 4 ? "movss %" : "movsd %") << r << ", "
                        << std::to_string(scp.getVariableOffset(nm)) + "(%rbp)"
                        << "\n";
                    alloc.freeXMM(r);
                }
                else
                {
                    std::string adj = adjustReg(r, ti.bits);
                    out << "    " << getCorrectMove(sz, false) << " %" << adj
                        << ", "
                        << std::to_string(scp.getVariableOffset(nm)) + "(%rbp)"
                        << "\n";
                    alloc.free(r);
                }
            }
            else
            {
                if (ti.isFloat)
                {
                    out << "    "
                        << (sz == 4 ? "movss $0.0, (%rsp)" : "movsd $0.0, (%rsp)")
                        << "\n";
                }
                else
                {
                    out << "    " << getCorrectMove(sz, false) << " $0, (%rsp)"
                        << "\n";
                }
            }
        }
        else
        {
            if (statement->children.size() >= 2)
            {
                auto r = emitExpression(std::move(statement->children.back()));
                if (ti.isFloat)
                {
                    out << "    " << (sz == 4 ? "movss %" : "movsd %") << r << ", "
                        << nm + "(%rip)" << "\n";
                    alloc.freeXMM(r);
                }
                else
                {
                    std::string adj = adjustReg(r, ti.bits);
                    out << "    " << getCorrectMove(sz, false) << " %" << adj
                        << ", " << nm + "(%rip)" << "\n";
                    alloc.free(r);
                }
            }
            else
            {
                if (ti.isFloat)
                {
                    out << "    "
                        << (sz == 4 ? "movss $0.0, " + nm + "(%rip)"
                                    : "movsd $0.0, " + nm + "(%rip)")
                        << "\n";
                }
                else
                {
                    out << "    " << getCorrectMove(sz, false)
                        << " $0, " + nm + "(%rip)" << "\n";
                }
            }
        }

        return;
    }
    void CodeGenLinux::generateIfStatement(std::unique_ptr<ASTNode> statement)
    {
        int id = blockLabelCount++;
        std::string elseLbl = ".Lelse" + std::to_string(id);
        std::string endLbl = ".Lend" + std::to_string(id);
        auto condR = emitExpression(std::move(statement->children[0]));
        out << "    cmpq $0, %" << condR << "\n";
        out << "    je " << elseLbl << "\n";
        alloc.free(condR);
        std::unique_ptr<ASTNode> ifBlock = std::move(statement->children[1]);
        std::vector<std::unique_ptr<zlang::ASTNode>> children = std::move(ifBlock->children);
        emitPrologue(std::move(ifBlock));
        for (auto &stmt : children)
        {
            generateStatement(std::move(stmt));
        }
        emitEpilogue();
        out << "    jmp   " << endLbl << "\n";
        out << elseLbl << ":\n";
        ASTNode *branch = statement->getElseBranch();
        while (branch)
        {
            if (branch->type == NodeType::ElseIfStatement)
            {
                auto r2 = emitExpression(std::move(branch->children[0]));
                out << "    cmpq $0, %" << r2 << "\n";
                out << "    je   " << elseLbl << "\n";
                alloc.free(r2);

                std::unique_ptr<ASTNode> elifBlock = std::move(branch->children[1]);
                std::vector<std::unique_ptr<zlang::ASTNode>> children = std::move(elifBlock->children);
                emitPrologue(std::move(elifBlock));
                for (auto &stmt : children)
                    generateStatement(std::move(stmt));
                emitEpilogue();
                out << "    jmp   " << endLbl << "\n";

                branch = branch->getElseBranch();
            }
            else if (branch->type == NodeType::ElseStatement)
            {
                std::unique_ptr<ASTNode> elseBlock = std::move(branch->children[0]);
                std::vector<std::unique_ptr<zlang::ASTNode>> children = std::move(elseBlock->children);
                emitPrologue(std::move(elseBlock));
                for (auto &stmt : children)
                    generateStatement(std::move(stmt));
                emitEpilogue();
                break;
            }
        }
        out << endLbl << ":\n\n";
        return;
    }

    void CodeGenLinux::generate(std::unique_ptr<ASTNode> program)
    {
        std::vector<ASTNode *> globals;
        for (auto &statement : program->children)
            if (statement->type == NodeType::VariableDeclaration)
                globals.push_back(statement.get());
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

        for (std::unique_ptr<ASTNode> &statement : program.get()->children)
        {
            generateStatement(std::move(statement));
        }

        out << "    movq $60, %rax\n    movq $0, %rdi\n    syscall\n";
    }
} // namespace zlang