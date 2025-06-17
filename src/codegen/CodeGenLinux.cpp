#include "all.hpp"

namespace zlang
{
    std::string CodeGenLinux::castValue(
        const std::string &val,
        const TypeInfo &fromType,
        const TypeInfo &toType)
    {
        if (fromType.bits == toType.bits && fromType.isFloat == toType.isFloat)
        {
            if (!fromType.isFloat && fromType.isSigned != toType.isSigned)
            {
                std::string dst = alloc.allocate();
                // AT&T: source, destination
                out << "    mov %" << adjustReg(val, fromType.bits)
                    << ", %" << adjustReg(dst, fromType.bits) << "\n";
                alloc.free(val);
                noteType(dst, toType);
                return dst;
            }
            return RegisterAllocator::getBaseReg(val);
        }

        if (fromType.isFloat && toType.isFloat)
        {
            std::string dstX = alloc.allocateXMM();
            auto instr = (fromType.bits < toType.bits) ? "cvtss2sd" : "cvtsd2ss";
            // AT&T: source, destination
            out << "    " << instr << " %" << val << ", %" << dstX << "\n";
            alloc.free(val);
            noteType(dstX, toType);
            return dstX;
        }

        if (!fromType.isFloat && toType.isFloat)
        {
            std::string dstX = alloc.allocateXMM();
            auto instr = (toType.bits == 32) ? "cvtsi2ss" : "cvtsi2sd";
            out << "    " << instr << " %" << adjustReg(val, fromType.bits)
                << ", %" << dstX << "\n";
            alloc.free(val);
            noteType(dstX, toType);
            return dstX;
        }

        if (fromType.isFloat && !toType.isFloat)
        {
            std::string dstG = alloc.allocate();
            auto instr = (fromType.bits == 32) ? "cvttss2si" : "cvttsd2si";
            out << "    " << instr << " %" << val
                << ", %" << adjustReg(dstG, toType.bits) << "\n";
            alloc.free(val);
            noteType(dstG, toType);
            return dstG;
        }

        // Integer casts
        if (!fromType.isFloat && !toType.isFloat)
        {
            std::string dstG = alloc.allocate();
            std::string srcAdj = adjustReg(val, fromType.bits);
            std::string dstAdj = adjustReg(dstG, toType.bits);

            if (toType.bits > fromType.bits)
            {
                // Extension
                if (fromType.isSigned)
                {
                    if (fromType.bits == 32 && toType.bits == 64)
                    {
                        // AT&T: source, destination
                        out << "    movsxd %" << srcAdj << ", %" << dstAdj << "\n";
                    }
                    else
                    {
                        // movsx for 8/16 -> larger
                        out << "    movsx %" << srcAdj << ", %" << dstAdj << "\n";
                    }
                }
                else
                {
                    if (fromType.bits == 8 || fromType.bits == 16)
                    {
                        // AT&T: source, destination
                        out << "    movzx %" << srcAdj << ", %" << dstAdj << "\n";
                    }
                    else
                    {
                        // 32->64: normal mov zero-extends
                        out << "    movl %" << adjustReg(val, 32)
                            << ", %" << adjustReg(dstG, 32) << "\n";
                    }
                }
            }
            else if (toType.bits < fromType.bits)
            {
                // Truncation: move smaller portion
                out << "    mov %" << adjustReg(val, toType.bits)
                    << ", %" << dstAdj << "\n";
            }
            else
            {
                // Same size: simple move
                out << "    mov %" << srcAdj << ", %" << dstAdj << "\n";
            }

            alloc.free(val);
            noteType(dstG, toType);
            return dstG;
        }

        throw std::runtime_error(
            "Unsupported cast: from " + fromType.to_string() +
            " to " + toType.to_string());
    }

    std::string CodeGenLinux::generateIntegerLiteral(std::unique_ptr<ASTNode> node)
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

        outGlobal << lbl << ": ." << (isF32 ? "float" : "double") << " " << val << "\n";

        auto r_xmm = alloc.allocateXMM();
        out << "    " << (isF32 ? "movss " : "movsd ") << lbl << "(%rip), %" << r_xmm << "\n";
        noteType(r_xmm, (isF32 ? node->scope->lookupType("float")
                               : node->scope->lookupType("double")));
        return r_xmm;
    }
    std::string CodeGenLinux::generateStringLiteral(std::unique_ptr<ASTNode> node)
    {
        std::string lbl = ".Lstr" + std::to_string(stringLabelCount++);

        std::string str = node->value;

        outGlobal << lbl << ": .string \"" << str << "\"\n";

        auto r = alloc.allocate();
        out << "    leaq " << lbl << "(%rip), %" << r << "\n";
        noteType(r, node->scope->lookupType("string"));
        return r;
    }
    std::string CodeGenLinux::generateBooleanLiteral(std::unique_ptr<ASTNode> node)
    {
        auto r = alloc.allocate();
        out << "    movq $" << (node->value == "true" ? "1" : "0") << ", %" << r
            << "\n";
        noteType(r, node->scope->lookupType("boolean"));
        return r;
    }
    std::string CodeGenLinux::generateVariableAccess(std::unique_ptr<ASTNode> node)
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
    std::string CodeGenLinux::generateBinaryOperation(std::unique_ptr<ASTNode> node)
    {
        auto rl = emitExpression(std::move(node->children[0]));
        auto rr = emitExpression(std::move(node->children[1]));

        auto t1 = regType.at(rl);
        auto t2 = regType.at(rr);
        auto tr = TypeChecker::promoteType(t1, t2);
        const auto &op = node->value;

        rl = castValue(rl, t1, tr);
        rr = castValue(rr, t2, tr);

        if (tr.isFloat)
        {
            bool isF32 = (tr.bits == 32);
            std::string vsuf = isF32 ? "ss" : "sd";
            std::string xl = rl, xr = rr;

            if (op == "+")
                out << "    add" << vsuf << " %" << xr << ", %" << xl << "\n";
            else if (op == "-")
                out << "    sub" << vsuf << " %" << xr << ", %" << xl << "\n";
            else if (op == "*")
                out << "    mul" << vsuf << " %" << xr << ", %" << xl << "\n";
            else if (op == "/")
                out << "    div" << vsuf << " %" << xr << ", %" << xl << "\n";
            else if (assembly_comparison_operations.count(op))
            {
                out << "    ucomi" << vsuf << " %" << xr << ", %" << xl << "\n"
                    << "    " << assembly_comparison_operations.at(op) << " %al\n";
                auto r_bool = alloc.allocate();
                out << "    movzbq %al, %" << r_bool << "\n";
                noteType(r_bool, node->scope->lookupType("boolean"));
                alloc.free(xr);
                alloc.free(xl);
                return r_bool;
            }
            else
            {
                throw std::runtime_error("Unsupported FP op " + op);
            }

            noteType(xl, tr);
            alloc.free(xr);
            return xl;
        }

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
            if (tr.isSigned)
            {
                if (tr.bits == 32)
                {
                    out << "    movl %" << r_l << ", %eax\n"
                        << "    cltd\n"
                        << "    idivl %" << r_r << "\n"
                        << "    movl %eax, %" << r_l << "\n";
                }
                else
                {
                    out << "    movq %" << r_l << ", %rax\n"
                        << "    cqo\n"
                        << "    idivq %" << r_r << "\n"
                        << "    movq %rax, %" << r_l << "\n";
                }
            }
            else
            {
                if (tr.bits == 32)
                {
                    out << "    movl %" << r_l << ", %eax\n"
                        << "    clrl %edx\n"
                        << "    divl %" << r_r << "\n"
                        << "    movl %eax, %" << r_l << "\n";
                }
                else
                {
                    out << "    movq %" << r_l << ", %rax\n"
                        << "    xorq %rdx, %rdx\n"
                        << "    divq %" << r_r << "\n"
                        << "    movq %rax, %" << r_l << "\n";
                }
            }
            alloc.free(rr);
            noteType(r_l, tr);
            return rl;
        }
        else if (assembly_comparison_operations.count(op))
        {
            static const std::unordered_map<std::string, std::string> signedMap = {
                {"==", "sete"}, {"!=", "setne"}, {"<", "setl"}, {"<=", "setle"}, {">", "setg"}, {">=", "setge"}};
            static const std::unordered_map<std::string, std::string> unsignedMap = {
                {"==", "sete"}, {"!=", "setne"}, {"<", "setb"}, {"<=", "setbe"}, {">", "seta"}, {">=", "setae"}};
            auto &map = tr.isSigned ? signedMap : unsignedMap;
            auto r_bool = alloc.allocate();
            out << "    cmp" << suf << " %" << r_r << ", %" << r_l << "\n"
                << "    " << map.at(op) << " %al\n"
                << "    movzbq %al, %" << r_bool << "\n";
            alloc.free(rr);
            alloc.free(rl);
            noteType(r_bool, node->scope->lookupType("boolean"));
            return r_bool;
        }
        else if (op == "&&" || op == "||" || op == "&" || op == "|")
        {
            std::string instr;
            if (op == "&&")
                instr = "and";
            else if (op == "||")
                instr = "or";
            else
                instr = op;
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
    std::string CodeGenLinux::generateUnaryOperation(std::unique_ptr<ASTNode> node)
    {
        const auto &op = node->value;
        if (op != "!" && op != "++" && op != "--")
            throw std::runtime_error("Unsupported unary operator: " + op);

        auto child = std::move(node->children[0]);

        const std::string varName = child->value;
        auto &scp = *child->scope;
        TypeInfo ti = scp.lookupType(scp.lookupVariable(varName).type);

        if (op == "!")
        {
            auto r = emitExpression(std::move(child));
            TypeInfo boolType = node->scope->lookupType("boolean");
            r = castValue(r, regType.at(r), boolType);
            std::string res = alloc.allocate();
            out << "    cmpq $0, %" << r << "\n"
                << "    sete %al\n"
                << "    movzbq %al, %" << res << "\n";
            noteType(res, boolType);
            return res;
        }

        if (child->type != NodeType::VariableAccess)
            throw std::runtime_error(op + " can only be applied to variables");
        if (ti.isFloat)
            throw std::runtime_error(op + " not supported on float");

        std::string ptr = scp.isGlobalVariable(varName)
                              ? varName + "(%rip)"
                              : std::to_string(scp.getVariableOffset(varName)) + "(%rbp)";
        uint64_t sz = ti.bits / 8;
        char suf = integer_suffixes[ti.bits];

        std::string r = alloc.allocate();
        std::string adj = adjustReg(r, ti.bits);
        out << "    " << getCorrectMove(sz, /*isFloat=*/false)
            << " " << ptr << ", %" << adj << "\n";

        out << "    " << (op == "++" ? "inc" : "dec") << suf
            << " %" << adj << "\n";

        out << "    " << getCorrectMove(sz, /*isFloat=*/false)
            << " %" << adj << ", " << ptr << "\n";

        noteType(adj, ti);
        return adj;
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
        case NodeType::UnaryOp:
        {
            if (statement->value == "--" or statement->value == "++")
            {
                std::string reg = emitExpression(std::move(statement));
                alloc.free(reg);
            }
            break;
        }
        case NodeType::BinaryOp:
        {
            std::string reg = emitExpression(std::move(statement)); // I am doing this just so the increments/decrements work in x + y-- -> this itself must not have any result, but y-- should still be effective.
            alloc.free(reg);
            break;
        }
        default:
            throw std::runtime_error("Unknown statement encountered.");
        }
    }
    void CodeGenLinux::generateVariableReassignment(std::unique_ptr<ASTNode> statement)
    {
        auto &scp = *statement->scope;
        auto nm = statement->value;
        auto ti = scp.lookupType(scp.lookupVariable(nm).type);
        uint64_t sz = ti.bits / 8;

        auto r = emitExpression(std::move(statement->children.back()));
        // Ensure the result is cast to the target variable's type
        r = castValue(r, regType.at(r), ti);

        std::string addr = scp.isGlobalVariable(nm)
                               ? nm + "(%rip)"
                               : std::to_string(scp.getVariableOffset(nm)) + "(%rbp)";

        if (ti.isFloat)
        {
            out << "    " << (sz == 4 ? "movss %" : "movsd %") << r << ", " << addr << "\n";
        }
        else
        {
            std::string adj = adjustReg(r, ti.bits);
            out << "    " << getCorrectMove(sz, false) << " %" << adj << ", " << addr << "\n";
        }

        alloc.free(r);
    }
    void CodeGenLinux::generateVariableDeclaration(std::unique_ptr<ASTNode> statement)
    {
        auto &scp = *statement->scope;
        auto nm = statement->value;
        auto ti = scp.lookupType(scp.lookupVariable(nm).type);
        uint64_t sz = ti.bits / 8;

        if (!scp.isGlobalVariable(nm))
        {
            if (statement->children.size() >= 2)
            {
                auto r = emitExpression(std::move(statement->children.back()));
                r = castValue(r, regType.at(r), ti); // <- Added casting

                if (ti.isFloat)
                {
                    out << "    " << (sz == 4 ? "movss %" : "movsd %") << r << ", "
                        << std::to_string(scp.getVariableOffset(nm)) + "(%rbp)"
                        << "\n";
                }
                else
                {
                    std::string adj = adjustReg(r, ti.bits);
                    out << "    " << getCorrectMove(sz, false) << " %" << adj << ", "
                        << std::to_string(scp.getVariableOffset(nm)) + "(%rbp)"
                        << "\n";
                }

                alloc.free(r);
            }
            else
            {
                if (ti.isFloat)
                {
                    std::string r_xmm = alloc.allocateXMM();
                    out << "    pxor %" << r_xmm << ", %" << r_xmm << "\n";
                    out << "    " << (sz == 4 ? "movss %" : "movsd %") << r_xmm << ", "
                        << std::to_string(scp.getVariableOffset(nm)) + "(%rbp)"
                        << "\n";
                    alloc.free(r_xmm);
                }
                else
                {
                    out << "    xorq %rax, %rax\n";
                    out << "    mov" << integer_suffixes[ti.bits] << " %rax, "
                        << std::to_string(scp.getVariableOffset(nm)) + "(%rbp)"
                        << "\n";
                }
            }
        }
        else // Global variable path
        {
            if (statement->children.size() >= 2)
            {
                auto r = emitExpression(std::move(statement->children.back()));
                r = castValue(r, regType.at(r), ti); // <- Added casting

                if (ti.isFloat)
                {
                    out << "    " << (sz == 4 ? "movss %" : "movsd %") << r << ", "
                        << nm + "(%rip)"
                        << "\n";
                }
                else
                {
                    std::string adj = adjustReg(r, ti.bits);
                    out << "    " << getCorrectMove(sz, false) << " %" << adj << ", "
                        << nm + "(%rip)"
                        << "\n";
                }

                alloc.free(r);
            }
            else
            {
                if (ti.isFloat)
                {
                    std::string r_xmm = alloc.allocateXMM();
                    out << "    pxor %" << r_xmm << ", %" << r_xmm << "\n";
                    out << "    " << (sz == 4 ? "movss %" : "movsd %") << r_xmm << ", "
                        << nm + "(%rip)"
                        << "\n";
                    alloc.free(r_xmm);
                }
                else
                {
                    out << "    xorq %rax, %rax\n";
                    out << "    mov" << integer_suffixes[ti.bits] << " %rax, "
                        << nm + "(%rip)"
                        << "\n";
                }
            }
        }
    }
    void CodeGenLinux::generateIfStatement(std::unique_ptr<ASTNode> statement)
    {
        int id = blockLabelCount++;
        std::string endLbl = ".Lend" + std::to_string(id);
        std::vector<std::string> elseLabels;

        // Precompute labels for each else-if / else branch
        ASTNode *branch = statement->getElseBranch();
        while (branch)
        {
            elseLabels.push_back(".Lelse" + std::to_string(blockLabelCount++));
            branch = branch->getElseBranch();
        }

        size_t elseIdx = 0;

        auto condR = emitExpression(std::move(statement->children[0]));
        out << "    cmpq $0, %" << condR << "\n";
        out << "    je " << (elseLabels.empty() ? endLbl : elseLabels[elseIdx]) << "\n";
        alloc.free(condR);

        auto ifBlock = std::move(statement->children[1]);
        auto children = std::move(ifBlock->children);
        emitPrologue(std::move(ifBlock));
        for (auto &stmt : children)
            generateStatement(std::move(stmt));
        emitEpilogue();
        out << "    jmp " << endLbl << "\n";

        branch = statement->getElseBranch();
        while (branch)
        {
            out << elseLabels[elseIdx++] << ":\n";

            if (branch->type == NodeType::ElseIfStatement)
            {
                auto r2 = emitExpression(std::move(branch->children[0]));
                out << "    cmpq $0, %" << r2 << "\n";
                out << "    je " << (elseIdx < elseLabels.size() ? elseLabels[elseIdx] : endLbl) << "\n";
                alloc.free(r2);

                auto elifBlock = std::move(branch->children[1]);
                auto children = std::move(elifBlock->children);
                emitPrologue(std::move(elifBlock));
                for (auto &stmt : children)
                    generateStatement(std::move(stmt));
                emitEpilogue();
                out << "    jmp " << endLbl << "\n";
            }
            else if (branch->type == NodeType::ElseStatement)
            {
                auto elseBlock = std::move(branch->children[0]);
                auto children = std::move(elseBlock->children);
                emitPrologue(std::move(elseBlock));
                for (auto &stmt : children)
                    generateStatement(std::move(stmt));
                emitEpilogue();
            }

            branch = branch->getElseBranch();
        }

        out << endLbl << ":\n\n";
    }
    void CodeGenLinux::generate(std::unique_ptr<ASTNode> program)
    {
        std::vector<ASTNode *> globals;
        for (auto &statement : program->children)
            if (statement->type == NodeType::VariableDeclaration)
                globals.push_back(statement.get());
        outGlobal << ".data\n\n";
        for (auto &g : globals)
        {
            TypeInfo info = g->scope->lookupType(g->children[0]->value);
            switch (info.bits / 8)
            {
            case 8:
                outGlobal << g->value << ": .quad 0\n";
                break;
            case 4:
                outGlobal << g->value << ": .long 0\n";
                break;
            case 2:
                outGlobal << g->value << ": .word 0\n";
                break;
            case 1:
                outGlobal << g->value << ": .byte 0\n";
                break;
            default:
                throw std::runtime_error("Unsupported global size");
            }
        }
        outGlobal << "\n.section .rodata\n"; // Ensure rodata section exists
        out << ".text\n.global _start\n_start:\n\n";
        for (std::unique_ptr<ASTNode> &statement : program.get()->children)
        {
            generateStatement(std::move(statement));
        }

        out << "    movq $60, %rax\n    movq $0, %rdi\n    syscall\n";

        outfinal << outGlobal.str() + "\n# ==============Globals End Here==============\n"
                 << out.str() << "\n\n";
    }
} // namespace zlang