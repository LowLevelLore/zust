#include "all.hpp"

namespace zlang
{
    std::string CodeGenWindows::castValue(const std::string &srcReg, const TypeInfo &fromType, const TypeInfo &toType)
    {
        if (fromType.bits == toType.bits && fromType.isFloat == toType.isFloat)
        {
            if (!fromType.isFloat && fromType.bits == toType.bits &&
                fromType.isSigned != toType.isSigned)
            {
                std::string dst = alloc.allocate();
                out << "    mov " << adjustReg(dst, fromType.bits)
                    << ", " << adjustReg(srcReg, fromType.bits) << "\n";
                alloc.free(srcReg);
                noteType(dst, toType);
                return dst;
            }
            return RegisterAllocator::getBaseReg(srcReg);
        }

        if (fromType.isFloat && toType.isFloat)
        {
            std::string dst = alloc.allocateXMM();
            auto instr = (fromType.bits < toType.bits)
                             ? "cvtss2sd"
                             : "cvtsd2ss";
            out << "    " << instr << " " << dst << ", " << srcReg << "\n";
            alloc.free(srcReg);
            noteType(dst, toType);
            return dst;
        }

        if (!fromType.isFloat && toType.isFloat)
        {
            std::string dst = alloc.allocateXMM();
            auto instr = (toType.bits == 32)
                             ? "cvtsi2ss"
                             : "cvtsi2sd";
            out << "    " << instr << " " << dst
                << ", " << adjustReg(srcReg, fromType.bits) << "\n";
            alloc.free(srcReg);
            noteType(dst, toType);
            return dst;
        }

        if (fromType.isFloat && !toType.isFloat)
        {
            std::string dst = alloc.allocate();
            auto instr = (fromType.bits == 32)
                             ? "cvttss2si"
                             : "cvttsd2si";
            out << "    " << instr << " " << dst << ", " << srcReg << "\n";
            alloc.free(srcReg);
            noteType(dst, toType);
            return dst;
        }

        if (!fromType.isFloat && !toType.isFloat)
        {
            std::string dst = alloc.allocate();
            std::string srcAdj = adjustReg(srcReg, fromType.bits);
            std::string dstAdj = dst;

            if (toType.bits > fromType.bits)
            {
                if (fromType.isSigned)
                {
                    if (fromType.bits == 32 && toType.bits == 64)
                    {
                        out << "    movsxd " << dstAdj << ", " << srcAdj << "\n";
                    }
                    else
                    {
                        out << "    movsx " << dstAdj << ", " << srcAdj << "\n";
                    }
                }
                else
                {
                    if (fromType.bits == 8 || fromType.bits == 16)
                    {
                        out << "    movzx " << dstAdj << ", " << srcAdj << "\n";
                    }
                    else if (fromType.bits == 32 && toType.bits == 64)
                    {
                        out << "    mov " << dstAdj << "d, " << srcAdj << "\n";
                    }
                    else
                    {
                        throw std::runtime_error("Unsupported unsigned cast: " + std::to_string(fromType.bits) + " -> " + std::to_string(toType.bits));
                    }
                }
            }
            else if (toType.bits < fromType.bits)
            {
                if (toType.bits == 8 || toType.bits == 16)
                {
                    if (toType.isSigned)
                    {
                        out << "    movsx " << dstAdj << ", " << adjustReg(srcReg, toType.bits) << "\n";
                    }
                    else
                    {
                        out << "    movzx " << dstAdj << ", " << adjustReg(srcReg, toType.bits) << "\n";
                    }
                }
                else if (toType.bits == 32)
                {
                    out << "    mov " << adjustReg(dstAdj, 32) << ", " << adjustReg(srcReg, 32) << "\n";
                }
                else
                {
                    throw std::runtime_error("Unsupported downcast to " + std::to_string(toType.bits));
                }
            }
            else
            {
                out << "    mov " << adjustReg(dstAdj, toType.bits) << ", " << srcAdj << "\n";
            }

            alloc.free(srcReg);
            noteType(dst, toType);
            return dst;
        }

        throw std::runtime_error(
            "Unsupported cast from " +
            fromType.to_string() +
            " to " +
            toType.to_string());
    }

    std::string CodeGenWindows::getVariableAddress(const ScopeContext &scope, const std::string &name) const
    {
        if (scope.isGlobalVariable(name))
        {
            return "[" + name + "]";
        }
        else
        {
            int offset = scope.getVariableOffset(name);
            return (offset < 0)
                       ? "[rbp - " + std::to_string(-offset) + "]"
                       : "[rbp + " + std::to_string(offset) + "]";
        }
    }
    std::string CodeGenWindows::generateIntegerLiteral(std::unique_ptr<ASTNode> node)
    {
        TypeInfo ti = node->scope->lookupType("int64_t");
        auto r = alloc.allocate();
        std::string adj = adjustReg(r, ti.bits);

        out << "    mov " << adj << ", " << node->value << "\n";
        noteType(r, ti);

        return r;
    }
    std::string CodeGenWindows::generateFloatLiteral(std::unique_ptr<ASTNode> node)
    {
        std::string lbl = "Lfloat" + std::to_string(floatLabelCount++);
        std::string val = node->value;
        bool isF32 = (!val.empty() && (val.back() == 'f' || val.back() == 'F'));

        if (isF32)
            val.pop_back();

        if (isF32)
            outGlobal << lbl << " DWORD " << val << "\n";
        else
            outGlobal << lbl << " QWORD " << val << "\n";

        auto r_xmm = alloc.allocateXMM();

        out << "    " << (isF32 ? "movss" : "movsd")
            << " " << r_xmm << ", "
            << (isF32 ? "DWORD PTR " : "QWORD PTR ")
            << "[" << lbl << "]\n";

        noteType(r_xmm, node->scope->lookupType(isF32 ? "float" : "double"));

        return r_xmm;
    }
    std::string CodeGenWindows::generateStringLiteral(std::unique_ptr<ASTNode> node)
    {
        std::string lbl = "Lstr" + std::to_string(stringLabelCount++);
        outGlobal << lbl << " BYTE "
                  << '"' << ((node->value.length() == 0) ? "\\0" : node->value) << '"' << ", 0\n";
        auto r = alloc.allocate();
        out << "    lea " << r << ", [" << lbl << "]\n";
        noteType(r, node->scope->lookupType("string"));

        return r;
    }
    std::string CodeGenWindows::generateBooleanLiteral(std::unique_ptr<ASTNode> node)
    {
        auto r = alloc.allocate();
        out << "    mov " << r << ", " << (node->value == "true" ? "1" : "0") << "\n";
        noteType(r, node->scope->lookupType("boolean"));
        return r;
    }
    std::string CodeGenWindows::generateVariableAccess(std::unique_ptr<ASTNode> node)
    {
        auto &scope = *node->scope;
        const std::string name = node->value;
        TypeInfo ti = scope.lookupType(scope.lookupVariable(name).type);
        uint64_t sz = ti.bits / 8;
        uint64_t bits = ti.bits;

        std::string address = getVariableAddress(scope, name); // Use helper

        if (ti.isFloat)
        {
            auto r_xmm = alloc.allocateXMM();
            if (sz == 4)
            {
                out << "    movss " << r_xmm << ", DWORD PTR " << address << "\n";
            }
            else if (sz == 8)
            {
                out << "    movsd " << r_xmm << ", QWORD PTR " << address << "\n";
            }
            else
            {
                throw std::runtime_error("Unsupported float size: " + std::to_string(sz));
            }
            noteType(r_xmm, ti);
            return r_xmm;
        }
        else
        {
            auto r64 = alloc.allocate();
            auto r_adj = adjustReg(r64, bits);

            std::string ptrSize;
            switch (sz)
            {
            case 8:
                ptrSize = "QWORD PTR ";
                break;
            case 4:
                ptrSize = "DWORD PTR ";
                break;
            case 2:
                ptrSize = "WORD PTR ";
                break;
            case 1:
                ptrSize = "BYTE PTR ";
                break;
            default:
                throw std::runtime_error("Unsupported integer size: " + std::to_string(sz));
            }

            out << "    mov " << r_adj << ", " << ptrSize << address << "\n";
            noteType(r_adj, ti);
            return r_adj;
        }
    }
    std::string CodeGenWindows::generateBinaryOperation(std::unique_ptr<ASTNode> node)
    {
        std::string rl = emitExpression(std::move(node->children[0]));
        std::string rr = emitExpression(std::move(node->children[1]));

        TypeInfo t1 = regType.at(rl);
        TypeInfo t2 = regType.at(rr);
        TypeInfo tr = TypeChecker::promoteType(t1, t2);
        uint64_t bits = tr.bits;
        const std::string &op = node->value;

        // Cast operands to the promoted type
        rl = castValue(rl, t1, tr);
        rr = castValue(rr, t2, tr);

        if (tr.isFloat)
        {
            const char *suf = (bits == 32 ? "ss" : "sd");

            if (op == "+")
                out << "    add" << suf << " " << rl << ", " << rr << "\n";
            else if (op == "-")
                out << "    sub" << suf << " " << rl << ", " << rr << "\n";
            else if (op == "*")
                out << "    mul" << suf << " " << rl << ", " << rr << "\n";
            else if (op == "/")
                out << "    div" << suf << " " << rl << ", " << rr << "\n";
            else if (assembly_comparison_operations.count(op))
            {
                out << "    ucomi" << suf << " " << rl << ", " << rr << "\n";
                auto result = alloc.allocate();
                auto result8 = adjustReg(result, 8);
                out << "    " << assembly_comparison_operations.at(op) << " " << result8 << "\n";
                out << "    movzx " << result << ", " << result8 << "\n";
                alloc.free(rl);
                alloc.free(rr);
                noteType(result, node->scope->lookupType("boolean"));
                return result;
            }
            else
            {
                throw std::runtime_error("Unsupported FP op: " + op);
            }

            alloc.free(rr);
            noteType(rl, tr);
            return rl;
        }
        else
        {
            std::string dl = rl;
            std::string dr = rr;

            if (op == "+")
                out << "    add " << dl << ", " << dr << "\n";
            else if (op == "-")
                out << "    sub " << dl << ", " << dr << "\n";
            else if (op == "*")
                out << "    imul " << dl << ", " << dr << "\n";
            else if (op == "/")
            {
                auto temp = alloc.allocate();
                out << "    mov " << temp << ", rax\n";
                out << "    mov rax, " << dl << "\n";
                out << "    cqo\n";
                out << "    idiv " << dr << "\n";
                out << "    mov " << dl << ", rax\n";
                out << "    mov rax, " << temp << "\n";
                alloc.free(temp);
                alloc.free(rr);
                noteType(dl, tr);
                return dl;
            }
            else if (assembly_comparison_operations.count(op))
            {
                out << "    cmp " << dl << ", " << dr << "\n";
                auto result = alloc.allocate();
                auto result8 = adjustReg(result, 8);
                out << "    " << assembly_comparison_operations.at(op) << " " << result8 << "\n";
                out << "    movzx " << result << ", " << result8 << "\n";
                alloc.free(rr);
                noteType(result, node->scope->lookupType("boolean"));
                return result;
            }
            else
            {
                throw std::runtime_error("Unsupported integer op: " + op);
            }

            alloc.free(rr);
            noteType(rl, tr);
            return rl;
        }
    }
    std::string CodeGenWindows::generateUnaryOperation(std::unique_ptr<ASTNode> node)
    {
        const std::string &op = node->value;
        auto child = std::move(node->children[0]);

        if (op == "!")
        {
            std::string r = emitExpression(std::move(child));
            TypeInfo t = regType.at(r);

            // Cast value to boolean-compatible type (e.g., integer of any size)
            TypeInfo boolType = node->scope->lookupType("boolean");
            r = castValue(r, t, boolType);

            out << "    cmp " << r << ", 0\n";

            std::string result = alloc.allocate();
            std::string result8 = adjustReg(result, 8);

            out << "    sete " << result8 << "\n";
            out << "    movzx " << result << ", " << result8 << "\n";

            alloc.free(r);
            noteType(result, boolType);
            return result;
        }
        else if (op == "++" || op == "--")
        {
            if (child->type != NodeType::VariableAccess)
                throw std::runtime_error(op + " can only be applied to variables");

            std::string var = child->value;
            auto &scp = *child->scope;
            TypeInfo ti = scp.lookupType(scp.lookupVariable(var).type);

            if (ti.isFloat)
                throw std::runtime_error("Increment/Decrement not supported on float");

            std::string reg = generateVariableAccess(std::move(child));
            reg = castValue(reg, regType.at(reg), ti); // Ensure reg is properly typed
            std::string adj = adjustReg(reg, ti.bits);

            out << "    " << (op == "++" ? "inc" : "dec") << " " << adj << "\n";

            std::string ptrSize;
            switch (ti.bits)
            {
            case 64:
                ptrSize = "QWORD PTR ";
                break;
            case 32:
                ptrSize = "DWORD PTR ";
                break;
            case 16:
                ptrSize = "WORD PTR ";
                break;
            case 8:
                ptrSize = "BYTE PTR ";
                break;
            default:
                throw std::runtime_error("Unsupported variable size: " + std::to_string(ti.bits));
            }

            std::string addr = getVariableAddress(scp, var);
            out << "    mov " << ptrSize << addr << ", " << adj << "\n";

            noteType(reg, ti);
            return reg;
        }

        throw std::runtime_error("Unknown Unary Operator: " + op);
    }
    std::string CodeGenWindows::emitExpression(std::unique_ptr<ASTNode> node)
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
            throw std::runtime_error("Unknown expression encountered.");
        }
    }
    void CodeGenWindows::emitPrologue(std::unique_ptr<ASTNode> blockNode)
    {
        auto size = -blockNode->scope->stackOffset + 32; // +32 for shadow space
        out << "    ; Block Starts (scope enter)\n";
        out << "    push rbp\n";
        out << "    mov rbp, rsp\n";
        out << "    sub rsp, " << size << "\n";
    }
    void CodeGenWindows::emitEpilogue()
    {
        out << "    ; Block Ends (scope exit)\n";
        out << "    leave\n";
    }
    void CodeGenWindows::generateStatement(std::unique_ptr<ASTNode> statement)
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
    void CodeGenWindows::generateVariableReassignment(std::unique_ptr<ASTNode> statement)
    {
        auto &scope = *statement->scope;
        const std::string &name = statement->value;
        TypeInfo ti = scope.lookupType(scope.lookupVariable(name).type);
        uint64_t bits = ti.bits;
        std::string dest = getVariableAddress(scope, name);

        std::string r = emitExpression(std::move(statement->children.back()));
        TypeInfo rt = regType.at(r);

        r = castValue(r, rt, ti);

        if (ti.isFloat)
        {
            std::string movInstr = (bits == 32 ? "movss" : "movsd");
            std::string ptrPrefix = (bits == 32 ? "DWORD PTR " : "QWORD PTR ");
            out << "    " << movInstr << " "
                << ptrPrefix << dest << ", " << r << "\n";
            alloc.free(r);
        }
        else
        {
            std::string adj = adjustReg(r, bits);
            std::string ptrPrefix;
            switch (bits)
            {
            case 64:
                ptrPrefix = "QWORD PTR ";
                break;
            case 32:
                ptrPrefix = "DWORD PTR ";
                break;
            case 16:
                ptrPrefix = "WORD PTR ";
                break;
            case 8:
                ptrPrefix = "BYTE PTR ";
                break;
            default:
                throw std::runtime_error("Unsupported size in reassignment: " + std::to_string(bits));
            }
            out << "    mov " << ptrPrefix << dest << ", " << adj << "\n";
            alloc.free(r);
        }
    }
    void CodeGenWindows::generateVariableDeclaration(std::unique_ptr<ASTNode> statement)
    {
        auto &scope = *statement->scope;
        const std::string &name = statement->value;
        TypeInfo ti = scope.lookupType(scope.lookupVariable(name).type);
        uint64_t bits = ti.bits;

        auto ptrPrefix = [&]() -> std::string
        {
            switch (bits)
            {
            case 64:
                return "QWORD PTR ";
            case 32:
                return "DWORD PTR ";
            case 16:
                return "WORD PTR ";
            case 8:
                return "BYTE PTR ";
            default:
                throw std::runtime_error("Unsupported size in declaration: " + std::to_string(bits));
            }
        };

        if (statement->children.size() >= 2)
        {
            std::string r = emitExpression(std::move(statement->children.back()));
            TypeInfo rt = regType.at(r);
            r = castValue(r, rt, ti);

            std::string addr = getVariableAddress(scope, name);
            if (ti.isFloat)
            {
                std::string movInstr = (bits == 32 ? "movss" : "movsd");
                out << "    " << movInstr << " "
                    << ptrPrefix() << addr << ", " << r << "\n";
            }
            else
            {
                std::string adj = adjustReg(r, bits);
                out << "    mov " << ptrPrefix() << addr << ", " << adj << "\n";
            }
            alloc.free(r);
        }
        else
        {
            std::string addr = getVariableAddress(scope, name);
            if (ti.isFloat)
            {
                std::string r_xmm = alloc.allocateXMM();
                std::string movInstr = (bits == 32 ? "movss" : "movsd");
                out << "    pxor " << r_xmm << ", " << r_xmm << "\n"
                    << "    " << movInstr << " "
                    << ptrPrefix() << addr << ", " << r_xmm << "\n";
                alloc.free(r_xmm);
            }
            else
            {
                out << "    xor rax, rax\n"
                    << "    mov " << ptrPrefix() << addr << ", rax\n";
            }
        }
    }
    void CodeGenWindows::generateIfStatement(std::unique_ptr<ASTNode> statement)
    {
        int id = blockLabelCount++;
        std::string endLbl = "Lend" + std::to_string(id);

        std::vector<std::string> elseLabels;
        ASTNode *branch = statement->getElseBranch();
        while (branch)
        {
            elseLabels.push_back("Lelse" + std::to_string(blockLabelCount++));
            branch = branch->getElseBranch();
        }

        size_t elseIdx = 0;

        auto condReg = emitExpression(std::move(statement->children[0]));
        out << "    cmp " << condReg << ", 0\n";
        if (!elseLabels.empty())
            out << "    je " << elseLabels[elseIdx] << "\n";
        else
            out << "    je " << endLbl << "\n";
        alloc.free(condReg);

        auto ifBlock = std::move(statement->children[1]);
        auto stmts = std::move(ifBlock->children);
        emitPrologue(std::move(ifBlock));
        for (auto &stmt : stmts)
            generateStatement(std::move(stmt));
        emitEpilogue();
        out << "    jmp " << endLbl << "\n";

        branch = statement->getElseBranch();
        while (branch)
        {
            out << elseLabels[elseIdx++] << ":\n";

            if (branch->type == NodeType::ElseIfStatement)
            {
                auto r = emitExpression(std::move(branch->children[0]));
                out << "    cmp " << r << ", 0\n";
                if (elseIdx < elseLabels.size())
                    out << "    je " << elseLabels[elseIdx] << "\n";
                else
                    out << "    je " << endLbl << "\n";
                alloc.free(r);

                auto elifBlock = std::move(branch->children[1]);
                auto stmts = std::move(elifBlock->children);
                emitPrologue(std::move(elifBlock));
                for (auto &stmt : stmts)
                    generateStatement(std::move(stmt));
                emitEpilogue();
                out << "    jmp " << endLbl << "\n";
            }
            else if (branch->type == NodeType::ElseStatement)
            {
                auto elseBlock = std::move(branch->children[0]);
                auto stmts = std::move(elseBlock->children);
                emitPrologue(std::move(elseBlock));
                for (auto &stmt : stmts)
                    generateStatement(std::move(stmt));
                emitEpilogue();
                break;
            }
            else
            {
                throw std::runtime_error("Unexpected node type in else chain");
            }

            branch = branch->getElseBranch();
        }

        out << endLbl << ":\n\n";
    }
    void CodeGenWindows::generate(std::unique_ptr<ASTNode> program)
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
                outGlobal << g->value << " QWORD 0\n";
                break;
            case 4:
                outGlobal << g->value << " DWORD 0\n";
                break;
            case 2:
                outGlobal << g->value << " WORD 0\n";
                break;
            case 1:
                outGlobal << g->value << " BYTE 0\n";
                break;
            default:
                throw std::runtime_error("Unsupported global size");
            }
        }

        outGlobal << "\n.const\n";

        out << ".code\n";
        out << "main PROC\n";

        for (std::unique_ptr<ASTNode> &statement : program.get()->children)
        {
            generateStatement(std::move(statement));
        }

        out << "    xor eax, eax\n";
        out << "    ret\n";

        out << "main ENDP\n";

        outfinal << outGlobal.str()
                 << "\n; ============== Globals End Here ==============\n"
                 << out.str() << "END\n\n";
    }
} // namespace zlang