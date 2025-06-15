#include "all.hpp"

namespace zlang
{
    std::string CodeGenWindows::intToXmm(const std::string &register_int, uint32_t bits)
    {
        std::string r_xmm = alloc.allocateXMM();
        const char *cvt = (bits == 32 ? "cvtsi2ss" : "cvtsi2sd");
        out << "    " << cvt
            << " " << r_xmm
            << " " << register_int
            << "\n";
        alloc.free(register_int);
        noteType(r_xmm, {bits, bits / 8, true, true});
        return r_xmm;
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

        std::ostringstream escaped;
        for (char c : node->value)
        {
            if (c == '"')
                escaped << "\"\""; // MASM doubles quotes inside strings
            else
                escaped << c;
        }

        outGlobal << lbl << " BYTE "
                  << '"' << escaped.str() << '"' << ", 0\n";

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

        std::string address;
        if (scope.isGlobalVariable(name))
        {
            address = name;
        }
        else
        {
            int64_t off = scope.getVariableOffset(name);
            address = "rbp - " + std::to_string(off);
        }

        if (ti.isFloat)
        {
            auto r_xmm = alloc.allocateXMM();
            if (sz == 4)
            {
                out << "    movss " << r_xmm << ", DWORD PTR [" << address << "]\n";
            }
            else if (sz == 8)
            {
                out << "    movsd " << r_xmm << ", QWORD PTR [" << address << "]\n";
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

            out << "    mov " << r_adj << ", " << ptrSize << "[" << address << "]\n";
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

        if (tr.isFloat)
        {
            const char *suf = (bits == 32 ? "ss" : "sd");
            std::string xl = (t1.isFloat ? rl : intToXmm(rl, bits));
            std::string xr = (t2.isFloat ? rr : intToXmm(rr, bits));

            if (op == "+")
                out << "    add" << suf << " " << xl << ", " << xr << "\n";
            else if (op == "-")
                out << "    sub" << suf << " " << xl << ", " << xr << "\n";
            else if (op == "*")
                out << "    mul" << suf << " " << xl << ", " << xr << "\n";
            else if (op == "/")
                out << "    div" << suf << " " << xl << ", " << xr << "\n";
            else if (assembly_comparison_operations.count(op))
            {
                out << "    ucomi" << suf << " " << xl << ", " << xr << "\n";
                auto result = alloc.allocate();
                auto result8 = adjustReg(result, 8);
                out << "    " << assembly_comparison_operations.at(op) << " " << result8 << "\n";
                out << "    movzx " << result << ", " << result8 << "\n";
                alloc.freeXMM(xl);
                alloc.freeXMM(xr);
                noteType(result, node->scope->lookupType("boolean"));
                return result;
            }
            else
            {
                throw std::runtime_error("Unsupported FP op: " + op);
            }

            alloc.freeXMM(xr);
            noteType(xl, tr);
            return xl;
        }
        else
        {
            auto dl = adjustReg(rl, bits);
            auto dr = adjustReg(rr, bits);

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
            out << "    cmp " << r << ", 0\n";

            std::string result = alloc.allocate();
            std::string result8 = adjustReg(result, 8);

            out << "    sete " << result8 << "\n";
            out << "    movzx " << result << ", " << result8 << "\n";

            alloc.free(r);

            noteType(result, node->scope->lookupType("boolean"));
            return result;
        }
        else if (op == "++" || op == "--")
        {
            std::string var = child->value;
            auto &scp = *child->scope;
            TypeInfo ti = scp.lookupType(scp.lookupVariable(var).type);
            uint64_t bits = ti.bits;

            if (ti.isFloat)
                throw std::runtime_error("Increment/Decrement not supported on float");

            std::string reg = generateVariableAccess(std::move(child));
            std::string adj = adjustReg(reg, bits);

            out << "    " << (op == "++" ? "inc" : "dec") << " " << adj << "\n";

            std::string ptrSize;
            switch (bits)
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
                throw std::runtime_error("Unsupported variable size: " + std::to_string(bits));
            }

            std::string addr = scp.isGlobalVariable(var)
                                   ? var
                                   : "rbp - " + std::to_string(scp.getVariableOffset(var));

            out << "    mov " << ptrSize << "[" << addr << "], " << adj << "\n";

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
            throw std::runtime_error("Unknown statement encountered.");
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

        auto destAddr = [&]() -> std::string
        {
            if (scope.isGlobalVariable(name))
                return "[" + name + "]";
            else
                return "[rbp - " + std::to_string(scope.getVariableOffset(name)) + "]";
        };

        if (ti.isFloat)
        {
            std::string r_xmm = emitExpression(std::move(statement->children.back()));
            std::string movInstr = (bits == 32) ? "movss" : "movsd";

            out << "    " << movInstr << " " << destAddr() << ", " << r_xmm << "\n";
            alloc.freeXMM(r_xmm);
        }
        else
        {
            std::string r = emitExpression(std::move(statement->children.back()));
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
                throw std::runtime_error("Unsupported variable size in reassignment: " + std::to_string(bits));
            }

            out << "    mov " << ptrPrefix << destAddr() << ", " << adj << "\n";
            alloc.free(r);
        }
    }

    void CodeGenWindows::generateVariableDeclaration(std::unique_ptr<ASTNode> statement)
    {
        auto &scope = *statement->scope;
        const std::string &name = statement->value;
        TypeInfo ti = scope.lookupType(scope.lookupVariable(name).type);
        uint64_t bits = ti.bits;
        uint64_t sz = bits / 8;

        auto destAddr = [&]() -> std::string
        {
            if (scope.isGlobalVariable(name))
                return "[" + name + "]";
            else
                return "[rbp - " + std::to_string(scope.getVariableOffset(name)) + "]";
        };

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

        if (!scope.isGlobalVariable(name))
        {
            out << "    sub rsp, " << sz << "\n";
        }

        if (statement->children.size() >= 2)
        {
            if (ti.isFloat)
            {
                std::string r_xmm = emitExpression(std::move(statement->children.back()));
                std::string movInstr = (bits == 32) ? "movss" : "movsd";
                out << "    " << movInstr << " " << destAddr() << ", " << r_xmm << "\n";
                alloc.freeXMM(r_xmm);
            }
            else
            {
                std::string r = emitExpression(std::move(statement->children.back()));
                std::string adj = adjustReg(r, bits);
                out << "    mov " << ptrPrefix() << destAddr() << ", " << adj << "\n";
                alloc.free(r);
            }
        }
        else
        {
            if (ti.isFloat)
            {
                std::string r_xmm = alloc.allocateXMM();
                out << "    pxor " << r_xmm << ", " << r_xmm << "\n";
                out << "    movsd " << destAddr() << ", " << r_xmm << "\n";
                alloc.freeXMM(r_xmm);
            }
            else
            {
                out << "    xor rax, rax\n";
                out << "    mov " << ptrPrefix() << destAddr() << ", rax\n";
            }
        }
    }

    void CodeGenWindows::generateIfStatement(std::unique_ptr<ASTNode> statement)
    {
        int id = blockLabelCount++;
        std::string endLbl = "Lend" + std::to_string(id);
        std::string nextLbl = "Lelse" + std::to_string(id);

        auto emitBlock = [&](std::unique_ptr<ASTNode> block)
        {
            std::vector<std::unique_ptr<ASTNode>> stmts = std::move(block->children);
            emitPrologue(std::move(block));
            for (auto &stmt : stmts)
                generateStatement(std::move(stmt));
            emitEpilogue();
        };

        auto condReg = emitExpression(std::move(statement->children[0]));
        out << "    cmp " << condReg << ", 0\n";
        out << "    je " << nextLbl << "\n";
        alloc.free(condReg);

        emitBlock(std::move(statement->children[1]));
        out << "    jmp " << endLbl << "\n";
        out << nextLbl << ":\n";

        ASTNode *branch = statement->getElseBranch();
        while (branch)
        {
            if (branch->type == NodeType::ElseIfStatement)
            {
                std::string elseIfNextLbl = "Lelse" + std::to_string(blockLabelCount++);

                auto r = emitExpression(std::move(branch->children[0]));
                out << "    cmp " << r << ", 0\n";
                out << "    je " << elseIfNextLbl << "\n";
                alloc.free(r);

                emitBlock(std::move(branch->children[1]));
                out << "    jmp " << endLbl << "\n";
                out << elseIfNextLbl << ":\n";

                branch = branch->getElseBranch();
            }
            else if (branch->type == NodeType::ElseStatement)
            {
                emitBlock(std::move(branch->children[0]));
                break;
            }
            else
            {
                throw std::runtime_error("Unexpected node type in else chain");
            }
        }

        out << endLbl << ":\n\n";
    }

    void CodeGenWindows::generate(std::unique_ptr<ASTNode> program)
    {
        std::vector<ASTNode *> globals;
        for (auto &statement : program->children)
            if (statement->type == NodeType::VariableDeclaration)
                globals.push_back(statement.get());

        std::cout << "GENERATE CALLED" << std::endl;
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

        out << "    mov rax, 60\n";
        out << "    mov rdi, 0\n";
        out << "    syscall\n";

        out << "main ENDP\n";

        outfinal << outGlobal.str()
                 << "\n; ============== Globals End Here ==============\n"
                 << out.str() << "\n\n";
    }

} // namespace zlang