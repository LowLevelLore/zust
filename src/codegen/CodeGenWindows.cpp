#include "all.hpp"

namespace zlang
{
    std::string CodeGenWindows::intToXmm(const std::string &register_int,
                                         uint32_t bits)
    {
        std::string r_xmm = alloc.allocateXMM();
        const char *cvt = (bits == 32 ? "cvtsi2ss" : "cvtsi2sd");
        out << "    " << cvt
            << " " << r_xmm
            << ", " << register_int
            << "\n";
        alloc.free(register_int);
        noteType(r_xmm, {bits, bits / 8, true, true});
        return r_xmm;
    }

    std::string CodeGenWindows::generateIntegerLiteral(std::unique_ptr<ASTNode> node)
    {
        auto r = alloc.allocate();
        out << "    mov    " << r << ", " << node->value << "\n";
        noteType(r, node->scope->lookupType("int64_t"));
        return r;
    }
    std::string CodeGenWindows::generateFloatLiteral(std::unique_ptr<ASTNode> node)
    {
        std::string lbl = "Lfloat" + std::to_string(floatLabelCount++);
        auto val = node->value;
        bool isF32 = (!val.empty() && (val.back() == 'f' || val.back() == 'F'));
        if (isF32)
            val.pop_back();

        // in read‑only data
        out << ".rdata?\n";
        out << lbl << " DWORD " << val << "\n";
        out << ".code\n";

        auto r_xmm = alloc.allocateXMM();
        out << "    " << (isF32 ? "movss" : "movsd")
            << " " << r_xmm
            << ", PTR [" << lbl << "]\n";
        noteType(r_xmm, node->scope->lookupType(isF32 ? "float" : "double"));
        return r_xmm;
    }
    std::string CodeGenWindows::generateStringLiteral(std::unique_ptr<ASTNode> node)
    {
        std::string lbl = ".Lstr" + std::to_string(stringLabelCount++);
        out << ".rdata?\n";
        out << lbl << " BYTE "
            << '"' << node->value << '"' << ", 0\n";
        out << ".code\n";

        auto r = alloc.allocate();
        out << "    lea    " << r
            << ", PTR [" << lbl << "]\n";
        noteType(r, node->scope->lookupType("string"));
        return r;
    }
    std::string
    CodeGenWindows::generateBooleanLiteral(std::unique_ptr<ASTNode> node)
    {
        auto r = alloc.allocate();
        out << "    movq $" << (node->value == "true" ? "1" : "0") << ", %" << r
            << "\n";
        noteType(r, node->scope->lookupType("boolean"));
        return r;
    }
    std::string
    CodeGenWindows::generateVariableAccess(std::unique_ptr<ASTNode> node)
    {
        auto &scope = *node->scope;
        auto name = node->value;
        TypeInfo ti = scope.lookupType(scope.lookupVariable(name).type);
        uint64_t sz = ti.bits / 8;

        if (scope.isGlobalVariable(name))
        {
            if (ti.isFloat)
            {
                // global float: movss/movsd xmm0, PTR [name]
                auto r_xmm = alloc.allocateXMM();
                std::string mov = (sz == 4 ? "movss" : "movsd");
                out << "    " << mov
                    << " " << r_xmm
                    << ", PTR [" << name << "]\n";
                noteType(r_xmm, ti);
                return r_xmm;
            }
            else
            {
                // global integer: mov    rax, qword ptr [name]
                auto r = alloc.allocate();
                std::string ptrSize = (sz == 8 ? "qword ptr " : sz == 4 ? "dword ptr "
                                                            : sz == 2   ? "word ptr "
                                                                        : "byte ptr ");
                out << "    mov    " << r << ", "
                    << ptrSize << "[" << name << "]\n";
                noteType(r, ti);
                return r;
            }
        }
        else
        {
            int64_t off = scope.getVariableOffset(name);
            if (ti.isFloat)
            {
                // stack float: movss/movsd xmm0, PTR [rbp - offset]
                auto r_xmm = alloc.allocateXMM();
                const char *mov = (sz == 4 ? "movss" : "movsd");
                out << "    " << mov
                    << " " << r_xmm
                    << ", PTR [rbp - " << off << "]\n";
                noteType(r_xmm, ti);
                return r_xmm;
            }
            else
            {
                // stack integer: mov    eax, dword ptr [rbp - offset]
                auto r = alloc.allocate();
                std::string ptrSize = (sz == 8 ? "qword ptr " : sz == 4 ? "dword ptr "
                                                            : sz == 2   ? "word ptr "
                                                                        : "byte ptr ");
                out << "    mov    " << r << ", "
                    << ptrSize << "[rbp - " << off << "]\n";
                noteType(r, ti);
                return r;
            }
        }
    }
    std::string
    CodeGenWindows::generateBinaryOperation(std::unique_ptr<ASTNode> node)
    {
        std::string rl = emitExpression(std::move(node->children[0]));
        std::string rr = emitExpression(std::move(node->children[1]));

        // Lookup types & decide result
        TypeInfo t1 = regType.at(rl);
        TypeInfo t2 = regType.at(rr);
        TypeInfo tr = TypeChecker::promoteType(t1, t2);
        const std::string &op = node->value;

        if (tr.isFloat)
        {
            // FP promotion
            uint64_t bits = tr.bits;
            const char *suf = (bits == 32 ? "ss" : "sd");
            // Convert ints → xmm if needed
            std::string xl = (t1.isFloat ? rl : intToXmm(rl, bits));
            std::string xr = (t2.isFloat ? rr : intToXmm(rr, bits));

            // Instruction selection
            if (op == "+")
                out << "    add" << suf << " " << xl << ", " << xr << "\n";
            else if (op == "-")
                out
                    << "    sub" << suf << " " << xl << ", " << xr << "\n";
            else if (op == "*")
                out
                    << "    mul" << suf << " " << xl << ", " << xr << "\n";
            else if (op == "/")
                out
                    << "    div" << suf << " " << xl << ", " << xr << "\n";
            else if (assembly_comparison_operations.count(op))
            {
                out << "    ucomi" << suf << " " << xl << ", " << xr << "\n";
                out
                    << "    " << assembly_comparison_operations.at(op) << " al\n";
                out
                    << "    movzx rax, al\n";
                alloc.freeXMM(xl);
                alloc.freeXMM(xr);
                noteType("rax", node->scope->lookupType("boolean"));
                return "rax";
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
            // Integer path
            char suf = integer_suffixes[tr.bits]; // 'b','w','l','q'
            std::string dl = adjustReg(rl, tr.bits);
            std::string dr = adjustReg(rr, tr.bits);

            if (op == "+")
                out << "    add" << suf << " " << dl << ", " << dr << "\n";
            else if (op == "-")
                out
                    << "    sub" << suf << " " << dl << ", " << dr << "\n";
            else if (op == "*")
                out
                    << "    imul" << suf << " " << dl << ", " << dr << "\n";
            else if (op == "/")
            {
                // idiv: dividend in rax, divisor dr → result in rax
                out << "    mov    rax, " << dl << "\n";
                out
                    << "    cqo\n"; // sign-extend to rdx:rax
                out
                    << "    idiv   " << dr << "\n";
                out
                    << "    mov    " << dl << ", rax\n";
                alloc.free(rr);
                noteType(dl, tr);
                return dl;
            }
            else if (assembly_comparison_operations.count(op))
            {
                out << "    cmp" << suf << " " << dl << ", " << dr << "\n";
                out
                    << "    " << assembly_comparison_operations.at(op) << " al\n";
                out
                    << "    movzx rax, al\n";
                alloc.free(rr);
                noteType("rax", node->scope->lookupType("boolean"));
                return "rax";
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
    std::string
    CodeGenWindows::generateUnaryOperation(std::unique_ptr<ASTNode> node)
    {
        const std::string &op = node->value;
        auto child = std::move(node->children[0]);
        if (op == "!")
        {
            auto r = emitExpression(std::move(child));
            // cmp r,0; sete al; movzx rax, al
            out << "    cmp    " << r << ", 0";
            out
                << "    sete   al";
            out
                << "    movzx  rax, al";
            alloc.free(r);
            noteType("rax", node->scope->lookupType("boolean"));
            return "rax";
        }
        else if (op == "++" || op == "--")
        {
            std::string var = child->value;
            auto &scp = *child->scope;
            TypeInfo ti = child->scope->lookupType(child->scope->lookupVariable(var).type);
            if (ti.isFloat)
                throw std::runtime_error("Increment/Decrement not supported on float");
            char suf = integer_suffixes[ti.bits];
            std::string reg = generateVariableAccess(std::move(child)); // load variable into reg or xmm
            std::string adj = adjustReg(reg, ti.bits);                  // get correct name size
            // perform inc or dec
            out << "    " << (op == "++" ? "inc" : "dec") << suf << " " << adj << "\n";

            // store back to memory
            std::string ptrSize = (ti.bits / 8 == 8 ? "qword ptr " : ti.bits / 8 == 4 ? "dword ptr "
                                                                 : ti.bits / 8 == 2   ? "word ptr "
                                                                                      : "byte ptr ");
            if (scp.isGlobalVariable(var))
            {
                out << "    mov    " << ptrSize << "[" << var << "], " << adj << "\n";
            }
            else
            {
                int64_t off = scp.getVariableOffset(var);
                out << "    mov    " << ptrSize << "[rbp - " << off << "], " << adj << "\n";
            }
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

    void CodeGenWindows::emitEpilogue()
    {
        out << "    # Block Ends (scope exit)\n";
        out << "    leave\n";
    }
    void CodeGenWindows::emitPrologue(std::unique_ptr<ASTNode> blockNode)
    {
        auto size = -blockNode->scope->stackOffset + 32; // +32 for shadow space
        out << "    # Block Starts (scope enter)\n";
        out << "    push   rbp\n";
        out << "    mov    rbp, rsp\n";
        out << "    sub    rsp, " << size << "\n";
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

    void CodeGenWindows::generateVariableReassignment(
        std::unique_ptr<ASTNode> statement)
    {
        auto &scope = *statement->scope;
        auto name = statement->value;
        TypeInfo ti = scope.lookupType(scope.lookupVariable(name).type);
        uint64_t sz = ti.bits / 8;
        if (ti.isFloat)
        {
            auto r_xmm = emitExpression(std::move(statement->children.back()));
            std::string mov = (sz == 4 ? "movss" : "movsd");
            if (scope.isGlobalVariable(name))
            {
                // movss/movsd PTR [name], xmm0
                out << "    " << mov << " PTR [" << name << "], " << r_xmm << "\n";
            }
            else
            {
                int64_t off = scope.getVariableOffset(name);
                // movss/movsd PTR [rbp - offset], xmm0
                out << "    " << mov << " PTR [rbp - " << off << "], " << r_xmm << "\n";
            }
            alloc.freeXMM(r_xmm);
        }
        else
        {
            auto r = emitExpression(std::move(statement->children.back()));
            std::string ptrSize = (sz == 8 ? "qword ptr " : sz == 4 ? "dword ptr "
                                                        : sz == 2   ? "word ptr "
                                                                    : "byte ptr ");
            if (scope.isGlobalVariable(name))
            {
                // mov    qword ptr [name], rax
                out << "    mov    " << ptrSize << "[" << name << "], " << r << "\n";
            }
            else
            {
                int64_t off = scope.getVariableOffset(name);
                // mov    qword ptr [rbp - offset], rax
                out << "    mov    " << ptrSize << "[rbp - " << off << "], " << r << "\n";
            }
            alloc.free(r);
        }
    }
    void CodeGenWindows::generateVariableDeclaration(
        std::unique_ptr<ASTNode> statement)
    {
        auto &scope = *statement->scope;
        auto name = statement->value;
        TypeInfo ti = scope.lookupType(scope.lookupVariable(name).type);
        uint64_t sz = ti.bits / 8;

        if (!scope.isGlobalVariable(name))
        {
            // allocate on stack
            out << "    sub    rsp, " << sz << "\n";
            if (statement->children.size() >= 2)
            {
                if (ti.isFloat)
                {
                    auto r_xmm = emitExpression(std::move(statement->children.back()));
                    const char *mov = (sz == 4 ? "movss" : "movsd");
                    int64_t off = scope.getVariableOffset(name);
                    // movss/movsd PTR [rbp - offset], xmm0
                    out << "    " << mov << " PTR [rbp - " << off << "], " << r_xmm << "\n";
                    alloc.freeXMM(r_xmm);
                }
                else
                {
                    auto r = emitExpression(std::move(statement->children.back()));
                    std::string ptrSize = (sz == 8 ? "qword ptr " : sz == 4 ? "dword ptr "
                                                                : sz == 2   ? "word ptr "
                                                                            : "byte ptr ");
                    int64_t off = scope.getVariableOffset(name);
                    // mov    ptr [rbp - offset], rax
                    out << "    mov    " << ptrSize << "[rbp - " << off << "], " << r << "\n";
                    alloc.free(r);
                }
            }
            else
            {
                // zero initialize
                if (ti.isFloat)
                {
                    // pxor xmm0, xmm0
                    auto r_xmm = alloc.allocateXMM();
                    out << "    pxor   " << r_xmm << ", " << r_xmm << "\n";
                    out
                        << "    movsd  PTR [rsp], " << r_xmm << "\n";
                    alloc.freeXMM(r_xmm);
                }
                else
                {
                    out << "    xor    rax, rax\n";
                    out
                        << "    mov    qword ptr [rsp], rax\n";
                }
            }
        }
        else
        {
            // global: already in .data?
            if (statement->children.size() >= 2)
            {
                if (ti.isFloat)
                {
                    auto r_xmm = emitExpression(std::move(statement->children.back()));
                    const char *mov = (sz == 4 ? "movss" : "movsd");
                    out << "    " << mov << " PTR [" << name << "], " << r_xmm << "\n";
                    alloc.freeXMM(r_xmm);
                }
                else
                {
                    auto r = emitExpression(std::move(statement->children.back()));
                    std::string ptrSize = (sz == 8 ? "qword ptr " : sz == 4 ? "dword ptr "
                                                                : sz == 2   ? "word ptr "
                                                                            : "byte ptr ");
                    out << "    mov    " << ptrSize << "[" << name << "], " << r << "\n";
                    alloc.free(r);
                }
            }
        }
    }
    void CodeGenWindows::generateIfStatement(std::unique_ptr<ASTNode> statement)
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

    void CodeGenWindows::generate(std::unique_ptr<ASTNode> program)
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