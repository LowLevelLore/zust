#include "all.hpp"

namespace zlang {
    std::string CodeGenLinux::castValue(
        const std::string &val,
        const TypeInfo &fromType,
        const TypeInfo &toType, const std::shared_ptr<ScopeContext> currentScope, std::ostringstream &out) {
        // Ensure input value is in a register (restore if previously spilled)
        restoreIfSpilled(val, currentScope, out);

        // No-op cast except sign adjustment
        if (fromType.bits == toType.bits && fromType.isFloat == toType.isFloat) {
            if (!fromType.isFloat && fromType.isSigned != toType.isSigned) {
                // Allocate or spill an integer register
                std::string dst = allocateOrSpill(false, currentScope, out);
                // AT&T syntax: source, destination
                out << "    mov %" << adjustReg(val, fromType.bits)
                    << ", %" << adjustReg(dst, fromType.bits) << "\n";
                alloc.free(val);
                noteType(dst, toType);
                return dst;
            }
            return RegisterAllocator::getBaseReg(val);
        }

        // Float-to-float conversion
        if (fromType.isFloat && toType.isFloat) {
            std::string dstX = allocateOrSpill(true, currentScope, out);
            auto instr = (fromType.bits < toType.bits) ? "cvtss2sd" : "cvtsd2ss";
            out << "    " << instr << " %" << val << ", %" << dstX << "\n";
            alloc.free(val);
            noteType(dstX, toType);
            return dstX;
        }

        // Integer-to-float conversion
        if (!fromType.isFloat && toType.isFloat) {
            std::string dstX = allocateOrSpill(true, currentScope, out);
            auto instr = (toType.bits == 32) ? "cvtsi2ss" : "cvtsi2sd";
            out << "    " << instr << " %" << adjustReg(val, fromType.bits)
                << ", %" << dstX << "\n";
            alloc.free(val);
            noteType(dstX, toType);
            return dstX;
        }

        // Float-to-integer conversion
        if (fromType.isFloat && !toType.isFloat) {
            std::string dstG = allocateOrSpill(false, currentScope, out);
            auto instr = (fromType.bits == 32) ? "cvttss2si" : "cvttsd2si";
            out << "    " << instr << " %" << val
                << ", %" << adjustReg(dstG, toType.bits) << "\n";
            alloc.free(val);
            noteType(dstG, toType);
            return dstG;
        }

        // Integer-to-integer cast (extend/truncate/move)
        if (!fromType.isFloat && !toType.isFloat) {
            std::string dstG = allocateOrSpill(false, currentScope, out);
            std::string srcAdj = adjustReg(val, fromType.bits);
            std::string dstAdj = adjustReg(dstG, toType.bits);

            if (toType.bits > fromType.bits) {
                if (fromType.isSigned) {
                    if (fromType.bits == 32 && toType.bits == 64) {
                        out << "    movsxd %" << srcAdj << ", %" << dstAdj << "\n";
                    } else {
                        out << "    movsx %" << srcAdj << ", %" << dstAdj << "\n";
                    }
                } else {
                    if (fromType.bits == 8 || fromType.bits == 16) {
                        out << "    movzx %" << srcAdj << ", %" << dstAdj << "\n";
                    } else {
                        out << "    movl %" << adjustReg(val, 32)
                            << ", %" << adjustReg(dstG, 32) << "\n";
                    }
                }
            } else if (toType.bits < fromType.bits) {
                // Truncate
                out << "    mov %" << adjustReg(val, toType.bits)
                    << ", %" << dstAdj << "\n";
            } else {
                // Simple move
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

    std::string CodeGenLinux::allocateOrSpill(bool isXMM, std::shared_ptr<ScopeContext> scope, std::ostringstream &out) {
        try {
            std::string reg = isXMM ? alloc.allocateXMM() : alloc.allocate();
            if (!reg.empty())
                return reg;
        } catch (...) {
            auto result_scope = scope->findEnclosingFunctionScope();
            if (!result_scope) {
                throw std::runtime_error("Cannot spill in global context");
            }

            std::shared_ptr<FunctionScope> funcScope = std::static_pointer_cast<FunctionScope>(result_scope);

            std::string victim = isXMM ? alloc.pickVictimXMM() : alloc.pickVictim();
            if (victim.empty())
                throw std::runtime_error("No available registers and no victim found");

            std::string spillSlot = funcScope->allocateSpillSlot(isXMM ? 16 : 8, CodegenOutputFormat::X86_64_LINUX);

            if (isXMM) {
                alloc.markSpilledXMM(victim, spillSlot);
                out << "    movdqu " << victim << ", " << spillSlot << "    # Spill XMM\n";
            } else {
                char suffix = integer_suffixes.at(64);
                out << "    mov" << suffix << " " << victim << ", " << spillSlot << "    # Spill GPR\n";
                alloc.markSpilled(victim, spillSlot);
            }

            return victim;
        }
        return "";
    }
    void CodeGenLinux::restoreIfSpilled(const std::string &reg, std::shared_ptr<ScopeContext> scope, std::ostringstream &out) {
        if (!alloc.isSpilled(reg))
            return;

        std::string slot = alloc.spillSlotFor(reg);

        if (reg.starts_with("xm")) {
            alloc.unSpillXMM(reg, zlang::CodegenOutputFormat::X86_64_LINUX, out);
        } else {
            alloc.unSpill(reg, zlang::CodegenOutputFormat::X86_64_LINUX, out);
        }

        auto result_scope = scope->findEnclosingFunctionScope();
        if (result_scope) {
            std::shared_ptr<FunctionScope> funcScope = std::static_pointer_cast<FunctionScope>(result_scope);
            funcScope->freeSpillSlot(slot, reg.starts_with("xm") ? 16 : 8);
        }
    }

    std::string CodeGenLinux::generateIntegerLiteral(std::unique_ptr<ASTNode> node, std::ostringstream &out) {
        TypeInfo intType = node->scope->lookupType("int64_t");
        std::string r = allocateOrSpill(false, node->scope, out);
        out << "    movq $" << node->value << ", %" << r << "\n";
        noteType(r, intType);
        return r;
    }
    std::string CodeGenLinux::generateFloatLiteral(std::unique_ptr<ASTNode> node, std::ostringstream &out) {
        std::string lbl = ".Lfloat" + std::to_string(floatLabelCount++);
        std::string val = node->value;
        bool isF32 = (!val.empty() && (val.back() == 'f' || val.back() == 'F'));
        if (isF32)
            val.pop_back();
        outGlobalStream << "    .align " << (isF32 ? 4 : 8) << "\n"
                        << lbl << ": ." << (isF32 ? "float" : "double")
                        << " " << val << "\n";
        TypeInfo floatType = isF32
                                 ? node->scope->lookupType("float")
                                 : node->scope->lookupType("double");
        std::string r_xmm = allocateOrSpill(true, node->scope, out);
        out << "    " << (isF32 ? "movss" : "movsd")
            << " " << lbl << "(%rip), %" << r_xmm << "\n";
        noteType(r_xmm, floatType);
        return r_xmm;
    }
    std::string CodeGenLinux::generateStringLiteral(std::unique_ptr<ASTNode> node, std::ostringstream &out) {
        std::string lbl = ".Lstr" + std::to_string(stringLabelCount++);
        std::string str = node->value;
        outGlobalStream << "    .align 1\n"
                        << lbl << ": .asciz \"" << str << "\"\n";
        TypeInfo strType = node->scope->lookupType("string");
        std::string r = allocateOrSpill(false, node->scope, out);
        out << "    leaq " << lbl << "(%rip), %" << r << "\n";
        noteType(r, strType);
        return r;
    }
    std::string CodeGenLinux::generateBooleanLiteral(std::unique_ptr<ASTNode> node, std::ostringstream &out) {
        TypeInfo boolType = node->scope->lookupType("boolean");
        std::string r = allocateOrSpill(false, node->scope, out);

        // Move immediate 1 or 0
        out << "    movq $" << (node->value == "true" ? "1" : "0")
            << ", %" << r << "\n";
        noteType(r, boolType);
        return r;
    }
    std::string CodeGenLinux::generateVariableAccess(std::unique_ptr<ASTNode> node, std::ostringstream &out) {
        auto &scope = *node->scope;
        std::string name = node->value;
        TypeInfo ti = scope.lookupType(scope.lookupVariable(name).type);
        uint64_t sz = ti.bits / 8;

        if (scope.isGlobalVariable(name)) {
            if (ti.isFloat) {
                // Allocate or spill an XMM reg
                std::string r_xmm = allocateOrSpill(true, node->scope, out);
                out << "    " << (sz == 4 ? "movss" : "movsd")
                    << " " << name << "(%rip), %" << r_xmm << "\n";
                noteType(r_xmm, ti);
                return r_xmm;
            } else {
                // Allocate or spill a GPR
                std::string r = allocateOrSpill(false, node->scope, out);
                std::string adj = adjustReg(r, ti.bits);
                out << "    " << getCorrectMove(sz, false)
                    << " " << name << "(%rip), %" << adj << "\n";
                noteType(r, ti);
                return r;
            }
        } else {
            int64_t off = scope.getVariableOffset(name);
            if (ti.isFloat) {
                std::string r_xmm = allocateOrSpill(true, node->scope, out);
                out << "    " << (sz == 4 ? "movss" : "movsd")
                    << " " << std::to_string(off) << "(%rbp), %" << r_xmm << "\n";
                noteType(r_xmm, ti);
                return r_xmm;
            } else {
                std::string r = allocateOrSpill(false, node->scope, out);
                std::string adj = adjustReg(r, ti.bits);
                out << "    " << getCorrectMove(sz, false)
                    << " " << std::to_string(off) << "(%rbp), %" << adj << "\n";
                noteType(r, ti);
                return r;
            }
        }
    }
    std::string CodeGenLinux::generateBinaryOperation(std::unique_ptr<ASTNode> node, std::ostringstream &out) {
        // Evaluate operands
        std::string rl = emitExpression(std::move(node->children[0]), out);
        restoreIfSpilled(rl, node->scope, out);

        std::string rr = emitExpression(std::move(node->children[1]), out);
        restoreIfSpilled(rr, node->scope, out);

        // Determine promoted type and operator
        TypeInfo t1 = regType.at(rl);
        TypeInfo t2 = regType.at(rr);
        TypeInfo tr = TypeChecker::promoteType(t1, t2);
        const std::string &op = node->value;

        // Cast operands to common type
        rl = castValue(rl, t1, tr, node->scope, out);
        restoreIfSpilled(rl, node->scope, out);
        rr = castValue(rr, t2, tr, node->scope, out);
        restoreIfSpilled(rl, node->scope, out);

        if (tr.isFloat) {
            bool isF32 = (tr.bits == 32);
            std::string vsuf = isF32 ? "ss" : "sd";
            // Perform floating-point operation
            if (op == "+")
                out << "    add" << vsuf << " %" << rr << ", %" << rl << "\n";
            else if (op == "-")
                out << "    sub" << vsuf << " %" << rr << ", %" << rl << "\n";
            else if (op == "*")
                out << "    mul" << vsuf << " %" << rr << ", %" << rl << "\n";
            else if (op == "/")
                out << "    div" << vsuf << " %" << rr << ", %" << rl << "\n";
            else if (assembly_comparison_operations.count(op)) {
                out << "    ucomi" << vsuf << " %" << rr << ", %" << rl << "\n"
                    << "    " << assembly_comparison_operations.at(op) << " %al\n";
                // Allocate boolean result
                TypeInfo boolType = node->scope->lookupType("boolean");
                std::string r_bool = allocateOrSpill(false, node->scope, out);
                out << "    movzbq %al, %" << r_bool << "\n";
                noteType(r_bool, boolType);
                // Free operand regs
                alloc.free(rr);
                alloc.free(rl);
                return r_bool;
            } else {
                throw std::runtime_error("Unsupported FP op " + op);
            }
            // Free right operand, result in rl
            alloc.free(rr);
            noteType(rl, tr);
            return rl;
        }

        // Integer operations
        char suf = integer_suffixes.at(tr.bits);
        std::string adj_l = adjustReg(rl, tr.bits);
        std::string adj_r = adjustReg(rr, tr.bits);

        if (op == "+" || op == "-" || op == "*" || op == "/") {
            if (op == "+")
                out << "    add" << suf << " %" << adj_r << ", %" << adj_l << "\n";
            else if (op == "-")
                out << "    sub" << suf << " %" << adj_r << ", %" << adj_l << "\n";
            else if (op == "*")
                out << "    imul" << suf << " %" << adj_r << ", %" << adj_l << "\n";
            else /* div */ {
                // division requires special setup
                if (tr.isSigned) {
                    if (tr.bits == 32) {
                        out << "    movl %" << adj_l << ", %eax\n"
                            << "    cltd\n"
                            << "    idivl %" << adj_r << "\n"
                            << "    movl %eax, %" << adj_l << "\n";
                    } else {
                        out << "    movq %" << adj_l << ", %rax\n"
                            << "    cqo\n"
                            << "    idivq %" << adj_r << "\n"
                            << "    movq %rax, %" << adj_l << "\n";
                    }
                } else {
                    if (tr.bits == 32) {
                        out << "    movl %" << adj_l << ", %eax\n"
                            << "    clrl %edx\n"
                            << "    divl %" << adj_r << "\n"
                            << "    movl %eax, %" << adj_l << "\n";
                    } else {
                        out << "    movq %" << adj_l << ", %rax\n"
                            << "    xorq %rdx, %rdx\n"
                            << "    divq %" << adj_r << "\n"
                            << "    movq %rax, %" << adj_l << "\n";
                    }
                }
                alloc.free(rr);
                noteType(rl, tr);
                return rl;
            }
            alloc.free(rr);
            noteType(rl, tr);
            return rl;
        }

        // Integer comparison
        if (assembly_comparison_operations.count(op)) {
            auto &map = tr.isSigned ? assembly_comparison_operations : /* unsigned map here */ assembly_comparison_operations;
            out << "    cmp" << suf << " %" << adj_r << ", %" << adj_l << "\n"
                << "    " << map.at(op) << " %al\n";
            TypeInfo boolType = node->scope->lookupType("boolean");
            std::string r_bool = allocateOrSpill(false, node->scope, out);
            out << "    movzbq %al, %" << r_bool << "\n";
            noteType(r_bool, boolType);
            alloc.free(rr);
            alloc.free(rl);
            return r_bool;
        }

        // Bitwise/logical ops: &&, ||, &, |
        if (op == "&&" || op == "||" || op == "&" || op == "|") {
            std::string instr = (op == "&&") ? "and" : (op == "||") ? "or"
                                                                    : op;
            out << "    " << instr << suf << " %" << adj_r << ", %" << adj_l << "\n";
            alloc.free(rr);
            noteType(rl, tr);
            return rl;
        }

        throw std::runtime_error("Unsupported int op " + op);
    }
    std::string CodeGenLinux::generateUnaryOperation(std::unique_ptr<ASTNode> node, std::ostringstream &out) {
        const auto &op = node->value;
        if (op != "!" && op != "++" && op != "--")
            throw std::runtime_error("Unsupported unary operator: " + op);

        auto child = std::move(node->children[0]);

        if (op == "!") {
            auto r = emitExpression(std::move(child), out);
            restoreIfSpilled(r, node->scope, out);
            TypeInfo boolType = node->scope->lookupType("boolean");
            r = castValue(r, regType.at(r), boolType, node->scope, out);
            restoreIfSpilled(r, node->scope, out);

            auto res = allocateOrSpill(false, node->scope, out);
            out << "    cmpb $0, %" << adjustReg(r, 8) << "\n"
                << "    sete %al\n"
                << "    movzbq %al, %" << res << "\n";
            noteType(res, boolType);

            alloc.free(r);
            return res;
        }

        if (child->type != NodeType::VariableAccess)
            throw std::runtime_error(op + " can only be applied to variables");

        auto &scp = *child->scope;
        const std::string varName = child->value;
        TypeInfo ti = scp.lookupType(scp.lookupVariable(varName).type);
        if (ti.isFloat)
            throw std::runtime_error(op + " not supported on float");

        std::string ptr = scp.isGlobalVariable(varName)
                              ? varName + "(%rip)"
                              : std::to_string(scp.getVariableOffset(varName)) + "(%rbp)";
        uint64_t sz = ti.bits / 8;
        char suf = integer_suffixes.at(ti.bits);

        auto r = allocateOrSpill(false, node->scope, out);
        std::string adj = adjustReg(r, ti.bits);
        out << "    " << getCorrectMove(sz, false) << " " << ptr << ", %" << adj << "\n";

        out << "    " << (op == "++" ? "inc" : "dec") << suf << " %" << adj << "\n";

        out << "    " << getCorrectMove(sz, false) << " %" << adj << ", " << ptr << "\n";

        noteType(adj, ti);
        return adj;
    }
    std::string CodeGenLinux::emitExpression(std::unique_ptr<ASTNode> node, std::ostringstream &out) {
        switch (node->type) {
        case NodeType::IntegerLiteral:
            return generateIntegerLiteral(std::move(node), out);
        case NodeType::FloatLiteral:
            return generateFloatLiteral(std::move(node), out);
        case NodeType::StringLiteral:
            return generateStringLiteral(std::move(node), out);
        case NodeType::BooleanLiteral:
            return generateBooleanLiteral(std::move(node), out);
        case NodeType::VariableAccess:
            return generateVariableAccess(std::move(node), out);
        case NodeType::BinaryOp:
            return generateBinaryOperation(std::move(node), out);
        case NodeType::UnaryOp:
            return generateUnaryOperation(std::move(node), out);
        case NodeType::FunctionCall:
            return generateFunctionCall(std::move(node), out);
        default:
            throw std::runtime_error("Unknown statement encountered.");
        }
    }

    void CodeGenLinux::emitPrologue(std::shared_ptr<ScopeContext> scope, std::ostringstream &out) {
        if (scope->kind() == "Function") {
            out << "    # Function Prologue\n";
            out << "    push   %rbp\n";
            out << "    mov    %rsp, %rbp\n";

            auto funcScope = std::dynamic_pointer_cast<FunctionScope>(scope);
            if (!funcScope)
                throw std::runtime_error("Expected FunctionScope for function prologue");

            size_t regsCount = CALLEE_GPR_LINUX.size();
            std::uint64_t spillSize = std::abs(funcScope->getSpillSize());
            std::uint64_t localSize = std::abs(funcScope->getStackOffset());
            std::uint64_t rawSize = 8 + localSize + spillSize;
            std::uint64_t stackReserve = (rawSize + 15) & ~15ULL;

            std::string canaryReg = allocateOrSpill(false, scope, out);
            out << std::hex
                << "    movabs $0x" << funcScope->getCanary() << ", %" << canaryReg << "    # load canary\n"
                << std::dec
                << "    movq   %" << canaryReg << ", -8(%rbp)    # store canary at [rbp - 8]\n";
            alloc.free(canaryReg);

            if (stackReserve > 0) {
                out << "    sub    $" << stackReserve << ", %rsp    # reserve locals + spills\n";
            }

            for (const auto &reg : CALLEE_GPR_LINUX) {
                out << "    push   %" << reg << "    # save callee-saved GPR\n";
            }
            if (regsCount & 1) {
                out << "    push   %" << CALLEE_GPR_LINUX.back() << "    # just for alignment\n";
            }
        }
    }
    void CodeGenLinux::emitEpilogue(std::shared_ptr<ScopeContext> scope, std::ostringstream &out, bool clearRax) {
        if (scope->kind() == "Function") {
            out << "    # Function Epilogue with Canary Check\n";

            auto funcScope = std::dynamic_pointer_cast<FunctionScope>(scope);
            if (!funcScope)
                throw std::runtime_error("Expected FunctionScope for function epilogue");

            size_t regsCount = CALLEE_GPR_LINUX.size();
            std::uint64_t spillSize = std::abs(funcScope->getSpillSize());
            std::uint64_t localSize = std::abs(funcScope->getStackOffset());
            std::uint64_t rawSize = 8 + localSize + spillSize;
            std::uint64_t stackReserve = (rawSize + 15) & ~15ULL;

            std::string regStored = allocateOrSpill(false, scope, out);
            std::string regExpected = allocateOrSpill(false, scope, out);
            out << "    movq   -8(%rbp), %" << regStored << "    # load stored canary\n"
                << std::hex
                << "    movabs $0x" << funcScope->getCanary() << ", %" << regExpected << "    # load expected canary\n"
                << std::dec
                << "    cmp    %" << regExpected << ", %" << regStored << "    # compare canary\n"
                << "    jne    __stack_smash_detected    # abort on mismatch\n";
            alloc.free(regStored);
            alloc.free(regExpected);

            if (stackReserve > 0) {
                out << "    add    $" << stackReserve << ", %rsp    # free locals + spills\n";
            }

            if (regsCount & 1) {
                out << "    pop    %" << CALLEE_GPR_LINUX.back() << "    # pop alignment\n";
            }

            for (auto it = CALLEE_GPR_LINUX.rbegin(); it != CALLEE_GPR_LINUX.rend(); ++it) {
                out << "    pop    %" << *it << "    # restore callee-saved GPR\n";
            }

            if (clearRax) {
                out << "    xor    %rax, %rax\n";
            }

            out << "    leave    # restore RSP, pop RBP\n";
            out << "    ret\n";
        }
    }

    void CodeGenLinux::generateStatement(std::unique_ptr<ASTNode> statement, std::ostringstream &out) {
        switch (statement->type) {
        case NodeType::VariableReassignment: {
            generateVariableReassignment(std::move(statement), out);
            out << "\n";
            break;
        }
        case NodeType::VariableDeclaration: {
            generateVariableDeclaration(std::move(statement), out);
            out << "\n";
            break;
        }
        case NodeType::IfStatement: {
            generateIfStatement(std::move(statement), out);
            out << "\n";
            break;
        }
        case NodeType::UnaryOp: {
            std::string op = statement->value;
            auto scope = statement->scope;
            std::string reg = emitExpression(std::move(statement), out);
            restoreIfSpilled(reg, scope, out);
            if (op == "--" or op == "++") {
                alloc.free(reg);
            }
            out << "\n";
            break;
        }
        case NodeType::BinaryOp: {
            auto scope = statement->scope;
            std::string reg = emitExpression(std::move(statement), out);  // I am doing this just so the increments/decrements work in x + y-- -> this itself must not have any result, but y-- should still be effective.
            restoreIfSpilled(reg, scope, out);
            alloc.free(reg);
            out << "\n";
            break;
        }
        case NodeType::FunctionCall: {
            std::string reg = generateFunctionCall(std::move(statement), out);
            alloc.free(reg);
            out << "\n";
            break;
        }
        case NodeType::Function: {
            generateFunctionDeclaration(std::move(statement), out);
            out << "\n";
            break;
        }
        case NodeType::ExternFunction: {
            generateExternFunctionDeclaration(std::move(statement), out);
            out << "\n";
            break;
        }
        case NodeType::ReturnStatement: {
            generateReturnstatement(std::move(statement), out);
            out << "\n";
            break;
        }
        default:
            throw std::runtime_error("Unknown statement encountered.");
        }
    }
    void CodeGenLinux::generateVariableReassignment(std::unique_ptr<ASTNode> statement, std::ostringstream &out) {
        auto &scp = *statement->scope;
        auto nm = statement->value;
        auto ti = scp.lookupType(scp.lookupVariable(nm).type);
        uint64_t sz = ti.bits / 8;

        auto r = emitExpression(std::move(statement->children.back()), out);
        restoreIfSpilled(r, statement->scope, out);

        r = castValue(r, regType.at(r), ti, statement->scope, out);
        restoreIfSpilled(r, statement->scope, out);

        std::string addr = scp.isGlobalVariable(nm)
                               ? nm + "(%rip)"
                               : std::to_string(scp.getVariableOffset(nm)) + "(%rbp)";

        if (ti.isFloat) {
            out << "    " << (sz == 4 ? "movss %" : "movsd %") << r << ", " << addr << "\n";
        } else {
            std::string adj = adjustReg(r, ti.bits);
            out << "    " << getCorrectMove(sz, false) << " %" << adj << ", " << addr << "\n";
        }

        alloc.free(r);
    }
    void CodeGenLinux::generateVariableDeclaration(std::unique_ptr<ASTNode> statement, std::ostringstream &out) {
        auto &scp = *statement->scope;
        auto nm = statement->value;
        auto ti = scp.lookupType(scp.lookupVariable(nm).type);
        uint64_t sz = ti.bits / 8;

        auto emitStore = [&](const std::string &r) {
            if (ti.isFloat) {
                out << "    " << (sz == 4 ? "movss %" : "movsd %") << r << ", "
                    << (scp.isGlobalVariable(nm) ? nm + "(%rip)" : std::to_string(scp.getVariableOffset(nm)) + "(%rbp)")
                    << "\n";
            } else {
                std::string adj = adjustReg(r, ti.bits);
                out << "    " << getCorrectMove(sz, false) << " %" << adj << ", "
                    << (scp.isGlobalVariable(nm) ? nm + "(%rip)" : std::to_string(scp.getVariableOffset(nm)) + "(%rbp)")
                    << "\n";
            }
        };

        if (statement->children.size() >= 2) {
            auto r = emitExpression(std::move(statement->children.back()), out);
            restoreIfSpilled(r, statement->scope, out);
            r = castValue(r, regType.at(r), ti, statement->scope, out);
            restoreIfSpilled(r, statement->scope, out);
            emitStore(r);
            alloc.free(r);
        } else {
            if (ti.isFloat) {
                auto r_xmm = allocateOrSpill(true, statement->scope, out);
                out << "    pxor %" << r_xmm << ", %" << r_xmm << "\n";
                emitStore(r_xmm);
                alloc.free(r_xmm);
            } else {
                out << "    xorq %rax, %rax\n";
                out << "    mov" << integer_suffixes.at(ti.bits) << " %rax, "
                    << (scp.isGlobalVariable(nm) ? nm + "(%rip)" : std::to_string(scp.getVariableOffset(nm)) + "(%rbp)")
                    << "\n";
            }
        }
    }
    void CodeGenLinux::generateIfStatement(std::unique_ptr<ASTNode> statement, std::ostringstream &out) {
        int id = blockLabelCount++;
        std::string endLbl = ".Lend" + std::to_string(id);
        std::vector<std::string> elseLabels;

        // Precompute labels for each else-if / else branch
        ASTNode *branch = statement->getElseBranch();
        while (branch) {
            elseLabels.push_back(".Lelse" + std::to_string(blockLabelCount++));
            branch = branch->getElseBranch();
        }

        size_t elseIdx = 0;

        // IF branch
        auto condR = emitExpression(std::move(statement->children[0]), out);
        restoreIfSpilled(condR, statement->scope, out);
        out << "    cmpq $0, %" << condR << "\n";
        out << "    je " << (elseLabels.empty() ? endLbl : elseLabels[elseIdx]) << "\n";
        alloc.free(condR);

        auto ifBlock = std::move(statement->children[1]);
        emitPrologue(ifBlock->scope, out);
        for (auto &stmt : ifBlock->children) {
            generateStatement(std::move(stmt), out);
        }
        emitEpilogue(ifBlock->scope, out);
        out << "    jmp " << endLbl << "\n";

        // ELSE-IF / ELSE branches
        branch = statement->getElseBranch();
        while (branch) {
            out << elseLabels[elseIdx++] << ":\n";

            if (branch->type == NodeType::ElseIfStatement) {
                auto r2 = emitExpression(std::move(branch->children[0]), out);
                restoreIfSpilled(r2, branch->scope, out);
                out << "    cmpq $0, %" << r2 << "\n";
                out << "    je " << (elseIdx < elseLabels.size() ? elseLabels[elseIdx] : endLbl) << "\n";
                alloc.free(r2);

                auto elifBlock = std::move(branch->children[1]);
                emitPrologue(elifBlock->scope, out);
                for (auto &stmt : elifBlock->children) {
                    generateStatement(std::move(stmt), out);
                }
                emitEpilogue(elifBlock->scope, out);
                out << "    jmp " << endLbl << "\n";

            } else if (branch->type == NodeType::ElseStatement) {
                auto elseBlock = std::move(branch->children[0]);
                emitPrologue(elseBlock->scope, out);
                for (auto &stmt : elseBlock->children) {
                    generateStatement(std::move(stmt), out);
                }
                emitEpilogue(elseBlock->scope, out);
            }

            branch = branch->getElseBranch();
        }

        out << endLbl << ":\n\n";
    }
    void CodeGenLinux::generate(std::unique_ptr<ASTNode> program) {
        std::vector<ASTNode *> globals;

        // Collect global variable declarations
        for (auto &statement : program->children) {
            if (statement->type == NodeType::VariableDeclaration) {
                globals.push_back(statement.get());
            }
        }

        // Emit .data section for globals
        outGlobalStream << ".data\n\n";
        for (auto *g : globals) {
            TypeInfo info = g->scope->lookupType(g->children[0]->value);
            switch (info.bits / 8) {
            case 8:
                outGlobalStream << g->value << ": .quad 0\n";
                break;
            case 4:
                outGlobalStream << g->value << ": .long 0\n";
                break;
            case 2:
                outGlobalStream << g->value << ": .word 0\n";
                break;
            case 1:
                outGlobalStream << g->value << ": .byte 0\n";
                break;
            default:
                throw std::runtime_error("Unsupported global size: " + std::to_string(info.bits / 8));
            }
        }

        // Emit rodata and stack protection code
        outGlobalStream << "\n.section .rodata\n";

        outStream << ".text\n";
        outStream << ".globl __stack_smash_detected\n";
        outStream << "__stack_smash_detected:\n";
        outStream << "    mov $60, %rax # syscall: exit\n";
        outStream << "    mov $69, %rdi # exit code\n";
        outStream << "    syscall\n";

        // Entry point
        outStream << ".global main\n";

        std::unique_ptr<ASTNode> mainFunction;
        std::vector<std::unique_ptr<ASTNode>> declarationsAndReassignments;

        // Generate code for all top-level statements
        for (auto &statement : program->children) {
            if (statement->type == NodeType::Function and statement->value == "main") {
                mainFunction = std::move(statement);
            } else if (statement->type == NodeType::VariableDeclaration || statement->type == NodeType::VariableReassignment || (statement->type == NodeType::UnaryOp and (statement->value == "++" || statement->value == "--"))) {
                declarationsAndReassignments.push_back(std::move(statement));
            } else {
                generateStatement(std::move(statement), outStream);
            }
        }

        outStream << "main:\n";
        for (auto &s : declarationsAndReassignments) {
            generateStatement(std::move(s), outStream);
        }
        generateFunctionDeclaration(std::move(mainFunction), outStream, true);

        // Final output
        outfinal << outGlobalStream.str()
                 << "\n# ============== Globals End Here ==============\n"
                 << outStream.str() << "\n";
    }

    std::string CodeGenLinux::generateFunctionCall(std::unique_ptr<ASTNode> node, std::ostringstream &out) {
        auto fnInfo = node->scope->lookupFunction(node->value);
        const std::string &label = fnInfo.isExtern ? node->value : fnInfo.label;
        auto &args = node->children[0]->children;
        const auto &functionParams = fnInfo.paramTypes;

        // --- Save caller-saved GPRs ---
        std::vector<std::string> savedGPR;
        for (const auto &reg : CALLER_GPR_LINUX) {
            if (alloc.isInUse(reg)) {
                out << "    push   %" << reg << "    # save caller-saved GPR\n";
                savedGPR.push_back(reg);
            }
        }

        // --- Save caller-saved XMMs ---
        std::vector<std::pair<std::string, std::string>> savedXMM;
        for (const auto &reg : CALLER_XMM_LINUX) {
            if (alloc.isInUseXMM(reg)) {
                auto funcScope = std::static_pointer_cast<FunctionScope>(node->scope->findEnclosingFunctionScope());
                std::string slot = funcScope->allocateSpillSlot(16, CodegenOutputFormat::X86_64_LINUX);
                alloc.markSpilledXMM(reg, slot);
                out << "    movdqu %" << reg << ", " << slot << "    # save caller-saved XMM\n";
                savedXMM.emplace_back(reg, slot);
            }
        }

        // calculate stack space for overflow arguments
        uint64_t gpCount = 0, xmmCount = 0, stackOffset = 0;
        for (size_t i = 0; i < args.size(); ++i) {
            bool isFloat;
            if (i < functionParams.size()) {
                isFloat = node->scope->lookupType(functionParams[i].type).isFloat;
            } else if (fnInfo.isVariadic) {
                // Promote variadic argument based on the actual passed type
                // We cannot know actual float/int until expression emitted; assume float if expression type is float
                // For stack-space calculation, we'll conservatively treat variadic floats as double
                // so count toward xmmCount
                // Here, default to integer (non-float) stack slot; actual float moved will go to xmm later
                isFloat = false;
            } else {
                throw std::runtime_error("Too many arguments passed to non-variadic function '" + node->value + "'");
            }

            if (!isFloat) {
                if (gpCount < ARG_GPR_LINUX.size())
                    gpCount++;
                else
                    stackOffset += 8;
            } else {
                if (xmmCount < ARG_XMM_LINUX.size())
                    xmmCount++;
                else
                    stackOffset += 8;
            }
        }
        uint64_t stackReserve = (stackOffset + 15) & ~15ULL;
        if (stackReserve)
            out << "    sub    $" << stackReserve << ", %rsp    # reserve stack for args\n";

        // emit argument moves
        gpCount = xmmCount = 0;
        stackOffset = 0;
        std::vector<std::string> reservedArgumentRegs;
        std::vector<std::string> spilledArgumentRegs;

        for (size_t i = 0; i < args.size(); ++i) {
            std::string src = emitExpression(std::move(args[i]), out);
            restoreIfSpilled(src, node->scope, out);

            TypeInfo passed = regType.at(src);
            bool passedIsFloat = passed.isFloat;

            TypeInfo expect;
            if (i < functionParams.size()) {
                expect = node->scope->lookupType(functionParams[i].type);
            } else if (fnInfo.isVariadic) {
                if (passedIsFloat) {
                    expect = node->scope->lookupType("double");
                } else {
                    expect = node->scope->lookupType("int64_t");
                }
            } else {
                throw std::runtime_error("Too many arguments passed to non-variadic function '" + node->value + "'");
            }

            std::string cvt = castValue(src, passed, expect, node->scope, out);
            restoreIfSpilled(cvt, node->scope, out);

            bool isFloatArg = expect.isFloat;
            if (!isFloatArg && gpCount < ARG_GPR_LINUX.size()) {
                std::string dst = ARG_GPR_LINUX[gpCount];
                if (alloc.isInUseArgument(dst)) {
                    auto funcScope = std::static_pointer_cast<FunctionScope>(node->scope->findEnclosingFunctionScope());
                    std::string slot = funcScope->allocateSpillSlot(8, CodegenOutputFormat::X86_64_LINUX);
                    alloc.markSpilled(dst, slot);
                    spilledArgumentRegs.push_back(dst);
                    out << "    movq   %" << dst << ", " << slot << "    # spill GPR\n";
                } else {
                    dst = alloc.allocateArgument(gpCount);
                    reservedArgumentRegs.emplace_back(dst);
                }
                gpCount++;
                out << "    movq   %" << cvt << ", %" << dst << "\n";
            } else if (isFloatArg && xmmCount < ARG_XMM_LINUX.size()) {
                std::string dst = ARG_XMM_LINUX[xmmCount];
                if (alloc.isInUseArgumentXMM(dst)) {
                    auto funcScope = std::static_pointer_cast<FunctionScope>(node->scope->findEnclosingFunctionScope());
                    std::string slot = funcScope->allocateSpillSlot(16, CodegenOutputFormat::X86_64_LINUX);
                    alloc.markSpilledXMM(dst, slot);
                    spilledArgumentRegs.push_back(dst);
                    out << "    movdqu %" << dst << ", " << slot << "    # spill XMM\n";
                } else {
                    dst = alloc.allocateArgumentXMM(xmmCount);
                    reservedArgumentRegs.emplace_back(dst);
                }
                xmmCount++;
                out << "    movsd  %" << cvt << ", %" << dst << "\n";
            } else {
                out << "    mov" << (isFloatArg ? "ss  %" : "q   %") << cvt << ", " << stackOffset << "(%rsp)\n";
                stackOffset += 8;
            }
            alloc.free(cvt);
        }

        // tell ABI how many float args are in XMM regs
        out << "    mov    $" << xmmCount << ", %rax    # variadic ABI float count\n";

        // --- Call ---
        out << "    call   " << label << "    # function call\n";

        for (std::string &reg : spilledArgumentRegs) {
            restoreIfSpilled(reg, node->scope, out);
        }

        for (std::string &reg : reservedArgumentRegs) {
            alloc.freeArgument(reg);
        }

        // restore caller-saved XMMs
        for (auto it = savedXMM.rbegin(); it != savedXMM.rend(); ++it) {
            out << "    movdqu " << it->second << ", %" << it->first << "    # restore XMM\n";
        }
        // restore caller-saved GPRs
        for (auto it = savedGPR.rbegin(); it != savedGPR.rend(); ++it) {
            out << "    pop    %" << *it << "    # restore GPR\n";
        }

        // restore stack
        if (stackReserve)
            out << "    add    $" << stackReserve << ", %rsp    # free args stack\n";

        // return value
        bool isFloatRet = fnInfo.returnType == "float" || fnInfo.returnType == "double";
        std::string holder = allocateOrSpill(isFloatRet, node->scope, out);
        std::string abiReg = isFloatRet ? "xmm0" : "rax";
        noteType(holder, node->scope->lookupType(fnInfo.returnType));
        if (isFloatRet) {
            const char *mov = fnInfo.returnType == "float" ? "movss" : "movsd";
            out << "    " << mov << " %" << abiReg << ", %" << holder << "\n";
        } else {
            out << "    movq   %" << abiReg << ", %" << holder << "\n";
        }
        return holder;
    }
    void CodeGenLinux::generateFunctionDeclaration(std::unique_ptr<ASTNode> node, std::ostringstream &out, bool force) {
        if (node->value == "main" && !force) {
            return;
        }
        auto fnInfo = node->scope->lookupFunction(node->value);
        auto bodyNode = node->getFunctionBody();
        auto funcScope = std::dynamic_pointer_cast<FunctionScope>(
            bodyNode->scope->findEnclosingFunctionScope());

        // Function label and prologue with canary and stack frame setup
        if (!force) {
            out << fnInfo.label << ":\n";
        }
        std::ostringstream prologue;
        std::ostringstream body;

        // Canary setup
        std::uint64_t canary = CanaryGenerator::generate();
        funcScope->setCanary(canary);

        std::vector<std::unique_ptr<ASTNode>> nestedFunctions;

        // SysV argument register order
        const auto &ARG_GPR_LINUX = alloc.availableArgumentRegs;     // {"rdi","rsi","rdx","rcx","r8","r9"}
        const auto &ARG_XMM_LINUX = alloc.availableArgumentRegsXMM;  // {"xmm0","xmm1",...,"xmm7"}

        auto &params = fnInfo.paramTypes;
        size_t gpIdx = 0, xmmIdx = 0, stackArgOffset = 0;

        // Move incoming args into their stack slots
        for (size_t i = 0; i < params.size(); ++i) {
            const auto &p = params[i];
            TypeInfo ti = node->scope->lookupType(p.type);
            bool isFloat = ti.isFloat;
            int64_t slotOff = bodyNode->scope->getVariableOffset(p.name);

            // Determine correct mov instruction
            std::string movInst;
            if (isFloat) {
                movInst = (ti.bits == 64) ? "movsd" : "movss";
            } else {
                movInst = "mov" + std::string(1, integer_suffixes.at(64));  // "movq"
            }

            if (!isFloat && gpIdx < ARG_GPR_LINUX.size()) {
                // integer in GPR
                body << "    " << movInst
                     << " %" << ARG_GPR_LINUX[gpIdx++]  // prefix register
                     << ", " << std::to_string(slotOff) << "( %rbp)\n";

            } else if (isFloat && xmmIdx < ARG_XMM_LINUX.size()) {
                // float in XMM reg
                body << "    " << movInst
                     << " %" << ARG_XMM_LINUX[xmmIdx++]
                     << ", " << std::to_string(slotOff) << "( %rbp)\n";

            } else {
                // spilled argument on caller's stack
                uint64_t callerDisp = 16 + stackArgOffset;
                body << "    " << movInst
                     << " " << callerDisp << "(%rbp)"
                     << ", " << std::to_string(slotOff) << "( %rbp)\n";
                stackArgOffset += 8;
            }
        }

        // Generate the function body
        for (auto &stmt : bodyNode->children) {
            if (stmt->type == NodeType::Function) {
                nestedFunctions.push_back(std::move(stmt));  // defer
            } else {
                generateStatement(std::move(stmt), body);
            }
        }
        emitPrologue(funcScope, prologue);
        if (bodyNode->scope->returnType == "none") {
            emitEpilogue(funcScope, body, force);
        }
        out << prologue.str() + body.str();
        for (auto &nestedFn : nestedFunctions) {
            generateFunctionDeclaration(std::move(nestedFn), out, false);
        }
    }
    void CodeGenLinux::generateExternFunctionDeclaration(std::unique_ptr<ASTNode> node, std::ostringstream &out) {
        auto fnInfo = node->scope->lookupFunction(node->value);
        const std::string &label = node->value;
        outGlobalStream << "    .global  " << label << "\n";
        outGlobalStream << "    .type   " << label << ", @function\n";
    }
    void CodeGenLinux::generateReturnstatement(std::unique_ptr<ASTNode> node, std::ostringstream &out) {
        // TODO: Ensure that we are not returning a local type, at this point I assume that typechecker handles this.
        std::string result = emitExpression(std::move(node->children[0]), out);

        std::shared_ptr<FunctionScope> funcScope = node->scope->findEnclosingFunctionScope();
        restoreIfSpilled(result, funcScope, out);

        TypeInfo actual = regType.at(result);
        TypeInfo expected = funcScope->lookupType(funcScope->returnType);
        std::string casted = castValue(result, actual, expected, funcScope, out);
        restoreIfSpilled(casted, funcScope, out);

        std::string retReg = expected.isFloat ? "xmm0" : "rax";

        if (expected.name == "none") {
            out << "    xor %rax, %rax\n";
        } else if (!expected.isFloat) {
            out << "    mov" << integer_suffixes.at(64)
                << " %" << casted << ", %" << retReg << "\n";
        } else {
            const char *mov = (expected.bits == 32 ? "movss" : "movsd");
            out << "    " << mov
                << " %" << casted << ", %" << retReg << "\n";
        }
        alloc.free(casted);
        emitEpilogue(funcScope, out);
    }
}  // namespace zlang

// TODO: Dont nest the code generation of nested functions.
// This leads to disasters.