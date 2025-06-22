#include "all.hpp"

namespace zlang {
    std::string CodeGenWindows::castValue(const std::string &val,
                                          const TypeInfo &fromType,
                                          const TypeInfo &toType, const std::shared_ptr<ScopeContext> currentScope, std::ostringstream &out) {
        restoreIfSpilled(val, currentScope, out);

        if (fromType.bits == toType.bits && fromType.isFloat == toType.isFloat) {
            if (!fromType.isFloat && fromType.bits == toType.bits &&
                fromType.isSigned != toType.isSigned) {
                std::string dst = allocateOrSpill(false, currentScope, out);
                out << "    mov " << adjustReg(dst, fromType.bits)
                    << ", " << adjustReg(val, fromType.bits) << "\n";
                alloc.free(val);
                noteType(dst, toType);
                return dst;
            }
            return RegisterAllocator::getBaseReg(val);
        }

        if (fromType.isFloat && toType.isFloat) {
            std::string dst = allocateOrSpill(true, currentScope, out);
            auto instr = (fromType.bits < toType.bits)
                             ? "cvtss2sd"
                             : "cvtsd2ss";
            out << "    " << instr << " " << dst << ", " << val << "\n";
            alloc.free(val);
            noteType(dst, toType);
            return dst;
        }

        if (!fromType.isFloat && toType.isFloat) {
            std::string dst = allocateOrSpill(true, currentScope, out);
            auto instr = (toType.bits == 32)
                             ? "cvtsi2ss"
                             : "cvtsi2sd";
            out << "    " << instr << " " << dst
                << ", " << adjustReg(val, fromType.bits) << "\n";
            alloc.free(val);
            noteType(dst, toType);
            return dst;
        }

        if (fromType.isFloat && !toType.isFloat) {
            std::string dst = allocateOrSpill(false, currentScope, out);
            auto instr = (fromType.bits == 32)
                             ? "cvttss2si"
                             : "cvttsd2si";
            out << "    " << instr << " " << dst << ", " << val << "\n";
            alloc.free(val);
            noteType(dst, toType);
            return dst;
        }

        if (!fromType.isFloat && !toType.isFloat) {
            std::string dst = allocateOrSpill(false, currentScope, out);
            std::string srcAdj = adjustReg(val, fromType.bits);
            std::string dstAdj = adjustReg(dst, toType.bits);

            if (toType.bits > fromType.bits) {
                // widening
                if (fromType.isSigned) {
                    // signed → use movsxd for 32→64, movsx otherwise
                    if (fromType.bits == 32 && toType.bits == 64) {
                        out << "    movsxd " << dstAdj << ", " << srcAdj << "\n";
                    } else {
                        out << "    movsx  " << dstAdj << ", " << srcAdj << "\n";
                    }
                } else {
                    // unsigned → use movzx for width ≤ 16, plain mov for 32→64
                    if (fromType.bits == 8 || fromType.bits == 16) {
                        out << "    movzx  " << dstAdj << ", " << srcAdj << "\n";
                    } else if (fromType.bits == 32 && toType.bits == 64) {
                        out << "    mov    " << dstAdj << "d, " << srcAdj << "\n";
                    } else {
                        throw std::runtime_error(
                            "Unsupported unsigned cast: " +
                            std::to_string(fromType.bits) +
                            " -> " +
                            std::to_string(toType.bits)
                        );
                    }
                }

            } else if (toType.bits < fromType.bits) {
                out << "    mov    " << dstAdj << ", " << adjustReg(val, toType.bits) << "\n";

            } else {
                // same size — plain mov
                out << "    mov    " << dstAdj << ", " << srcAdj << "\n";
            }

            alloc.free(val);
            noteType(dst, toType);
            return dst;
        }


        throw std::runtime_error(
            "Unsupported cast from " +
            fromType.to_string() +
            " to " +
            toType.to_string());
    }

    std::string CodeGenWindows::allocateOrSpill(bool isXMM, std::shared_ptr<ScopeContext> scope, std::ostringstream &out) {
        try {
            std::string reg = isXMM ? alloc.allocateXMM() : alloc.allocate();
            if (!reg.empty())
                return reg;
        } catch (...) {
        }

        std::shared_ptr<FunctionScope> result_scope = scope->findEnclosingFunctionScope();
        if (!result_scope)
            throw std::runtime_error("Cannot spill in global context");

        std::shared_ptr<FunctionScope> funcScope = std::static_pointer_cast<FunctionScope>(result_scope);
        std::string victim = isXMM ? alloc.pickVictimXMM() : alloc.pickVictim();
        if (victim.empty())
            throw std::runtime_error("No available registers and no victim found");

        std::string spillSlot =
            funcScope->allocateSpillSlot(isXMM ? 16 : 8,
                                         CodegenOutputFormat::X86_64_MSWIN);

        if (isXMM) {
            alloc.markSpilledXMM(victim, spillSlot);
            out << "    movdqu " << victim
                << ", XMMWORD PTR " << spillSlot
                << "    ; Spill XMM\n";
        } else {
            alloc.markSpilled(victim, spillSlot);
            out << "    mov " << victim
                << ", QWORD PTR " << spillSlot
                << "    ; Spill GPR\n";
        }

        return victim;
    }
    void CodeGenWindows::restoreIfSpilled(const std::string &reg, std::shared_ptr<ScopeContext> scope, std::ostringstream &out) {
        if (!alloc.isSpilled(reg))
            return;

        std::string slot = alloc.spillSlotFor(reg);

        if (reg.rfind("xm", 0) == 0) {
            alloc.unSpillXMM(reg,
                             CodegenOutputFormat::X86_64_MSWIN,
                             out);
        } else {
            alloc.unSpill(reg,
                          CodegenOutputFormat::X86_64_MSWIN,
                          out);
        }

        auto result_scope = scope->findEnclosingFunctionScope();
        if (result_scope) {
            auto funcScope =
                std::static_pointer_cast<FunctionScope>(result_scope);
            funcScope->freeSpillSlot(
                slot,
                reg.rfind("xm", 0) == 0 ? 16 : 8);
        }
    }

    std::string CodeGenWindows::generateIntegerLiteral(std::unique_ptr<ASTNode> node, std::ostringstream &out) {
        TypeInfo ti = node->scope->lookupType("int64_t");
        std::string adj = allocateOrSpill(false, node->scope, out);
        out << "    mov " << adj << ", " << node->value << "\n";
        noteType(adj, ti);
        return adj;
    }

    std::string CodeGenWindows::generateFloatLiteral(std::unique_ptr<ASTNode> node, std::ostringstream &out) {
        std::string lbl = "Lfloat" + std::to_string(floatLabelCount++);
        std::string val = node->value;
        bool isF32 = (!val.empty() && (val.back() == 'f' || val.back() == 'F'));
        if (isF32)
            val.pop_back();
        outGlobalStream
            << "    ALIGN " << (isF32 ? 4 : 8) << "\n"
            << lbl << " "
            << (isF32 ? "dd " : "dq ")
            << val << "\n";
        TypeInfo floatType = isF32
                                 ? node->scope->lookupType("float")
                                 : node->scope->lookupType("double");
        std::string r_xmm = allocateOrSpill(true, node->scope, out);
        out
            << "    " << (isF32 ? "movss " : "movsd ")
            << r_xmm << ", "
            << (isF32 ? "DWORD PTR " : "QWORD PTR ")
            << lbl << "\n";

        noteType(r_xmm, floatType);
        return r_xmm;
    }
    std::string CodeGenWindows::generateStringLiteral(
        std::unique_ptr<ASTNode> node,
        std::ostringstream &out
    ) {
        // 1) Create a unique label
        std::string lbl = "Lstr" + std::to_string(stringLabelCount++);

        // 2) Raw value from the AST (e.g. contains "\n", "\"", etc.)
        const std::string &raw = node->value;

        // 3) Split into printable chars and numeric bytes
        std::string printable;
        std::vector<unsigned char> nums;

        for (size_t i = 0; i < raw.size(); ++i) {
            if (raw[i] == '\\' && i + 1 < raw.size()) {
                char esc = raw[++i];
                switch (esc) {
                    case 'n':  nums.push_back(0x0A); break;
                    case 't':  nums.push_back(0x09); break;
                    case '\\': printable.push_back('\\'); break;
                    case '"':  printable.push_back('"');  break;
                    case '0':  nums.push_back(0x00); break;
                    default:
                        // Unknown escape: emit literally
                        printable.push_back('\\');
                        printable.push_back(esc);
                }
            } else {
                printable.push_back(raw[i]);
            }
        }
        // Always null-terminate
        nums.push_back(0x00);

        // 4) Emit the data declaration
        outGlobalStream << "    ALIGN 1\n";
        outGlobalStream << lbl << " db ";

        // Emit the quoted printable portion if non-empty
        if (!printable.empty()) {
            outGlobalStream << "\"";
            for (unsigned char c : printable) {
                if (c == '"')       outGlobalStream << "\"\"";   // MASM doubles quotes
                else if (c == '\\') outGlobalStream << "\\\\"; // literal backslash
                else                outGlobalStream << c;
            }
            outGlobalStream << "\"";

            if (!nums.empty()) {
                outGlobalStream << ",";
            }
        }

        // Emit numeric bytes (e.g. 0Ah for newline, 00h for terminator)
        for (size_t i = 0; i < nums.size(); ++i) {
            unsigned int b = nums[i];
            outGlobalStream
                << std::uppercase
                << std::hex
                << "0" << b << "h"
                << std::dec;
            if (i + 1 < nums.size()) {
                outGlobalStream << ",";
            }
        }
        outGlobalStream << "\n\n";

        // 5) Generate the code to load its address
        TypeInfo strType = node->scope->lookupType("string");
        std::string r = allocateOrSpill(false, node->scope, out);
        out << "    lea " << r << ", OFFSET " << lbl << "\n";
        noteType(r, strType);
        return r;
    }

    std::string CodeGenWindows::generateBooleanLiteral(std::unique_ptr<ASTNode> node, std::ostringstream &out) {
        TypeInfo boolType = node->scope->lookupType("boolean");
        std::string r = allocateOrSpill(false, node->scope, out);
        out
            << "    mov " << r << ", "
            << (node->value == "true" ? "1" : "0") << "\n";

        noteType(r, boolType);
        return r;
    }
    std::string CodeGenWindows::generateVariableAccess(std::unique_ptr<ASTNode> node, std::ostringstream &out) {
        auto &scope = *node->scope;
        std::string name = node->value;
        TypeInfo ti = scope.lookupType(scope.lookupVariable(name).type);
        uint64_t sz = ti.bits / 8;
        bool isGlobal = scope.isGlobalVariable(name);

        auto ptrQualifier = [&](uint64_t bytes) {
            switch (bytes) {
            case 8:
                return "QWORD PTR ";
            case 4:
                return "DWORD PTR ";
            case 2:
                return "WORD PTR ";
            case 1:
                return "BYTE PTR ";
            default:
                return "";
            }
        };

        if (ti.isFloat) {
            std::string r_xmm = allocateOrSpill(true, node->scope, out);
            std::string instr = (sz == 4 ? "movss" : "movsd");
            std::string qualifier = ptrQualifier(sz);

            if (isGlobal) {
                out << "    "
                    << instr << " "
                    << r_xmm << ", "
                    << qualifier << "[" << name << "]"
                    << "\n";
            } else {
                int64_t off = std::abs(scope.getVariableOffset(name));
                out << "    "
                    << instr << " "
                    << r_xmm << ", "
                    << qualifier << "[rbp - " << off << "]"
                    << "\n";
            }

            noteType(r_xmm, ti);
            return r_xmm;
        } else {
            std::string r = allocateOrSpill(false, node->scope, out);
            std::string adj = adjustReg(r, ti.bits);
            std::string qualifier = ptrQualifier(sz);

            if (isGlobal) {
                out << "    "
                    << "mov "
                    << adj << ", "
                    << qualifier << "[" << name << "]"
                    << "\n";
            } else {
                int64_t off = std::abs(scope.getVariableOffset(name));
                out << "    "
                    << "mov "
                    << adj << ", "
                    << qualifier << "[rbp - " << off << "]"
                    << "\n";
            }

            noteType(r, ti);
            return r;
        }
    }
    std::string CodeGenWindows::generateBinaryOperation(std::unique_ptr<ASTNode> node, std::ostringstream &out) {
        // 1. Evaluate left operand
        std::string rl = emitExpression(std::move(node->children[0]), out);
        restoreIfSpilled(rl, node->scope, out);

        // 2. Evaluate right operand
        std::string rr = emitExpression(std::move(node->children[1]), out);
        restoreIfSpilled(rr, node->scope, out);

        // 3. Figure out result type and operator
        TypeInfo t1 = regType.at(rl);
        TypeInfo t2 = regType.at(rr);
        TypeInfo tr = TypeChecker::promoteType(t1, t2);
        const std::string &op = node->value;

        // 4. Cast both sides to the common type
        rl = castValue(rl, t1, tr, node->scope, out);
        restoreIfSpilled(rl, node->scope, out);
        rr = castValue(rr, t2, tr, node->scope, out);
        restoreIfSpilled(rr, node->scope, out);

        // 5. Floating‑point operations?
        if (tr.isFloat) {
            bool isF32 = (tr.bits == 32);
            std::string vsuf = isF32 ? "ss" : "sd";  // movss/movsd, addss/addsd...
            std::string instr;
            if (op == "+")
                instr = "add" + vsuf;
            else if (op == "-")
                instr = "sub" + vsuf;
            else if (op == "*")
                instr = "mul" + vsuf;
            else if (op == "/")
                instr = "div" + vsuf;
            else if (assembly_comparison_operations.count(op)) {
                // unordered compare + setcc
                std::string ucom = "ucomi" + vsuf;  // ucomiss / ucomisd
                out << "    " << ucom << " " << rl << ", " << rr << "\n"
                    << "    " << assembly_comparison_operations.at(op) << " al\n";
                // alloc bool
                TypeInfo boolType = node->scope->lookupType("boolean");
                std::string r_bool = allocateOrSpill(false, node->scope, out);
                out << "    movzx " << r_bool << ", al\n";
                noteType(r_bool, boolType);
                alloc.free(rr);
                alloc.free(rl);
                return r_bool;
            } else {
                throw std::runtime_error("Unsupported FP op " + op);
            }

            // binary fp op
            out << "    " << instr << " " << rl << ", " << rr << "\n";
            alloc.free(rr);
            noteType(rl, tr);
            return rl;
        }

        // 6. Integer operations
        char suf = integer_suffixes.at(tr.bits);  // e.g. 'q' for 64, 'l' for 32
        std::string adj_l = adjustReg(rl, tr.bits);
        std::string adj_r = adjustReg(rr, tr.bits);

        if (op == "+" || op == "-" || op == "*" || op == "/") {
            if (op == "+") {
                out << "    add" << " " << adj_l << ", " << adj_r << "\n";
            } else if (op == "-") {
                out << "    sub" << " " << adj_l << ", " << adj_r << "\n";
            } else if (op == "*") {
                out << "    imul" << " " << adj_l << ", " << adj_r << "\n";
            } else {
                // division
                if (tr.isSigned) {
                    // signed: RDX:RAX ← RAX / src
                    out << "    mov" << " rax, " << adj_l << "\n"
                        << "    cqo\n"  // sign‑extend RAX→RDX:RAX
                        << "    idiv" << " " << adj_r << "\n"
                        << "    mov" << " " << adj_l << ", rax\n";
                } else {
                    // unsigned
                    out << "    mov" << " rax, " << adj_l << "\n"
                        << "    xor rdx, rdx\n"
                        << "    div" << " " << adj_r << "\n"
                        << "    mov" << " " << adj_l << ", rax\n";
                }
                alloc.free(rr);
                noteType(rl, tr);
                return rl;
            }

            alloc.free(rr);
            noteType(rl, tr);
            return rl;
        }

        // 7. Integer comparison?
        if (assembly_comparison_operations.count(op)) {
            std::string cmpInstr = "cmp";
            out << "    " << cmpInstr << " " << adj_l << ", " << adj_r << "\n"
                << "    " << assembly_comparison_operations.at(op) << " al\n";
            TypeInfo boolType = node->scope->lookupType("boolean");
            std::string r_bool = allocateOrSpill(false, node->scope, out);
            out << "    movzx " << r_bool << ", al\n";
            alloc.free(rr);
            alloc.free(rl);
            return r_bool;
        }

        // 8. Bitwise/logical ops: &&, ||, &, |
        if (op == "&&" || op == "||" || op == "&" || op == "|") {
            std::string instr = (op == "&&" ? "and" : op == "||" ? "or"
                                                                 : op);
            out << "    " << instr << suf << " " << adj_l << ", " << adj_r << "\n";
            alloc.free(rr);
            noteType(rl, tr);
            return rl;
        }

        throw std::runtime_error("Unsupported int op " + op);
    }
    std::string CodeGenWindows::generateUnaryOperation(std::unique_ptr<ASTNode> node, std::ostringstream &out) {
        const std::string &op = node->value;
        if (op != "!" && op != "++" && op != "--")
            throw std::runtime_error("Unsupported unary operator: " + op);

        if (op == "!") {
            // logical not
            std::string r = emitExpression(std::move(node->children[0]), out);
            restoreIfSpilled(r, node->scope, out);

            TypeInfo boolType = node->scope->lookupType("boolean");
            r = castValue(r, regType.at(r), boolType, node->scope, out);
            restoreIfSpilled(r, node->scope, out);

            std::string res = allocateOrSpill(false, node->scope, out);
            out << "    cmp BYTE PTR " << adjustReg(r, 8) << ", 0\n"
                << "    sete al\n"
                << "    movzx " << res << ", al\n";
            noteType(res, boolType);

            alloc.free(r);
            return res;
        }

        // ++ or -- must be applied to a variable
        auto child = std::move(node->children[0]);
        if (child->type != NodeType::VariableAccess)
            throw std::runtime_error(op + " can only be applied to variables");

        auto &scp = *child->scope;
        std::string varName = child->value;
        TypeInfo ti = scp.lookupType(scp.lookupVariable(varName).type);
        if (ti.isFloat)
            throw std::runtime_error(op + " not supported on float");

        // Compute address
        std::string ptr;
        if (scp.isGlobalVariable(varName)) {
            ptr = "[" + varName + "]";
        } else {
            int64_t off = std::abs(scp.getVariableOffset(varName));
            ptr = "[rbp - " + std::to_string(off) + "]";
        }
        // Load variable
        std::string r = allocateOrSpill(false, node->scope, out);
        std::string adj = adjustReg(r, ti.bits);
        out << "    mov" << " " << adj << ", " << ptr << "\n";

        // inc/dec
        out << "    " << (op == "++" ? "inc" : "dec") << " " << adj << "\n";

        // store back
        out << "    mov" << " " << ptr << ", " << adj << "\n";

        noteType(adj, ti);
        return adj;
    }
    std::string CodeGenWindows::emitExpression(std::unique_ptr<ASTNode> node, std::ostringstream &out) {
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

    std::string formatHex32(uint32_t value) {
        std::ostringstream oss;
        oss << std::hex << value;
        std::string s = oss.str();
        if (!s.empty()) {
            char first = s[0];
            if ((first >= 'a' && first <= 'f') || 
                (first >= 'A' && first <= 'F')) {
                s = "0" + s;
            }
        }
        return s + "h";
    }

    void CodeGenWindows::emitPrologue(std::shared_ptr<ScopeContext> scope, std::ostringstream &out) {
        if (scope->kind() != "Function")
            return;

        out << "    ; Function Prologue\n";
        out << "    push    rbp\n";
        out << "    mov     rbp, rsp\n";

        auto funcScope = std::dynamic_pointer_cast<FunctionScope>(scope);
        if (!funcScope)
            throw std::runtime_error("Expected FunctionScope for function prologue");

        // Compute sizes
        std::uint64_t spillSize = std::abs(funcScope->getSpillSize());
        std::uint64_t localSize = std::abs(funcScope->getStackOffset());
        std::uint64_t rawSize = 8 + localSize + spillSize;
        std::uint64_t stackReserve = (rawSize + 15) & ~15ULL;

        uint64_t canary = funcScope->getCanary();
        uint32_t low = static_cast<uint32_t>(canary);
        uint32_t high = static_cast<uint32_t>(canary >> 32);

        out << "    ; store canary as two 32-bit values\n";
        out << "    mov     DWORD PTR [rbp-8], " << formatHex32(low) << "\n";
        out << "    mov     DWORD PTR [rbp-4], " << formatHex32(high) << "\n";

        // Reserve stack for locals + spills
        if (stackReserve > 0) {
            out << "    sub     rsp, "
                << stackReserve
                << "    ; reserve locals + spills\n";
        }

        // Save callee-saved GPRs
        for (auto &reg : CALLEE_GPR_MSVC) {
            out << "    push    " << reg
                << "    ; save callee-saved GPR\n";
        }

        // 🔑 Align stack after odd number of GPR pushes
        if (CALLEE_GPR_MSVC.size() % 2 != 0) {
            out << "    sub     rsp, 8    ; align stack\n";
        }

        // Save callee-saved XMMs
        for (auto &reg : CALLEE_XMM_MSVC) {
            out << "    sub     rsp, 16\n";
            out << "    movdqu  [rsp], " << reg << "\n";
        }
    }
    void CodeGenWindows::emitEpilogue(std::shared_ptr<ScopeContext> scope, std::ostringstream &out, bool clearRax) {
        if (scope->kind() != "Function")
            return;

        out << "    ; Function Epilogue with Canary Check\n";

        auto funcScope = std::dynamic_pointer_cast<FunctionScope>(scope);
        if (!funcScope)
            throw std::runtime_error("Expected FunctionScope for function epilogue");

        // Compute sizes
        std::uint64_t spillSize = std::abs(funcScope->getSpillSize());
        std::uint64_t localSize = std::abs(funcScope->getStackOffset());
        std::uint64_t rawSize = 8 + localSize + spillSize;
        std::uint64_t stackReserve = (rawSize + 15) & ~15ULL;

        // Canary check
        uint64_t canary = funcScope->getCanary();
        uint32_t low_expected = static_cast<uint32_t>(canary);
        uint32_t high_expected = static_cast<uint32_t>(canary >> 32);

        std::string reg = adjustReg(allocateOrSpill(false, scope, out), 32);

        out << "    mov     " << reg << ", DWORD PTR [rbp-8]    ; load stored low\n";
        out << "    cmp     " << reg << ", " << formatHex32(low_expected) << "\n";
        out << "    jne     __stack_smash_detected\n";

        out << "    mov     " << reg << ", DWORD PTR [rbp-4]    ; load stored high\n";
        out << "    cmp     " << reg << ", " << formatHex32(high_expected) << "\n";
        out << "    jne     __stack_smash_detected\n";

        alloc.free(reg);

        // Restore XMM registers
        for (auto it = CALLEE_XMM_MSVC.rbegin(); it != CALLEE_XMM_MSVC.rend(); ++it) {
            out << "    movdqu  " << *it << ", [rsp]\n";
            out << "    add     rsp, 16\n";
        }

        // 🔑 Undo stack alignment after GPR pushes
        if (CALLEE_GPR_MSVC.size() % 2 != 0) {
            out << "    add     rsp, 8    ; undo alignment\n";
        }

        // Restore callee-saved GPRs in reverse
        for (auto it = CALLEE_GPR_MSVC.rbegin(); it != CALLEE_GPR_MSVC.rend(); ++it) {
            out << "    pop     " << *it
                << "    ; restore callee-saved GPR\n";
        }

        // Unwind stack frame
        if (stackReserve > 0) {
            out << "    add     rsp, "
                << stackReserve
                << "    ; free locals + spills\n";
        }

        // Zero RAX if required
        if (clearRax) {
            out << "    xor     rax, rax\n";
        }

        // Leave + ret
        out << "    leave    ; mov rsp, rbp & pop rbp\n";
        out << "    ret\n";
    }

    void CodeGenWindows::generateStatement(std::unique_ptr<ASTNode> statement, std::ostringstream &out) {
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
    void CodeGenWindows::generateVariableReassignment(std::unique_ptr<ASTNode> statement, std::ostringstream &out) {
        auto &scp = *statement->scope;
        const std::string name = statement->value;
        TypeInfo ti = scp.lookupType(scp.lookupVariable(name).type);
        uint64_t sz = ti.bits / 8;

        // Evaluate RHS expression
        std::string r = emitExpression(std::move(statement->children.back()), out);
        restoreIfSpilled(r, statement->scope, out);

        // Cast to the variable's type
        r = castValue(r, regType.at(r), ti, statement->scope, out);
        restoreIfSpilled(r, statement->scope, out);

        // Determine memory operand
        std::string mem;
        if (scp.isGlobalVariable(name)) {
            mem = "[" + name + "]";
        } else {
            int64_t off = std::abs(scp.getVariableOffset(name));
            mem = "[rbp - " + std::to_string(off) + "]";
        }

        // Emit store based on size and type
        if (ti.isFloat) {
            if (sz == 4) {
                out << "    movss DWORD PTR " << mem << ", " << r << "\n";
            } else {
                out << "    movsd QWORD PTR " << mem << ", " << r << "\n";
            }
        } else {
            switch (sz) {
                case 1:
                    out << "    mov     BYTE PTR " << mem << ", " << r << "\n";
                    break;
                case 2:
                    out << "    mov     WORD PTR " << mem << ", " << r << "\n";
                    break;
                case 4:
                    out << "    mov     DWORD PTR " << mem << ", " << r << "\n";
                    break;
                case 8:
                default:
                    out << "    mov     QWORD PTR " << mem << ", " << r << "\n";
                    break;
            }
        }

        alloc.free(r);
    }

    void CodeGenWindows::generateVariableDeclaration(std::unique_ptr<ASTNode> statement, std::ostringstream &out) {
        auto &scp = *statement->scope;
        const std::string name = statement->value;
        TypeInfo ti = scp.lookupType(scp.lookupVariable(name).type);
        uint64_t sz = ti.bits / 8;  // bytes

        // Compute memory operand
        std::string mem = scp.isGlobalVariable(name)
            ? ("[" + name + "]")
            : ("[rbp - " + std::to_string(std::abs(scp.getVariableOffset(name))) + "]");

        // Lambda to emit store
        auto emitStore = [&](const std::string &r) {
            if (ti.isFloat) {
                if (sz == 4) {
                    out << "    movss DWORD PTR " << mem << ", " << r << "\n";
                } else {
                    out << "    movsd QWORD PTR " << mem << ", " << r << "\n";
                }
            } else {
                switch (sz) {
                    case 1:
                        out << "    mov     BYTE PTR " << mem << ", " << adjustReg(r, sz*8) << "\n";
                        break;
                    case 2:
                        out << "    mov     WORD PTR " << mem << ", " << adjustReg(r, sz*8) << "\n";
                        break;
                    case 4:
                        out << "    mov     DWORD PTR " << mem << ", " << adjustReg(r, sz*8) << "\n";
                        break;
                    case 8:
                    default:
                        out << "    mov     QWORD PTR " << mem << ", " << adjustReg(r, sz*8) << "\n";
                        break;
                }
            }
        };

        if (statement->children.size() >= 2) {
            std::string r = emitExpression(std::move(statement->children.back()), out);
            restoreIfSpilled(r, statement->scope, out);

            r = castValue(r, regType.at(r), ti, statement->scope, out);
            restoreIfSpilled(r, statement->scope, out);

            emitStore(r);
            alloc.free(r);
        } else {
            if (ti.isFloat) {
                std::string r_xmm = allocateOrSpill(true, statement->scope, out);
                out << "    pxor    " << r_xmm << ", " << r_xmm << "\n";
                emitStore(r_xmm);
                alloc.free(r_xmm);
            } else {
                // Zero-initialize integer of size sz
                switch (sz) {
                    case 1:
                        out << "    mov     BYTE PTR " << mem << ", 0\n";
                        break;
                    case 2:
                        out << "    mov     WORD PTR " << mem << ", 0\n";
                        break;
                    case 4:
                        out << "    mov     DWORD PTR " << mem << ", 0\n";
                        break;
                    case 8:
                    default:
                        out << "    xor     rax, rax\n";
                        out << "    mov     QWORD PTR " << mem << ", rax\n";
                        break;
                }
            }
        }
    }
    void CodeGenWindows::generateIfStatement(std::unique_ptr<ASTNode> statement, std::ostringstream &out) {
        int id = blockLabelCount++;
        std::string endLbl = "Lend" + std::to_string(id);
        std::vector<std::string> elseLbls;

        // Precompute labels for all else‑if / else branches
        ASTNode *branch = statement->getElseBranch();
        while (branch) {
            elseLbls.push_back("Lelse" + std::to_string(blockLabelCount++));
            branch = branch->getElseBranch();
        }

        size_t elseIdx = 0;

        // --- IF branch ---
        std::string condR = emitExpression(std::move(statement->children[0]), out);
        restoreIfSpilled(condR, statement->scope, out);

        // cmp condR, 0
        out << "    cmp     " << condR << ", 0\n";
        // je to first else (or end)
        out << "    je      " << (elseLbls.empty() ? endLbl : elseLbls[elseIdx]) << "\n";
        alloc.free(condR);

        // then‑block
        auto ifBlk = std::move(statement->children[1]);
        emitPrologue(ifBlk->scope, out);
        for (auto &stmt : ifBlk->children) {
            generateStatement(std::move(stmt), out);
        }
        emitEpilogue(ifBlk->scope, out);
        // jump past else‐chains
        out << "    jmp     " << endLbl << "\n";

        // --- ELSE‑IF / ELSE branches ---
        branch = statement->getElseBranch();
        while (branch) {
            out << elseLbls[elseIdx++] << ":\n";

            if (branch->type == NodeType::ElseIfStatement) {
                // test elseif condition
                std::string r2 = emitExpression(std::move(branch->children[0]), out);
                restoreIfSpilled(r2, branch->scope, out);
                out << "    cmp     " << r2 << ", 0\n";
                // je to next else / end
                std::string nextLbl = (elseIdx < elseLbls.size()
                                           ? elseLbls[elseIdx]
                                           : endLbl);
                out << "    je      " << nextLbl << "\n";
                alloc.free(r2);

                // elseif‑block
                auto elifBlk = std::move(branch->children[1]);
                emitPrologue(elifBlk->scope, out);
                for (auto &stmt : elifBlk->children) {
                    generateStatement(std::move(stmt), out);
                }
                emitEpilogue(elifBlk->scope, out);
                out << "    jmp     " << endLbl << "\n";
            } else if (branch->type == NodeType::ElseStatement) {
                // else‑block
                auto elseBlk = std::move(branch->children[0]);
                emitPrologue(elseBlk->scope, out);
                for (auto &stmt : elseBlk->children) {
                    generateStatement(std::move(stmt), out);
                }
                emitEpilogue(elseBlk->scope, out);
            }

            branch = branch->getElseBranch();
        }

        // end label
        out << endLbl << ":\n\n";
    }
    void CodeGenWindows::generate(std::unique_ptr<ASTNode> program) {
        std::vector<ASTNode*> globals;

        // collect globals
        for (auto &stmt : program->children) {
            if (stmt->type == NodeType::VariableDeclaration) {
                globals.push_back(stmt.get());
            }
        }

        // --- .DATA segment (read-write globals) ---
        outGlobalStream << ".data\n\n";
        for (auto &g : globals)
        {
            TypeInfo info = g->scope->lookupType(g->children[0]->value);
            switch (info.bits / 8)
            {
            case 8:
                outGlobalStream << g->value << " QWORD 0\n";
                break;
            case 4:
                outGlobalStream << g->value << " DWORD 0\n";
                break;
            case 2:
                outGlobalStream << g->value << " WORD 0\n";
                break;
            case 1:
                outGlobalStream << g->value << " BYTE 0\n";
                break;
            default:
                throw std::runtime_error("Unsupported global size");
            }
        }

        outGlobalStream << "\n.const\n";
        
        // --- .CODE segment ---
        outStream << ".code\n";

        // emit stack-smash handler (define only, no EXTERN)
        outStream << "__stack_smash_detected PROC\n"
                << "    mov     rax, 60       ; syscall: exit\n"
                << "    mov     rdi, 69       ; exit code\n"
                << "    syscall\n"
                << "    ret\n"
                << "__stack_smash_detected ENDP\n\n";

        // collect main and top-level init statements
        std::unique_ptr<ASTNode> mainFn;
        std::vector<std::unique_ptr<ASTNode>> initStmts;
        for (auto &stmt : program->children) {
            if (stmt->type == NodeType::Function && stmt->value == "main") {
                mainFn = std::move(stmt);
            } else if (stmt->type == NodeType::VariableDeclaration || stmt->type == NodeType::VariableReassignment
                    || (stmt->type == NodeType::UnaryOp && (stmt->value == "++" || stmt->value == "--"))) {
                initStmts.push_back(std::move(stmt));
            } else {
                generateStatement(std::move(stmt), outStream);
            }
        }

        // emit main
        outStream << "main PROC\n";
        for (auto &s : initStmts) {
            generateStatement(std::move(s), outStream);
        }
        generateFunctionDeclaration(std::move(mainFn), outStream, /*force=*/true);
        outStream << "    ret\n";
        outStream << "main ENDP\n\n";

        // finalize with END directive
        outfinal << outGlobalStream.str()
                << "; ============== Globals End Here ==============\n\n"
                << outStream.str()
                << "\nEND\n";
    }

    std::string CodeGenWindows::generateFunctionCall(std::unique_ptr<ASTNode> node, std::ostringstream &out) {
        // 1) Lookup target and args
        auto fnInfo = node->scope->lookupFunction(node->value);
        const std::string label = fnInfo.isExtern ? node->value : fnInfo.label;
        auto &args = node->children[0]->children;
        const auto &params = fnInfo.paramTypes;

        // 2) Save caller‑saved GPRs
        std::vector<std::string> savedGPR;
        for (auto &reg : CALLER_GPR_MSVC) {
            if (alloc.isInUse(reg)) {
                out << "    push    " << reg << "    ; save caller GPR\n";
                savedGPR.push_back(reg);
            }
        }

        // 3) Save caller‑saved XMMs
        std::vector<std::pair<std::string, std::string>> savedXMM;
        for (auto &reg : CALLER_XMM_MSVC) {
            if (alloc.isInUseXMM(reg)) {
                auto funcScope = std::static_pointer_cast<FunctionScope>(
                    node->scope->findEnclosingFunctionScope());
                std::string slot = funcScope->allocateSpillSlot(16,
                                                                CodegenOutputFormat::X86_64_MSWIN);
                alloc.markSpilledXMM(reg, slot);
                out << "    movdqu  " << reg
                    << ", XMMWORD PTR " << slot
                    << "    ; spill caller XMM\n";
                savedXMM.emplace_back(reg, slot);
            }
        }

        // 4) Compute stack‐space needed for overflow args
        uint64_t gpCount = 0, xmmCount = 0, stackOff = 0;
        for (size_t i = 0; i < args.size(); ++i) {
            bool isFloat = (i < params.size())
                            ? node->scope->lookupType(params[i].type).isFloat
                            : (fnInfo.isVariadic ? false
                                                    : throw std::runtime_error(
                                                        "Too many args"));
            if (!isFloat) {
                if (gpCount < ARG_GPR_MSVC.size())
                    gpCount++;
                else
                    stackOff += 8;
            } else {
                if (xmmCount < ARG_XMM_MSVC.size())
                    xmmCount++;
                else
                    stackOff += 8;
            }
        }
        uint64_t stackReserve = (stackOff + 15) & ~15ULL;
        if (stackReserve) {
            out << "    sub     rsp, " << stackReserve
                << "    ; reserve arg stack\n";
        }

        std::vector<std::pair<int, std::string>> shadowDuplicates;

        // 5) Materialize arguments
        gpCount = xmmCount = stackOff = 0;
        std::vector<std::string> reservedArgs;
        std::vector<std::string> spilledArgs;

        for (size_t i = 0; i < args.size(); ++i) {
            // evaluate and cast
            std::string src = emitExpression(std::move(args[i]), out);
            restoreIfSpilled(src, node->scope, out);
            TypeInfo passed = regType.at(src);
            TypeInfo expect =
                (i < params.size())
                    ? node->scope->lookupType(params[i].type)
                    : (fnInfo.isVariadic
                        ? (passed.isFloat
                                ? node->scope->lookupType("double")
                                : node->scope->lookupType("int64_t"))
                        : throw std::runtime_error("Too many args"));
            std::string cvt = castValue(src, passed, expect, node->scope, out);
            restoreIfSpilled(cvt, node->scope, out);

            bool isF = expect.isFloat;
            // GPR args
            if (!isF && gpCount < ARG_GPR_MSVC.size()) {
                std::string dst = ARG_GPR_MSVC[gpCount];
                if (alloc.isInUseArgument(dst)) {
                    auto funcScope = std::static_pointer_cast<FunctionScope>(
                        node->scope->findEnclosingFunctionScope());
                    std::string slot = funcScope->allocateSpillSlot(8,
                                                                    CodegenOutputFormat::X86_64_MSWIN);
                    alloc.markSpilled(dst, slot);
                    spilledArgs.push_back(dst);
                    out << "    mov     QWORD PTR " << slot
                        << ", " << dst
                        << "    ; spill arg GPR\n";
                } else {
                    dst = alloc.allocateArgument(gpCount);
                    reservedArgs.push_back(dst);
                }
                out << "    mov     " << dst
                    << ", " << cvt
                    << "\n";
                gpCount++;
            }
            // XMM args
            else if (isF && xmmCount < ARG_XMM_MSVC.size()) {
                std::string dst = ARG_XMM_MSVC[xmmCount];
                if (alloc.isInUseArgumentXMM(dst)) {
                    auto funcScope = std::static_pointer_cast<FunctionScope>(
                        node->scope->findEnclosingFunctionScope());
                    std::string slot = funcScope->allocateSpillSlot(16,
                                                                    CodegenOutputFormat::X86_64_MSWIN);
                    alloc.markSpilledXMM(dst, slot);
                    spilledArgs.push_back(dst);
                    out << "    movdqu  " << dst
                        << ", XMMWORD PTR " << slot
                        << "    ; spill arg XMM\n";
                } else {
                    dst = alloc.allocateArgumentXMM(xmmCount);
                    reservedArgs.push_back(dst);
                }

                out << "    movsd   " << dst << ", " << cvt << "\n";

                // CORRECTED: Use argument index (i) instead of xmmCount
                if (fnInfo.isVariadic && i < 4) {
                    static const std::vector<std::string> VARIADIC_FLOAT_GPRS = { "rcx", "rdx", "r8", "r9" };
                    std::string gprDst = VARIADIC_FLOAT_GPRS[i];
                    out << "    movq    " << gprDst << ", " << dst << "    ; duplicate variadic float to GPR\n";
                    // Record for shadow space duplication
                    shadowDuplicates.push_back({8 * static_cast<int>(i), dst});
                }

                xmmCount++;
            }
            // stack‐overflow args
            else {
                out << "    mov     QWORD PTR [rsp+" << stackOff << "], "
                    << cvt << "\n";
                stackOff += 8;
            }

            alloc.free(cvt);
        }

        // 6) Variadic‐ABI: number of float args in XMM
        if (fnInfo.isVariadic) {
            out << "    mov     rax, " << xmmCount
                << "    ; variadic float count\n";
        }

        // CORRECTED: Calculate proper stack alignment (32+8=40)
        uint64_t shadowAndAlign = 32;
        out << "    sub     rsp, " << shadowAndAlign << "      ; 🔑 Allocate shadow space + alignment\n";
        out << "    call    " << label << "\n";
        out << "    add     rsp, " << shadowAndAlign << "      ; 🔑 Cleanup shadow space + alignment\n";

        // 8) Restore spilled argument regs
        for (auto &r : spilledArgs) {
            restoreIfSpilled(r, node->scope, out);
        }
        for (auto &r : reservedArgs) {
            alloc.freeArgument(r);
        }

        // 9) Restore caller‑saved XMMs & GPRs
        for (auto it = savedXMM.rbegin(); it != savedXMM.rend(); ++it) {
            out << "    movdqu  XMMWORD PTR " << it->second
                << ", " << it->first
                << "    ; restore XMM\n";
        }
        for (auto it = savedGPR.rbegin(); it != savedGPR.rend(); ++it) {
            out << "    pop     " << *it
                << "    ; restore GPR\n";
        }

        // 10) Tear down arg stack
        if (stackReserve) {
            out << "    add     rsp, " << stackReserve
                << "    ; pop arg stack\n";
        }

        // 11) Capture return
        bool isFltRet = fnInfo.returnType == "float" || fnInfo.returnType == "double";
        std::string holder = allocateOrSpill(isFltRet, node->scope, out);
        noteType(holder, node->scope->lookupType(fnInfo.returnType));

        if (isFltRet) {
            const char *mov = (fnInfo.returnType == "float" ? "movss" : "movsd");
            out << "    " << mov << " " << holder
                << ", xmm0\n";
        } else {
            out << "    mov     " << holder
                << ", rax\n";
        }

        return holder;
    }
    
    void CodeGenWindows::generateFunctionDeclaration(std::unique_ptr<ASTNode> node, std::ostringstream &out, bool force) {        
        if (node->value == "main" && !force)
            return;

        // Lookup signature & body
        auto fnInfo = node->scope->lookupFunction(node->value);
        auto bodyNode = node->getFunctionBody();
        auto funcScope = std::dynamic_pointer_cast<FunctionScope>(
            bodyNode->scope->findEnclosingFunctionScope());

        if (!funcScope)
            throw std::runtime_error("Expected FunctionScope for function declaration");

        if(!force)
            out << fnInfo.label << " PROC\n";

        // Generate parameter moves into locals ([rbp - offset])
        // Windows registers: RCX, RDX, R8, R9; XMM0–XMM3
        static const std::vector<std::string> ARG_GPR_MS = {"rcx", "rdx", "r8", "r9"};
        static const std::vector<std::string> ARG_XMM_MS = {"xmm0", "xmm1", "xmm2", "xmm3"};

        // Build body before prologue so we can prefix it
        std::ostringstream paramInit, body;

        size_t gpIdx = 0, xmmIdx = 0, stackArgOffset = 0;
        for (auto &p : fnInfo.paramTypes) {
            TypeInfo ti = node->scope->lookupType(p.type);
            bool isFlt = ti.isFloat;
            int64_t slotOff = std::abs(bodyNode->scope->getVariableOffset(p.name));

            // Select MOV instruction
            std::string movInst;
            if (isFlt)
                movInst = (ti.bits == 64 ? "movsd" : "movss");
            else
                movInst = "mov     ";  // QWORD PTR implied by reg

            if (!isFlt && gpIdx < ARG_GPR_MS.size()) {
                // integer reg → local slot
                paramInit
                    << "    " << movInst
                    << " QWORD PTR [rbp-" << slotOff << "], "
                    << ARG_GPR_MS[gpIdx++] << "\n";
            } else if (isFlt && xmmIdx < ARG_XMM_MS.size()) {
                // xmm reg → local slot
                paramInit
                    << "    " << "movdqu"
                    << " XMMWORD PTR [rbp-" << slotOff << "], "
                    << ARG_XMM_MS[xmmIdx++] << "\n";
            } else {
                // spilled on caller’s stack at [rbp+16 + stackArgOffset]
                int64_t callerDisp = 16 + stackArgOffset;
                paramInit
                    << "    " << movInst
                    << (isFlt ? "XMMWORD PTR " : "QWORD PTR ")
                    << "[rbp+" << callerDisp << "], "
                    << (isFlt ? ARG_XMM_MS[xmmIdx++]
                              : ARG_GPR_MS[gpIdx++])
                    << "\n";
                stackArgOffset += 8;
            }
        }

        // Setup canary and stack frame
        std::ostringstream prologue;
        std::uint64_t canary = CanaryGenerator::generate();
        funcScope->setCanary(canary);

        // Generate the actual function body
        std::vector<std::unique_ptr<ASTNode>> nestedFns;
        for (auto &stmt : bodyNode->children) {
            if (stmt->type == NodeType::Function) {
                nestedFns.push_back(std::move(stmt));
            } else {
                generateStatement(std::move(stmt), body);
            }
        }


        emitPrologue(funcScope, prologue);

        // If void return, emit epilogue after body
        std::ostringstream epilogue;
        if (bodyNode->scope->returnType == "none") {
            // Note: clearRax must be true for void
            emitEpilogue(funcScope, epilogue, /*clearRax=*/true);
        }

        // Stitch it all together
        out
            << prologue.str()
            << paramInit.str()
            << body.str()
            << epilogue.str();

        // Nested functions follow
        for (auto &nf : nestedFns) {
            generateFunctionDeclaration(std::move(nf), out, /*force=*/false);
        }

        // End PROC
        if(!force)
        out << fnInfo.label << " ENDP\n\n";
    }
    void CodeGenWindows::generateExternFunctionDeclaration(std::unique_ptr<ASTNode> node, std::ostringstream &out) {
        auto fnInfo = node->scope->lookupFunction(node->value);
        const std::string &label = node->value;
        outGlobalStream << "EXTERN  " << label << ":FAR\n";
    }
    void CodeGenWindows::generateReturnstatement(std::unique_ptr<ASTNode> node, std::ostringstream &out) {
        std::string result = emitExpression(std::move(node->children[0]), out);

        // Restore if spilled
        auto funcScope = std::static_pointer_cast<FunctionScope>(
            node->scope->findEnclosingFunctionScope());
        restoreIfSpilled(result, funcScope, out);

        // Perform any necessary casts
        TypeInfo actual = regType.at(result);
        TypeInfo expected = funcScope->lookupType(funcScope->returnType);
        std::string casted = castValue(result, actual, expected, funcScope, out);
        restoreIfSpilled(casted, funcScope, out);

        // Move into the ABI return register
        if (expected.name == "none") {
            // void return: zero RAX
            out << "    xor     rax, rax\n";
        } else if (!expected.isFloat) {
            // integer return
            out << "    mov     rax, " << casted << "\n";
        } else {
            // floating‑point return
            const char *mov = (expected.bits == 32 ? "movss" : "movsd");
            out << "    " << mov << " xmm0, " << casted << "\n";
        }
        alloc.free(casted);
        emitEpilogue(funcScope, out);
    }
}  // namespace zlang