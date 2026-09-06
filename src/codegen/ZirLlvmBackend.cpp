#include "codegen/ZirLlvmBackend.hpp"

#include <cstdint>
#include <cstring>
#include <iomanip>
#include <sstream>
#include <stdexcept>
#include <unordered_map>
#include <vector>

namespace zust {

    using namespace zust::zir;

    namespace {

        std::string llvmType(const TypeTable &types, TypeId id) {
            const Type &t = types.get(id);
            switch (t.kind) {
                case TypeKind::Void:
                    return "void";
                case TypeKind::Int:
                    // LLVM integer types carry no signedness of their own --
                    // "i64" is the whole story regardless of ZIR's Int::isSigned;
                    // signedness only ever selects *which instruction*
                    // (sdiv/udiv, sext/zext, icmp slt/ult...) to emit, never
                    // which type to print.
                    return "i" + std::to_string(t.bits);
                case TypeKind::Float:
                    return t.bits == 32 ? "float" : "double";
                case TypeKind::Ptr:
                    // Opaque pointers: every Ptr, regardless of pointee,
                    // prints as "ptr" -- matching Printer::printType's own
                    // ZIR-level opaqueness (zir::Printer.cpp) and current
                    // LLVM's actual pointer model.
                    return "ptr";
                case TypeKind::Array:
                    return "[" + std::to_string(t.arrayLen) + " x " + llvmType(types, t.elem) + "]";
                case TypeKind::Fn: {
                    std::string s = llvmType(types, t.ret) + " (";
                    for (std::size_t i = 0; i < t.params.size(); ++i) {
                        if (i)
                            s += ", ";
                        s += llvmType(types, t.params[i]);
                    }
                    if (t.variadic)
                        s += t.params.empty() ? "..." : ", ...";
                    s += ")";
                    return s;
                }
            }
            throw std::runtime_error("ZirLlvmBackend: unknown TypeKind");
        }

        // "<ret> (<param>, <param>, ...)" -- the explicit-signature form a
        // `call` needs to be well-formed for a variadic callee, and that is
        // harmless (if slightly verbose) to use unconditionally for every
        // call, variadic or not.
        std::string llvmFnSigForCall(const TypeTable &types, TypeId fnSig) {
            return llvmType(types, fnSig);
        }

        std::string hexFloatLiteral(TypeId ty, const TypeTable &types, std::uint64_t bits) {
            // LLVM hex float constants are always the *double*-width hex
            // encoding, even when the destination type is `float` (LangRef:
            // "This is required for round-trip in exact precision... the
            // value must be exactly representable when rounded to the
            // destination type"). So a 32-bit float's stored bits are
            // widened to their exact double equivalent (float -> double is
            // always exact) before hex-encoding -- matching the same
            // store-raw-bits-not-decimal-text principle ZIR's own constants
            // already follow (docs/PRD-ZIR.md behavior inventory).
            std::uint64_t doubleBits;
            if (types.get(ty).bits == 32) {
                std::uint32_t b32 = static_cast<std::uint32_t>(bits);
                float f;
                std::memcpy(&f, &b32, sizeof(f));
                double d = static_cast<double>(f);
                std::memcpy(&doubleBits, &d, sizeof(doubleBits));
            } else {
                doubleBits = bits;
            }
            std::ostringstream ss;
            ss << "0x" << std::uppercase << std::hex << std::setw(16) << std::setfill('0') << doubleBits;
            return ss.str();
        }

        // Escapes raw bytes into LLVM's `c"..."` string-constant syntax:
        // printable, non-special ASCII passes through; everything else
        // (including `"` and `\` themselves) becomes `\XX`, two uppercase
        // hex digits -- LLVM's own convention, not C's.
        std::string llvmEscapeBytes(const std::string &bytes) {
            std::ostringstream ss;
            ss << std::uppercase << std::hex << std::setfill('0');
            for (unsigned char c : bytes) {
                if (c >= 0x20 && c < 0x7F && c != '"' && c != '\\') {
                    ss << static_cast<char>(c);
                } else {
                    ss << '\\' << std::setw(2) << static_cast<unsigned>(c);
                }
            }
            return ss.str();
        }

        const char *icmpPred(CmpPred p) {
            switch (p) {
                case CmpPred::Eq:
                    return "eq";
                case CmpPred::Ne:
                    return "ne";
                case CmpPred::Slt:
                    return "slt";
                case CmpPred::Sle:
                    return "sle";
                case CmpPred::Sgt:
                    return "sgt";
                case CmpPred::Sge:
                    return "sge";
                case CmpPred::Ult:
                    return "ult";
                case CmpPred::Ule:
                    return "ule";
                case CmpPred::Ugt:
                    return "ugt";
                case CmpPred::Uge:
                    return "uge";
                default:
                    throw std::runtime_error("ZirLlvmBackend: icmp with a non-integer predicate");
            }
        }

        const char *fcmpPred(CmpPred p) {
            switch (p) {
                case CmpPred::Oeq:
                    return "oeq";
                case CmpPred::One:
                    return "one";
                case CmpPred::Olt:
                    return "olt";
                case CmpPred::Ole:
                    return "ole";
                case CmpPred::Ogt:
                    return "ogt";
                case CmpPred::Oge:
                    return "oge";
                default:
                    throw std::runtime_error("ZirLlvmBackend: fcmp with a non-float predicate");
            }
        }

        // One instance per function being emitted. Not every ZIR value
        // becomes a real LLVM SSA register: Const, GlobalAddr, and a no-op
        // Bitcast (ptr<->ptr or same-width differently-signed int, both of
        // which are literally the same LLVM type) all resolve to inline
        // text at their use sites instead -- exactly how LLVM textual IR
        // represents constants and global symbols itself, so this isn't an
        // optimization, it's what a faithful translation looks like.
        class FunctionEmitter {
        public:
            FunctionEmitter(const Module &m, const Function &fn, std::ostream &out) : m_(m), fn_(fn), out_(out) {
                valueText_.assign(fn_.valueCount(), "");
                for (std::size_t bi = 0; bi < fn_.blockCount(); ++bi) {
                    for (InstId iid : fn_.block(BlockId(static_cast<BlockId::Value>(bi))).insts()) {
                        const Instruction &inst = fn_.inst(iid);
                        if (inst.result.isValid())
                            defSite_[inst.result.value()] = iid;
                    }
                }
            }

            void emitDefinition() {
                const Type &sig = m_.types().get(fn_.signature());
                const BasicBlock &entry = fn_.block(fn_.entry());

                out_ << "define " << llvmType(m_.types(), sig.ret) << " @" << fn_.name() << "(";
                const std::vector<ValueId> &params = entry.params();
                for (std::size_t i = 0; i < params.size(); ++i) {
                    if (i)
                        out_ << ", ";
                    std::string argName = "%" + fn_.nameOf(params[i]);
                    valueText_[params[i].value()] = argName;
                    out_ << llvmType(m_.types(), fn_.typeOf(params[i])) << " " << argName;
                }
                if (sig.variadic)
                    out_ << (params.empty() ? "..." : ", ...");
                out_ << ") {\n";

                for (std::size_t bi = 0; bi < fn_.blockCount(); ++bi) {
                    BlockId bid(static_cast<BlockId::Value>(bi));
                    const BasicBlock &block = fn_.block(bid);
                    out_ << block.label() << ":\n";
                    if (bid != fn_.entry())
                        for (ValueId param : block.params()) emitPhi(bid, param);
                    for (InstId iid : block.insts()) emitInstruction(fn_.inst(iid));
                    emitTerminator(block.term());
                }
                out_ << "}\n\n";
            }

            void emitDeclaration() {
                const Type &sig = m_.types().get(fn_.signature());
                out_ << "declare " << llvmType(m_.types(), sig.ret) << " @" << fn_.name() << "(";
                for (std::size_t i = 0; i < sig.params.size(); ++i) {
                    if (i)
                        out_ << ", ";
                    out_ << llvmType(m_.types(), sig.params[i]);
                }
                if (sig.variadic)
                    out_ << (sig.params.empty() ? "..." : ", ...");
                out_ << ")\n\n";
            }

        private:
            std::string ref(ValueId v) const {
                const std::string &text = valueText_[v.value()];
                if (text.empty())
                    throw std::runtime_error("ZirLlvmBackend: value used before it was defined");
                return text;
            }

            // "<ty> <valuetext>" -- the operand form almost every
            // instruction wants.
            std::string typedRef(ValueId v) const { return llvmType(m_.types(), fn_.typeOf(v)) + " " + ref(v); }

            std::string freshName(ValueId result) {
                std::string name = "%v" + std::to_string(result.value());
                valueText_[result.value()] = name;
                return name;
            }

            std::string constText(const Instruction &inst) const {
                if (m_.types().get(inst.type).kind == TypeKind::Float)
                    return hexFloatLiteral(inst.type, m_.types(), inst.constant.bits);
                // The type's own bit width masks the stored pattern --
                // ConstValue::bits is a uint64_t regardless of the
                // constant's actual width.
                std::uint32_t bits = m_.types().get(inst.type).bits;
                std::uint64_t mask = bits >= 64 ? ~std::uint64_t{0} : ((std::uint64_t{1} << bits) - 1);
                return std::to_string(inst.constant.bits & mask);
            }

            // A phi's incoming value can legitimately be something this
            // emitter has not visited yet (a loop back edge from a block
            // that comes later in block order) -- LLVM textual IR allows
            // exactly this forward reference in a phi's incoming list, so
            // this resolves what the name *will* be rather than requiring
            // it to already exist. A not-yet-visited Const/GlobalAddr is
            // resolved immediately (and cached) since those never get a
            // real register name at all; anything else always ends up
            // named "%v<id>" by freshName, so predicting that name here is
            // exact, not a guess.
            std::string phiIncomingRef(ValueId v) {
                if (!valueText_[v.value()].empty())
                    return valueText_[v.value()];
                auto it = defSite_.find(v.value());
                if (it != defSite_.end()) {
                    const Instruction &def = fn_.inst(it->second);
                    if (def.op == Opcode::Const) {
                        valueText_[v.value()] = constText(def);
                        return valueText_[v.value()];
                    }
                    if (def.op == Opcode::GlobalAddr) {
                        valueText_[v.value()] = "@" + m_.global(def.global).name;
                        return valueText_[v.value()];
                    }
                }
                return "%v" + std::to_string(v.value());
            }

            void emitPhi(BlockId target, ValueId param) {
                std::string name = "%v" + std::to_string(param.value());
                valueText_[param.value()] = name;

                const std::vector<ValueId> &params = fn_.block(target).params();
                std::size_t idx = 0;
                while (idx < params.size() && params[idx] != param) ++idx;

                out_ << "  " << name << " = phi " << llvmType(m_.types(), fn_.typeOf(param));
                bool first = true;
                for (std::size_t bi = 0; bi < fn_.blockCount(); ++bi) {
                    BlockId pred(static_cast<BlockId::Value>(bi));
                    for (const BlockRef &ref : fn_.block(pred).term().targets) {
                        if (ref.block != target || idx >= ref.args.size())
                            continue;
                        out_ << (first ? " " : ", ") << "[ " << phiIncomingRef(ref.args[idx]) << ", %"
                             << fn_.block(pred).label() << " ]";
                        first = false;
                    }
                }
                out_ << "\n";
            }

            void emitInstruction(const Instruction &inst) {
                switch (inst.op) {
                    case Opcode::Const: {
                        valueText_[inst.result.value()] = constText(inst);
                        return;
                    }
                    case Opcode::GlobalAddr:
                        valueText_[inst.result.value()] = "@" + m_.global(inst.global).name;
                        return;
                    case Opcode::Bitcast: {
                        TypeId fromTy = fn_.typeOf(inst.operands[0]);
                        std::string fromLlvm = llvmType(m_.types(), fromTy);
                        std::string toLlvm = llvmType(m_.types(), inst.type);
                        if (fromLlvm == toLlvm) {
                            // Same LLVM type either side (ptr<->ptr, or two
                            // same-width ints differing only in ZIR's
                            // signedness flag, which LLVM's type system does
                            // not represent at all) -- nothing to emit.
                            valueText_[inst.result.value()] = ref(inst.operands[0]);
                            return;
                        }
                        std::string name = freshName(inst.result);
                        out_ << "  " << name << " = bitcast " << typedRef(inst.operands[0]) << " to " << toLlvm
                             << "\n";
                        return;
                    }
                    case Opcode::Alloca: {
                        std::string name = freshName(inst.result);
                        out_ << "  " << name << " = alloca " << llvmType(m_.types(), inst.elemType) << "\n";
                        return;
                    }
                    case Opcode::Load: {
                        std::string name = freshName(inst.result);
                        out_ << "  " << name << " = load " << llvmType(m_.types(), inst.type) << ", "
                             << typedRef(inst.operands[0]) << "\n";
                        return;
                    }
                    case Opcode::Store: {
                        out_ << "  store " << typedRef(inst.operands[0]) << ", " << typedRef(inst.operands[1])
                             << "\n";
                        return;
                    }
                    case Opcode::Add:
                    case Opcode::Sub:
                    case Opcode::Mul:
                    case Opcode::SDiv:
                    case Opcode::UDiv:
                    case Opcode::SRem:
                    case Opcode::URem:
                    case Opcode::And:
                    case Opcode::Or:
                    case Opcode::Xor:
                    case Opcode::Shl:
                    case Opcode::LShr:
                    case Opcode::AShr:
                    case Opcode::FAdd:
                    case Opcode::FSub:
                    case Opcode::FMul:
                    case Opcode::FDiv: {
                        std::string name = freshName(inst.result);
                        out_ << "  " << name << " = " << binopKeyword(inst.op) << " "
                             << llvmType(m_.types(), inst.type) << " " << ref(inst.operands[0]) << ", "
                             << ref(inst.operands[1]) << "\n";
                        return;
                    }
                    case Opcode::ICmp: {
                        std::string name = freshName(inst.result);
                        out_ << "  " << name << " = icmp " << icmpPred(inst.pred) << " "
                             << llvmType(m_.types(), fn_.typeOf(inst.operands[0])) << " " << ref(inst.operands[0])
                             << ", " << ref(inst.operands[1]) << "\n";
                        return;
                    }
                    case Opcode::FCmp: {
                        std::string name = freshName(inst.result);
                        out_ << "  " << name << " = fcmp " << fcmpPred(inst.pred) << " "
                             << llvmType(m_.types(), fn_.typeOf(inst.operands[0])) << " " << ref(inst.operands[0])
                             << ", " << ref(inst.operands[1]) << "\n";
                        return;
                    }
                    case Opcode::Neg: {
                        std::string name = freshName(inst.result);
                        if (m_.types().get(inst.type).kind == TypeKind::Float)
                            out_ << "  " << name << " = fneg " << llvmType(m_.types(), inst.type) << " "
                                 << ref(inst.operands[0]) << "\n";
                        else
                            out_ << "  " << name << " = sub " << llvmType(m_.types(), inst.type) << " 0, "
                                 << ref(inst.operands[0]) << "\n";
                        return;
                    }
                    case Opcode::Not: {
                        std::string name = freshName(inst.result);
                        out_ << "  " << name << " = xor " << typedRef(inst.operands[0]) << ", -1\n";
                        return;
                    }
                    case Opcode::Trunc:
                    case Opcode::ZExt:
                    case Opcode::SExt:
                    case Opcode::FPTrunc:
                    case Opcode::FPExt:
                    case Opcode::FPToSI:
                    case Opcode::FPToUI:
                    case Opcode::SIToFP:
                    case Opcode::UIToFP:
                    case Opcode::PtrToInt:
                    case Opcode::IntToPtr: {
                        std::string name = freshName(inst.result);
                        out_ << "  " << name << " = " << castKeyword(inst.op) << " " << typedRef(inst.operands[0])
                             << " to " << llvmType(m_.types(), inst.type) << "\n";
                        return;
                    }
                    case Opcode::Gep: {
                        std::string name = freshName(inst.result);
                        out_ << "  " << name << " = getelementptr " << llvmType(m_.types(), inst.elemType) << ", "
                             << typedRef(inst.operands[0]);
                        for (std::size_t i = 1; i < inst.operands.size(); ++i) out_ << ", " << typedRef(inst.operands[i]);
                        out_ << "\n";
                        return;
                    }
                    case Opcode::Call: {
                        FuncId callee = inst.callee;
                        const Function &calleeFn = m_.function(callee);
                        TypeId retTy = m_.types().get(calleeFn.signature()).ret;
                        bool isVoid = m_.types().get(retTy).kind == TypeKind::Void;
                        std::string name;
                        out_ << "  ";
                        if (!isVoid) {
                            name = freshName(inst.result);
                            out_ << name << " = ";
                        }
                        out_ << "call " << llvmFnSigForCall(m_.types(), calleeFn.signature()) << " @"
                             << calleeFn.name() << "(";
                        for (std::size_t i = 0; i < inst.operands.size(); ++i) {
                            if (i)
                                out_ << ", ";
                            out_ << typedRef(inst.operands[i]);
                        }
                        out_ << ")\n";
                        return;
                    }
                    case Opcode::Select: {
                        std::string name = freshName(inst.result);
                        out_ << "  " << name << " = select " << typedRef(inst.operands[0]) << ", "
                             << typedRef(inst.operands[1]) << ", " << typedRef(inst.operands[2]) << "\n";
                        return;
                    }
                }
                throw std::runtime_error("ZirLlvmBackend: unhandled opcode");
            }

            static const char *binopKeyword(Opcode op) {
                switch (op) {
                    case Opcode::Add:
                        return "add";
                    case Opcode::Sub:
                        return "sub";
                    case Opcode::Mul:
                        return "mul";
                    case Opcode::SDiv:
                        return "sdiv";
                    case Opcode::UDiv:
                        return "udiv";
                    case Opcode::SRem:
                        return "srem";
                    case Opcode::URem:
                        return "urem";
                    case Opcode::And:
                        return "and";
                    case Opcode::Or:
                        return "or";
                    case Opcode::Xor:
                        return "xor";
                    case Opcode::Shl:
                        return "shl";
                    case Opcode::LShr:
                        return "lshr";
                    case Opcode::AShr:
                        return "ashr";
                    case Opcode::FAdd:
                        return "fadd";
                    case Opcode::FSub:
                        return "fsub";
                    case Opcode::FMul:
                        return "fmul";
                    case Opcode::FDiv:
                        return "fdiv";
                    default:
                        throw std::runtime_error("ZirLlvmBackend: not a binop");
                }
            }

            static const char *castKeyword(Opcode op) {
                switch (op) {
                    case Opcode::Trunc:
                        return "trunc";
                    case Opcode::ZExt:
                        return "zext";
                    case Opcode::SExt:
                        return "sext";
                    case Opcode::FPTrunc:
                        return "fptrunc";
                    case Opcode::FPExt:
                        return "fpext";
                    case Opcode::FPToSI:
                        return "fptosi";
                    case Opcode::FPToUI:
                        return "fptoui";
                    case Opcode::SIToFP:
                        return "sitofp";
                    case Opcode::UIToFP:
                        return "uitofp";
                    case Opcode::PtrToInt:
                        return "ptrtoint";
                    case Opcode::IntToPtr:
                        return "inttoptr";
                    default:
                        throw std::runtime_error("ZirLlvmBackend: not a cast");
                }
            }

            void emitTerminator(const Terminator &t) {
                switch (t.kind) {
                    case TermKind::Br:
                        out_ << "  br label %" << fn_.block(t.targets[0].block).label() << "\n";
                        return;
                    case TermKind::CondBr:
                        out_ << "  br i1 " << ref(t.cond) << ", label %" << fn_.block(t.targets[0].block).label()
                             << ", label %" << fn_.block(t.targets[1].block).label() << "\n";
                        return;
                    case TermKind::Ret:
                        if (t.retValue.isValid())
                            out_ << "  ret " << typedRef(t.retValue) << "\n";
                        else
                            out_ << "  ret void\n";
                        return;
                    case TermKind::Unreachable:
                        out_ << "  unreachable\n";
                        return;
                    case TermKind::Switch: {
                        out_ << "  switch " << typedRef(t.cond) << ", label %"
                             << fn_.block(t.targets[0].block).label() << " [\n";
                        TypeId condTy = fn_.typeOf(t.cond);
                        for (std::size_t i = 1; i < t.targets.size(); ++i) {
                            out_ << "    " << llvmType(m_.types(), condTy) << " " << t.caseValues[i - 1]
                                 << ", label %" << fn_.block(t.targets[i].block).label() << "\n";
                        }
                        out_ << "  ]\n";
                        return;
                    }
                }
                throw std::runtime_error("ZirLlvmBackend: unhandled terminator kind");
            }

            const Module &m_;
            const Function &fn_;
            std::ostream &out_;
            std::vector<std::string> valueText_;
            std::unordered_map<ValueId::Value, InstId> defSite_;
        };

    }  // namespace

    void ZirLlvmBackend::emit(const Module &m, std::ostream &out) {
        out << "; ModuleID = '" << m.sourceName() << "'\n"
            << "source_filename = \"" << m.sourceName() << "\"\n\n";

        for (const GlobalVar &g : m.globals()) {
            out << "@" << g.name << " = " << (g.isPrivate ? "private " : "") << (g.isConstant ? "constant " : "global ")
                << llvmType(m.types(), g.type) << " ";
            if (g.hasInit)
                out << "c\"" << llvmEscapeBytes(g.initBytes) << "\"";
            else
                out << "zeroinitializer";
            out << "\n";
        }
        out << "\n";

        // Declarations before definitions -- purely for readability; LLVM
        // textual IR does not require forward declaration order.
        for (const Function &fn : m.functions()) {
            if (fn.isExtern())
                FunctionEmitter(m, fn, out).emitDeclaration();
        }
        for (const Function &fn : m.functions()) {
            if (!fn.isExtern())
                FunctionEmitter(m, fn, out).emitDefinition();
        }
    }

}  // namespace zust
