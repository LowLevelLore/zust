#include "zir/Printer.hpp"

#include <cstring>
#include <iomanip>
#include <sstream>
#include <stdexcept>

namespace zust::zir {

    namespace {

        std::string typeStr(const TypeTable &table, TypeId id) { return Printer::printType(table, id); }

        std::string opcodeKeyword(Opcode op) {
            switch (op) {
                case Opcode::Const:
                    return "const";
                case Opcode::Alloca:
                    return "alloca";
                case Opcode::Load:
                    return "load";
                case Opcode::Store:
                    return "store";
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
                case Opcode::ICmp:
                    return "icmp";
                case Opcode::FCmp:
                    return "fcmp";
                case Opcode::Neg:
                    return "neg";
                case Opcode::Not:
                    return "not";
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
                case Opcode::Bitcast:
                    return "bitcast";
                case Opcode::Gep:
                    return "gep";
                case Opcode::Call:
                    return "call";
                case Opcode::Select:
                    return "select";
            }
            throw std::runtime_error("opcodeKeyword: unknown Opcode");
        }

        std::string cmpPredKeyword(CmpPred pred) {
            switch (pred) {
                case CmpPred::None:
                    throw std::runtime_error("cmpPredKeyword: CmpPred::None on a comparison instruction");
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
            }
            throw std::runtime_error("cmpPredKeyword: unknown CmpPred");
        }

        // Sign-extends the low `width` bits of `bits` to a full 64-bit
        // signed value, for printing a signed integer constant as decimal.
        std::int64_t signExtend(std::uint64_t bits, std::uint32_t width) {
            if (width == 0 || width >= 64)
                return static_cast<std::int64_t>(bits);
            std::uint64_t mask = (std::uint64_t{1} << width) - 1;
            std::uint64_t v = bits & mask;
            std::uint64_t signBit = std::uint64_t{1} << (width - 1);
            if (v & signBit)
                v |= ~mask;
            return static_cast<std::int64_t>(v);
        }

        std::uint64_t zeroExtendMask(std::uint64_t bits, std::uint32_t width) {
            if (width == 0 || width >= 64)
                return bits;
            std::uint64_t mask = (std::uint64_t{1} << width) - 1;
            return bits & mask;
        }

        std::string formatIntConst(const Type &t, std::uint64_t bits) {
            if (t.isSigned)
                return std::to_string(signExtend(bits, t.bits));
            return std::to_string(zeroExtendMask(bits, t.bits));
        }

        std::string formatFloatConst(const Type &t, std::uint64_t bitsPattern) {
            std::ostringstream out;
            if (t.bits == 32) {
                float f;
                std::uint32_t bits32 = static_cast<std::uint32_t>(bitsPattern);
                std::memcpy(&f, &bits32, sizeof(f));
                out << std::setprecision(9) << static_cast<double>(f);
            } else {
                double d;
                std::memcpy(&d, &bitsPattern, sizeof(d));
                out << std::setprecision(17) << d;
            }
            return out.str();
        }

        // LLVM-style byte-string escaping: printable ASCII passes through
        // (except '"' and '\\', always escaped so the result round-trips
        // through a naive scanner), everything else becomes `\XX` uppercase
        // hex. Confirmed against docs/IR-DESIGN.md's own example: bytes
        // '%','d',0x0A,0x00 print as `%d\0A\00`.
        std::string escapeBytes(const std::string &bytes) {
            static const char *kHex = "0123456789ABCDEF";
            std::string out;
            out.reserve(bytes.size());
            for (unsigned char c : bytes) {
                if (c == '"' || c == '\\' || c < 0x20 || c > 0x7E) {
                    out += '\\';
                    out += kHex[(c >> 4) & 0xF];
                    out += kHex[c & 0xF];
                } else {
                    out += static_cast<char>(c);
                }
            }
            return out;
        }

        std::string valueRef(const Function &fn, ValueId v) { return "%" + fn.nameOf(v); }

        std::string blockRef(const Function &fn, const BlockRef &target) {
            std::string s = "^" + fn.block(target.block).label();
            if (!target.args.empty()) {
                s += "(";
                for (std::size_t i = 0; i < target.args.size(); ++i) {
                    s += valueRef(fn, target.args[i]);
                    if (i + 1 < target.args.size())
                        s += ", ";
                }
                s += ")";
            }
            return s;
        }

        std::string printInstBody(const Module &m, const Function &fn, const Instruction &inst) {
            const TypeTable &table = m.types();
            std::ostringstream out;
            switch (inst.op) {
                case Opcode::Const:
                    out << "const " << typeStr(table, inst.type) << " ";
                    if (table.get(inst.type).kind == TypeKind::Float)
                        out << formatFloatConst(table.get(inst.type), inst.constant.bits);
                    else
                        out << formatIntConst(table.get(inst.type), inst.constant.bits);
                    break;
                case Opcode::Alloca:
                    out << "alloca " << typeStr(table, inst.elemType);
                    if (inst.align != 0)
                        out << ", align " << inst.align;
                    break;
                case Opcode::Load:
                    out << "load " << typeStr(table, inst.type) << ", " << valueRef(fn, inst.operands[0]);
                    break;
                case Opcode::Store:
                    out << "store " << typeStr(table, fn.typeOf(inst.operands[0])) << " " << valueRef(fn, inst.operands[0])
                        << ", " << valueRef(fn, inst.operands[1]);
                    break;
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
                case Opcode::FDiv:
                    out << opcodeKeyword(inst.op) << " " << typeStr(table, inst.type) << " " << valueRef(fn, inst.operands[0])
                        << ", " << valueRef(fn, inst.operands[1]);
                    break;
                case Opcode::ICmp:
                case Opcode::FCmp:
                    out << opcodeKeyword(inst.op) << " " << cmpPredKeyword(inst.pred) << " "
                        << typeStr(table, fn.typeOf(inst.operands[0])) << " " << valueRef(fn, inst.operands[0]) << ", "
                        << valueRef(fn, inst.operands[1]);
                    break;
                case Opcode::Neg:
                case Opcode::Not:
                    out << opcodeKeyword(inst.op) << " " << typeStr(table, inst.type) << " " << valueRef(fn, inst.operands[0]);
                    break;
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
                case Opcode::IntToPtr:
                case Opcode::Bitcast:
                    out << opcodeKeyword(inst.op) << " " << valueRef(fn, inst.operands[0]) << " to "
                        << typeStr(table, inst.type);
                    break;
                case Opcode::Gep: {
                    out << "gep " << typeStr(table, inst.elemType) << ", " << valueRef(fn, inst.operands[0]);
                    for (std::size_t i = 1; i < inst.operands.size(); ++i) out << ", " << valueRef(fn, inst.operands[i]);
                    break;
                }
                case Opcode::Call: {
                    const Function &callee = m.function(inst.callee);
                    if (inst.result.isValid())
                        out << "call " << typeStr(table, inst.type) << " @" << callee.name() << "(";
                    else
                        out << "call void @" << callee.name() << "(";
                    for (std::size_t i = 0; i < inst.operands.size(); ++i) {
                        out << valueRef(fn, inst.operands[i]);
                        if (i + 1 < inst.operands.size())
                            out << ", ";
                    }
                    out << ")";
                    break;
                }
                case Opcode::Select:
                    out << "select " << valueRef(fn, inst.operands[0]) << ", " << valueRef(fn, inst.operands[1]) << ", "
                        << valueRef(fn, inst.operands[2]);
                    break;
            }
            return out.str();
        }

        std::string printTerminator(const Module &m, const Function &fn, const Terminator &t) {
            std::ostringstream out;
            switch (t.kind) {
                case TermKind::Br:
                    out << "br " << blockRef(fn, t.targets[0]);
                    break;
                case TermKind::CondBr:
                    out << "condbr " << valueRef(fn, t.cond) << ", " << blockRef(fn, t.targets[0]) << ", "
                        << blockRef(fn, t.targets[1]);
                    break;
                case TermKind::Ret:
                    if (t.retValue.isValid())
                        out << "ret " << typeStr(m.types(), fn.typeOf(t.retValue)) << " " << valueRef(fn, t.retValue);
                    else
                        out << "ret void";
                    break;
                case TermKind::Switch: {
                    out << "switch " << typeStr(m.types(), fn.typeOf(t.cond)) << " " << valueRef(fn, t.cond)
                        << ", default " << blockRef(fn, t.targets[0]) << " [";
                    for (std::size_t i = 1; i < t.targets.size(); ++i) {
                        out << " " << t.caseValues[i - 1] << " " << blockRef(fn, t.targets[i]);
                        if (i + 1 < t.targets.size())
                            out << ",";
                    }
                    out << " ]";
                    break;
                }
                case TermKind::Unreachable:
                    out << "unreachable";
                    break;
            }
            return out.str();
        }

    }  // namespace

    std::string Printer::printType(const TypeTable &table, TypeId id) {
        const Type &t = table.get(id);
        switch (t.kind) {
            case TypeKind::Void:
                return "void";
            case TypeKind::Int:
                // "i<bits>" for signed, "u<bits>" for unsigned -- a choice
                // this printer makes since docs/IR-DESIGN.md's own example
                // never needs to disambiguate the two (see zust-ir Wave 1.3
                // notes: signedness lives on the Type per the spec's own
                // grammar, but the shown textual form doesn't distinguish,
                // so this is this implementation's textual convention, not
                // something the spec dictates).
                return (t.isSigned ? "i" : "u") + std::to_string(t.bits);
            case TypeKind::Float:
                return "f" + std::to_string(t.bits);
            case TypeKind::Ptr:
                return "ptr";
            case TypeKind::Array:
                return "[" + std::to_string(t.arrayLen) + " x " + printType(table, t.elem) + "]";
            case TypeKind::Fn: {
                std::string s = "fn(";
                for (std::size_t i = 0; i < t.params.size(); ++i) {
                    s += printType(table, t.params[i]);
                    if (i + 1 < t.params.size() || t.variadic)
                        s += ", ";
                }
                if (t.variadic)
                    s += "...";
                s += ") -> " + printType(table, t.ret);
                return s;
            }
        }
        throw std::runtime_error("Printer::printType: unknown TypeKind");
    }

    std::string Printer::print(const Module &m) {
        std::ostringstream out;
        print(m, out);
        return out.str();
    }

    void Printer::print(const Module &m, std::ostream &out) {
        out << "; " << m.sourceName() << "\n";
        out << "module \"" << m.sourceName() << "\" target = \"" << m.targetName() << "\"\n";

        for (const GlobalVar &g : m.globals()) {
            out << "\n";
            out << "@" << g.name << " = " << (g.isPrivate ? "private " : "") << (g.isConstant ? "constant" : "global")
                << " " << printType(m.types(), g.type);
            if (g.hasInit)
                out << " c\"" << escapeBytes(g.initBytes) << "\"";
            out << "\n";
        }

        for (const Function &fn : m.functions()) {
            if (!fn.isExtern())
                continue;
            out << "\n";
            const Type &sig = m.types().get(fn.signature());
            out << "declare " << printType(m.types(), sig.ret) << " @" << fn.name() << "(";
            for (std::size_t i = 0; i < sig.params.size(); ++i) {
                out << printType(m.types(), sig.params[i]);
                if (i + 1 < sig.params.size() || sig.variadic)
                    out << ", ";
            }
            if (sig.variadic)
                out << "...";
            out << ")";
            if (fn.isVariadic())
                out << " variadic";
            out << "\n";
        }

        for (const Function &fn : m.functions()) {
            if (fn.isExtern())
                continue;
            out << "\n";
            const BasicBlock &entryBlock = fn.block(fn.entry());
            const Type &sig = m.types().get(fn.signature());
            out << "fn @" << fn.name() << "(";
            const auto &entryParams = entryBlock.params();
            for (std::size_t i = 0; i < entryParams.size(); ++i) {
                out << valueRef(fn, entryParams[i]) << ": " << printType(m.types(), fn.typeOf(entryParams[i]));
                if (i + 1 < entryParams.size())
                    out << ", ";
            }
            out << ") -> " << printType(m.types(), sig.ret) << " {\n";

            for (std::size_t bi = 0; bi < fn.blockCount(); ++bi) {
                if (bi > 0)
                    out << "\n";
                const BasicBlock &block = fn.block(BlockId(static_cast<BlockId::Value>(bi)));
                out << "^" << block.label();
                if (!block.params().empty()) {
                    out << "(";
                    for (std::size_t i = 0; i < block.params().size(); ++i) {
                        out << valueRef(fn, block.params()[i]) << ": "
                            << printType(m.types(), fn.typeOf(block.params()[i]));
                        if (i + 1 < block.params().size())
                            out << ", ";
                    }
                    out << ")";
                }
                out << ":\n";

                // Fixed-width alignment of the "= " column, matching
                // docs/IR-DESIGN.md's own example (every shown name pads to
                // 6 characters before "="; see the Wave 1.3 notes on why
                // this implementation picked a fixed width over a
                // per-block-computed one).
                constexpr std::size_t kNameColumn = 6;
                for (InstId id : block.insts()) {
                    const Instruction &inst = fn.inst(id);
                    out << "    ";
                    if (inst.result.isValid()) {
                        std::string name = valueRef(fn, inst.result);
                        out << name;
                        std::size_t pad = name.size() < kNameColumn - 1 ? kNameColumn - name.size() : 1;
                        out << std::string(pad, ' ') << "= ";
                    }
                    out << printInstBody(m, fn, inst) << "\n";
                }
                out << "    " << printTerminator(m, fn, block.term()) << "\n";
            }

            out << "}\n";
        }
    }

}  // namespace zust::zir
