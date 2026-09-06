#include "zir/passes/ConstFold.hpp"

#include <cstring>
#include <optional>

namespace zust::zir {

    namespace {
        std::uint64_t maskTo(std::uint32_t bits, std::uint64_t v) {
            if (bits >= 64)
                return v;
            return v & ((std::uint64_t{1} << bits) - 1);
        }

        std::int64_t signExtendFrom(std::uint32_t bits, std::uint64_t v) {
            if (bits >= 64)
                return static_cast<std::int64_t>(v);
            std::uint64_t signBit = std::uint64_t{1} << (bits - 1);
            std::uint64_t m = maskTo(bits, v);
            return static_cast<std::int64_t>((m ^ signBit) - signBit);
        }

        double bitsToFloatingPoint(std::uint32_t bits, std::uint64_t pattern) {
            if (bits == 32) {
                std::uint32_t b32 = static_cast<std::uint32_t>(pattern);
                float f;
                std::memcpy(&f, &b32, sizeof(f));
                return static_cast<double>(f);
            }
            double d;
            std::memcpy(&d, &pattern, sizeof(d));
            return d;
        }

        std::uint64_t floatingPointToBits(std::uint32_t bits, double v) {
            if (bits == 32) {
                float f = static_cast<float>(v);
                std::uint32_t b32;
                std::memcpy(&b32, &f, sizeof(f));
                return b32;
            }
            std::uint64_t b64;
            std::memcpy(&b64, &v, sizeof(v));
            return b64;
        }

        // Returns nullopt when the operand isn't (yet) known constant, or
        // when folding would require modeling something this pass
        // deliberately declines to (division/remainder by a constant zero,
        // a shift amount at or past the type's width) -- in every such
        // case the instruction is left alone rather than guessing at
        // undefined-behavior semantics.
        std::optional<std::uint64_t> foldBinop(Opcode op, const Type &ty, std::uint64_t a, std::uint64_t b) {
            std::uint32_t bits = ty.bits;
            switch (op) {
                case Opcode::Add:
                    return maskTo(bits, a + b);
                case Opcode::Sub:
                    return maskTo(bits, a - b);
                case Opcode::Mul:
                    return maskTo(bits, a * b);
                case Opcode::SDiv: {
                    std::int64_t bs = signExtendFrom(bits, b);
                    if (bs == 0)
                        return std::nullopt;
                    return maskTo(bits, static_cast<std::uint64_t>(signExtendFrom(bits, a) / bs));
                }
                case Opcode::UDiv:
                    if (maskTo(bits, b) == 0)
                        return std::nullopt;
                    return maskTo(bits, maskTo(bits, a) / maskTo(bits, b));
                case Opcode::SRem: {
                    std::int64_t bs = signExtendFrom(bits, b);
                    if (bs == 0)
                        return std::nullopt;
                    return maskTo(bits, static_cast<std::uint64_t>(signExtendFrom(bits, a) % bs));
                }
                case Opcode::URem:
                    if (maskTo(bits, b) == 0)
                        return std::nullopt;
                    return maskTo(bits, maskTo(bits, a) % maskTo(bits, b));
                case Opcode::And:
                    return maskTo(bits, a & b);
                case Opcode::Or:
                    return maskTo(bits, a | b);
                case Opcode::Xor:
                    return maskTo(bits, a ^ b);
                case Opcode::Shl:
                    if (b >= bits)
                        return std::nullopt;
                    return maskTo(bits, a << b);
                case Opcode::LShr:
                    if (b >= bits)
                        return std::nullopt;
                    return maskTo(bits, maskTo(bits, a) >> b);
                case Opcode::AShr:
                    if (b >= bits)
                        return std::nullopt;
                    return maskTo(bits, static_cast<std::uint64_t>(signExtendFrom(bits, a) >> b));
                case Opcode::FAdd:
                    return floatingPointToBits(bits, bitsToFloatingPoint(bits, a) + bitsToFloatingPoint(bits, b));
                case Opcode::FSub:
                    return floatingPointToBits(bits, bitsToFloatingPoint(bits, a) - bitsToFloatingPoint(bits, b));
                case Opcode::FMul:
                    return floatingPointToBits(bits, bitsToFloatingPoint(bits, a) * bitsToFloatingPoint(bits, b));
                case Opcode::FDiv:
                    return floatingPointToBits(bits, bitsToFloatingPoint(bits, a) / bitsToFloatingPoint(bits, b));
                default:
                    return std::nullopt;
            }
        }

        std::optional<std::uint64_t> foldCmp(Opcode op, CmpPred pred, const Type &operandTy, std::uint64_t a,
                                             std::uint64_t b) {
            std::uint32_t bits = operandTy.bits;
            if (op == Opcode::ICmp) {
                bool result;
                switch (pred) {
                    case CmpPred::Eq:
                        result = maskTo(bits, a) == maskTo(bits, b);
                        break;
                    case CmpPred::Ne:
                        result = maskTo(bits, a) != maskTo(bits, b);
                        break;
                    case CmpPred::Slt:
                        result = signExtendFrom(bits, a) < signExtendFrom(bits, b);
                        break;
                    case CmpPred::Sle:
                        result = signExtendFrom(bits, a) <= signExtendFrom(bits, b);
                        break;
                    case CmpPred::Sgt:
                        result = signExtendFrom(bits, a) > signExtendFrom(bits, b);
                        break;
                    case CmpPred::Sge:
                        result = signExtendFrom(bits, a) >= signExtendFrom(bits, b);
                        break;
                    case CmpPred::Ult:
                        result = maskTo(bits, a) < maskTo(bits, b);
                        break;
                    case CmpPred::Ule:
                        result = maskTo(bits, a) <= maskTo(bits, b);
                        break;
                    case CmpPred::Ugt:
                        result = maskTo(bits, a) > maskTo(bits, b);
                        break;
                    case CmpPred::Uge:
                        result = maskTo(bits, a) >= maskTo(bits, b);
                        break;
                    default:
                        return std::nullopt;
                }
                return result ? 1u : 0u;
            }
            if (op == Opcode::FCmp) {
                double da = bitsToFloatingPoint(bits, a), db = bitsToFloatingPoint(bits, b);
                bool result;
                switch (pred) {
                    case CmpPred::Oeq:
                        result = da == db;
                        break;
                    case CmpPred::One:
                        result = da != db;
                        break;
                    case CmpPred::Olt:
                        result = da < db;
                        break;
                    case CmpPred::Ole:
                        result = da <= db;
                        break;
                    case CmpPred::Ogt:
                        result = da > db;
                        break;
                    case CmpPred::Oge:
                        result = da >= db;
                        break;
                    default:
                        return std::nullopt;
                }
                return result ? 1u : 0u;
            }
            return std::nullopt;
        }

        std::optional<std::uint64_t> foldUnop(Opcode op, const Type &ty, std::uint64_t a) {
            switch (op) {
                case Opcode::Neg:
                    if (ty.kind == TypeKind::Float)
                        return floatingPointToBits(ty.bits, -bitsToFloatingPoint(ty.bits, a));
                    return maskTo(ty.bits, static_cast<std::uint64_t>(0) - a);
                case Opcode::Not:
                    return maskTo(ty.bits, ~a);
                default:
                    return std::nullopt;
            }
        }

        std::optional<std::uint64_t> foldCast(Opcode op, const Type &fromTy, const Type &toTy, std::uint64_t a) {
            switch (op) {
                case Opcode::Trunc:
                    return maskTo(toTy.bits, a);
                case Opcode::ZExt:
                    return maskTo(toTy.bits, maskTo(fromTy.bits, a));
                case Opcode::SExt:
                    return maskTo(toTy.bits, static_cast<std::uint64_t>(signExtendFrom(fromTy.bits, a)));
                case Opcode::FPTrunc:
                case Opcode::FPExt:
                    return floatingPointToBits(toTy.bits, bitsToFloatingPoint(fromTy.bits, a));
                case Opcode::FPToSI:
                    return maskTo(toTy.bits, static_cast<std::uint64_t>(
                                                 static_cast<std::int64_t>(bitsToFloatingPoint(fromTy.bits, a))));
                case Opcode::FPToUI:
                    return maskTo(toTy.bits, static_cast<std::uint64_t>(bitsToFloatingPoint(fromTy.bits, a)));
                case Opcode::SIToFP:
                    return floatingPointToBits(toTy.bits, static_cast<double>(signExtendFrom(fromTy.bits, a)));
                case Opcode::UIToFP:
                    return floatingPointToBits(toTy.bits, static_cast<double>(maskTo(fromTy.bits, a)));
                case Opcode::Bitcast:
                    // ZirGen only ever bitcasts ptr<->ptr or same-width
                    // differently-signed ints (see ZirGen::castTo) -- both
                    // cases are a pure bit-pattern passthrough.
                    return a;
                default:
                    return std::nullopt;
            }
        }
    }  // namespace

    bool ConstFoldPass::run(Function &fn, AnalysisManager &) {
        std::vector<bool> isConst(fn.valueCount(), false);
        std::vector<std::uint64_t> bits(fn.valueCount(), 0);
        for (std::size_t bi = 0; bi < fn.blockCount(); ++bi) {
            for (InstId iid : fn.block(BlockId(static_cast<BlockId::Value>(bi))).insts()) {
                const Instruction &inst = fn.inst(iid);
                if (inst.op == Opcode::Const && inst.result.isValid()) {
                    isConst[inst.result.value()] = true;
                    bits[inst.result.value()] = inst.constant.bits;
                }
            }
        }

        bool changed = false;
        for (std::size_t bi = 0; bi < fn.blockCount(); ++bi) {
            for (InstId iid : fn.block(BlockId(static_cast<BlockId::Value>(bi))).insts()) {
                Instruction &inst = fn.inst(iid);
                if (inst.op == Opcode::Const || !inst.result.isValid())
                    continue;

                std::optional<std::uint64_t> folded;
                switch (inst.op) {
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
                        if (inst.operands.size() == 2 && isConst[inst.operands[0].value()] &&
                            isConst[inst.operands[1].value()])
                            folded = foldBinop(inst.op, module_.types().get(inst.type), bits[inst.operands[0].value()],
                                               bits[inst.operands[1].value()]);
                        break;
                    case Opcode::ICmp:
                    case Opcode::FCmp:
                        if (inst.operands.size() == 2 && isConst[inst.operands[0].value()] &&
                            isConst[inst.operands[1].value()])
                            folded = foldCmp(inst.op, inst.pred, module_.types().get(fn.typeOf(inst.operands[0])),
                                             bits[inst.operands[0].value()], bits[inst.operands[1].value()]);
                        break;
                    case Opcode::Neg:
                    case Opcode::Not:
                        if (inst.operands.size() == 1 && isConst[inst.operands[0].value()])
                            folded = foldUnop(inst.op, module_.types().get(inst.type), bits[inst.operands[0].value()]);
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
                    case Opcode::Bitcast:
                        if (inst.operands.size() == 1 && isConst[inst.operands[0].value()])
                            folded = foldCast(inst.op, module_.types().get(fn.typeOf(inst.operands[0])),
                                              module_.types().get(inst.type), bits[inst.operands[0].value()]);
                        break;
                    default:
                        break;
                }

                if (folded) {
                    inst.op = Opcode::Const;
                    inst.constant.bits = *folded;
                    inst.operands.clear();
                    isConst[inst.result.value()] = true;
                    bits[inst.result.value()] = *folded;
                    changed = true;
                }
            }
        }
        return changed;
    }

}  // namespace zust::zir
