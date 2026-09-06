#include "zir/passes/InstCombine.hpp"

namespace zust::zir {

    namespace {
        // Rewrites `inst` in place into a same-type Bitcast of `replacement`
        // -- a genuine no-op both at the ZIR level and, per
        // ZirLlvmBackend's own no-op-bitcast handling, at codegen time. This
        // is instcombine's one output shape for "this instruction's value
        // is simply some other already-computed value": it needs no new
        // ValueId, and DCE cleans up the (now unused, side-effect-free)
        // original operands on its own.
        void replaceWith(Instruction &inst, ValueId replacement) {
            inst.op = Opcode::Bitcast;
            inst.operands = {replacement};
        }

        void replaceWithConst(Instruction &inst, std::uint64_t bits) {
            inst.op = Opcode::Const;
            inst.operands.clear();
            inst.constant.bits = bits;
        }
    }  // namespace

    bool InstCombinePass::run(Function &fn, AnalysisManager &) {
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
        auto isZero = [&](ValueId v) { return isConst[v.value()] && bits[v.value()] == 0; };
        auto isOne = [&](ValueId v) { return isConst[v.value()] && bits[v.value()] == 1; };

        bool changed = false;
        for (std::size_t bi = 0; bi < fn.blockCount(); ++bi) {
            for (InstId iid : fn.block(BlockId(static_cast<BlockId::Value>(bi))).insts()) {
                Instruction &inst = fn.inst(iid);
                if (inst.operands.size() != 2)
                    continue;
                ValueId a = inst.operands[0], b = inst.operands[1];

                switch (inst.op) {
                    case Opcode::Add:
                    case Opcode::FAdd:
                        if (isZero(b)) {
                            replaceWith(inst, a);
                            changed = true;
                        } else if (isZero(a)) {
                            replaceWith(inst, b);
                            changed = true;
                        }
                        break;
                    case Opcode::Sub:
                    case Opcode::FSub:
                        if (isZero(b)) {
                            replaceWith(inst, a);
                            changed = true;
                        } else if (a == b) {
                            replaceWithConst(inst, 0);
                            changed = true;
                        }
                        break;
                    case Opcode::Mul:
                    case Opcode::FMul:
                        if (isOne(b)) {
                            replaceWith(inst, a);
                            changed = true;
                        } else if (isOne(a)) {
                            replaceWith(inst, b);
                            changed = true;
                        } else if (inst.op == Opcode::Mul && (isZero(a) || isZero(b))) {
                            replaceWithConst(inst, 0);
                            changed = true;
                        }
                        break;
                    case Opcode::SDiv:
                    case Opcode::UDiv:
                    case Opcode::FDiv:
                        if (isOne(b)) {
                            replaceWith(inst, a);
                            changed = true;
                        }
                        break;
                    case Opcode::Xor:
                        if (isZero(b)) {
                            replaceWith(inst, a);
                            changed = true;
                        } else if (isZero(a)) {
                            replaceWith(inst, b);
                            changed = true;
                        } else if (a == b) {
                            replaceWithConst(inst, 0);
                            changed = true;
                        }
                        break;
                    case Opcode::And:
                        if (a == b) {
                            replaceWith(inst, a);
                            changed = true;
                        } else if (isZero(a) || isZero(b)) {
                            replaceWithConst(inst, 0);
                            changed = true;
                        }
                        break;
                    case Opcode::Or:
                        if (a == b) {
                            replaceWith(inst, a);
                            changed = true;
                        } else if (isZero(b)) {
                            replaceWith(inst, a);
                            changed = true;
                        } else if (isZero(a)) {
                            replaceWith(inst, b);
                            changed = true;
                        }
                        break;
                    default:
                        break;
                }
            }
        }
        return changed;
    }

}  // namespace zust::zir
