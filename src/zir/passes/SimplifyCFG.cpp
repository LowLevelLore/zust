#include "zir/passes/SimplifyCFG.hpp"

#include <unordered_map>

#include "zir/DominatorTree.hpp"

namespace zust::zir {

    namespace {
        void substituteValue(ValueId &v, const std::unordered_map<ValueId::Value, ValueId> &subst) {
            if (!v.isValid())
                return;
            auto it = subst.find(v.value());
            if (it != subst.end())
                v = it->second;
        }

        void substituteInBlock(BasicBlock &block, Function &fn, const std::unordered_map<ValueId::Value, ValueId> &subst) {
            for (InstId iid : block.insts()) {
                Instruction &inst = fn.inst(iid);
                for (ValueId &operand : inst.operands) substituteValue(operand, subst);
            }
            Terminator &t = block.term();
            substituteValue(t.cond, subst);
            substituteValue(t.retValue, subst);
            for (BlockRef &ref : t.targets)
                for (ValueId &arg : ref.args) substituteValue(arg, subst);
        }

        // Looks up whether `v` is defined by a still-in-place `const`
        // instruction somewhere in `fn`, returning its bit pattern. A plain
        // linear scan (fine at this function size) rather than a persistent
        // def-site map, since this pass only needs it for condbr
        // conditions, not every value.
        bool findConstBits(const Function &fn, ValueId v, std::uint64_t &bitsOut) {
            for (std::size_t bi = 0; bi < fn.blockCount(); ++bi) {
                for (InstId iid : fn.block(BlockId(static_cast<BlockId::Value>(bi))).insts()) {
                    const Instruction &inst = fn.inst(iid);
                    if (inst.result == v && inst.op == Opcode::Const) {
                        bitsOut = inst.constant.bits;
                        return true;
                    }
                }
            }
            return false;
        }

        bool simplifyConstantConditions(Function &fn) {
            bool changed = false;
            for (std::size_t bi = 0; bi < fn.blockCount(); ++bi) {
                Terminator &t = fn.block(BlockId(static_cast<BlockId::Value>(bi))).term();
                if (t.kind != TermKind::CondBr)
                    continue;
                std::uint64_t bits;
                if (!findConstBits(fn, t.cond, bits))
                    continue;
                BlockRef chosen = bits != 0 ? t.targets[0] : t.targets[1];
                t.kind = TermKind::Br;
                t.cond = ValueId{};
                t.targets = {std::move(chosen)};
                changed = true;
            }
            return changed;
        }

        bool mergeStraightLineBlocks(Function &fn) {
            DominatorTree dt(fn);
            for (std::size_t bi = 0; bi < fn.blockCount(); ++bi) {
                BlockId a(static_cast<BlockId::Value>(bi));
                if (!dt.isReachable(a))
                    continue;
                Terminator &aTerm = fn.block(a).term();
                if (aTerm.kind != TermKind::Br)
                    continue;
                BlockId b = aTerm.targets[0].block;
                if (b == a)
                    continue;  // a single-block infinite loop -- nothing to merge
                if (dt.predecessors(b).size() != 1)
                    continue;  // b has another edge into it; merging would drop it

                std::vector<ValueId> args = aTerm.targets[0].args;
                const std::vector<ValueId> &params = fn.block(b).params();
                if (args.size() != params.size())
                    continue;  // malformed input; leave it for the Verifier to reject

                std::unordered_map<ValueId::Value, ValueId> subst;
                for (std::size_t i = 0; i < params.size(); ++i) subst[params[i].value()] = args[i];
                if (!subst.empty())
                    substituteInBlock(fn.block(b), fn, subst);

                // Re-parent b's instructions onto a, then hand a b's
                // (already-substituted) terminator. b keeps its BlockId
                // (nothing else may reference it, but stable indices mean
                // it can't simply be erased) but is left with nothing in
                // it and an inert terminator -- unreferenced and removable,
                // which the Verifier's predecessor check explicitly allows.
                std::vector<InstId> &aInsts = fn.block(a).insts();
                std::vector<InstId> &bInsts = fn.block(b).insts();
                aInsts.insert(aInsts.end(), bInsts.begin(), bInsts.end());
                fn.block(a).term() = fn.block(b).term();
                bInsts.clear();
                fn.block(b).term() = Terminator{};
                return true;  // CFG shape changed -- let the caller re-derive dominance before continuing
            }
            return false;
        }
        // Folding away a CondBr arm (or a merge) can leave a whole chain of
        // blocks unreachable from entry while they still point at each
        // other -- the Verifier's predecessor check (docs/IR-DESIGN.md
        // check 5) only tolerates a dead block with *zero* predecessors
        // ("unreferenced and removable"), not one that still dangles off an
        // equally-dead block. One reachability pass catches a whole dead
        // subgraph at once (BFS from entry is transitive), so every block
        // in it gets cleared to a real, harmless "nothing here, points
        // nowhere" stub in the same call -- not just the block that lost
        // its edge directly.
        bool clearUnreachableBlocks(Function &fn) {
            DominatorTree dt(fn);
            bool changed = false;
            for (std::size_t bi = 0; bi < fn.blockCount(); ++bi) {
                BlockId b(static_cast<BlockId::Value>(bi));
                if (b == fn.entry() || dt.isReachable(b))
                    continue;
                BasicBlock &block = fn.block(b);
                bool alreadyClear = block.insts().empty() && block.term().kind == TermKind::Unreachable;
                if (alreadyClear)
                    continue;
                block.insts().clear();
                block.term() = Terminator{};
                changed = true;
            }
            return changed;
        }
    }  // namespace

    bool SimplifyCFGPass::run(Function &fn, AnalysisManager &) {
        // One category of change per call: each invalidates the next one's
        // precomputed view of the CFG (a condbr simplification changes who
        // has how many predecessors; a merge changes which blocks even
        // exist as separate entities; clearing dead blocks changes
        // reachability), so the fixpoint loop sees the effect of one before
        // this pass looks at the CFG again for the next.
        if (simplifyConstantConditions(fn))
            return true;
        if (mergeStraightLineBlocks(fn))
            return true;
        return clearUnreachableBlocks(fn);
    }

}  // namespace zust::zir
