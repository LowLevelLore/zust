#include "zir/passes/DCE.hpp"

#include "zir/Uses.hpp"

namespace zust::zir {

    namespace {
        bool hasSideEffect(Opcode op) { return op == Opcode::Store || op == Opcode::Call; }
    }  // namespace

    bool DCEPass::run(Function &fn, AnalysisManager &) {
        std::vector<int> uses = countUses(fn);
        bool changed = false;

        for (std::size_t bi = 0; bi < fn.blockCount(); ++bi) {
            BasicBlock &block = fn.block(BlockId(static_cast<BlockId::Value>(bi)));
            std::vector<InstId> &insts = block.insts();
            std::vector<InstId> kept;
            kept.reserve(insts.size());
            for (InstId iid : insts) {
                const Instruction &inst = fn.inst(iid);
                bool dead = inst.result.isValid() && !hasSideEffect(inst.op) && uses[inst.result.value()] == 0;
                if (dead) {
                    changed = true;
                } else {
                    kept.push_back(iid);
                }
            }
            if (kept.size() != insts.size())
                insts = std::move(kept);
        }
        return changed;
    }

}  // namespace zust::zir
