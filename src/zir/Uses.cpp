#include "zir/Uses.hpp"

namespace zust::zir {

    std::vector<int> countUses(const Function &fn) {
        std::vector<int> counts(fn.valueCount(), 0);
        auto bump = [&](ValueId v) {
            if (v.isValid())
                ++counts[v.value()];
        };

        for (std::size_t bi = 0; bi < fn.blockCount(); ++bi) {
            BlockId bid(static_cast<BlockId::Value>(bi));
            const BasicBlock &block = fn.block(bid);
            for (InstId iid : block.insts()) {
                const Instruction &inst = fn.inst(iid);
                for (ValueId operand : inst.operands) bump(operand);
            }
            const Terminator &t = block.term();
            if (t.kind == TermKind::CondBr || t.kind == TermKind::Switch)
                bump(t.cond);
            if (t.kind == TermKind::Ret)
                bump(t.retValue);
            for (const BlockRef &ref : t.targets)
                for (ValueId arg : ref.args) bump(arg);
        }
        return counts;
    }

}  // namespace zust::zir
