#include "zir/passes/LICM.hpp"

#include <unordered_set>

#include "zir/DominatorTree.hpp"

namespace zust::zir {

    namespace {
        bool isPure(Opcode op) {
            switch (op) {
                case Opcode::Load:
                case Opcode::Store:
                case Opcode::Alloca:
                case Opcode::Call:
                case Opcode::Select:
                case Opcode::Gep:
                    return false;
                default:
                    return true;
            }
        }

        std::unordered_set<BlockId::Value> naturalLoopBody(const DominatorTree &dt, BlockId header, BlockId latch) {
            std::unordered_set<BlockId::Value> body = {header.value(), latch.value()};
            std::vector<BlockId> worklist;
            if (latch != header)
                worklist.push_back(latch);
            while (!worklist.empty()) {
                BlockId b = worklist.back();
                worklist.pop_back();
                for (BlockId p : dt.predecessors(b)) {
                    if (body.insert(p.value()).second)
                        worklist.push_back(p);
                }
            }
            return body;
        }

        // Tries one loop; returns true (having hoisted something) on the
        // first successful hoist.
        bool tryHoistFromLoop(Function &fn, const DominatorTree &dt, BlockId header,
                              const std::unordered_set<BlockId::Value> &body) {
            std::vector<BlockId> outsidePreds;
            for (BlockId p : dt.predecessors(header))
                if (body.count(p.value()) == 0)
                    outsidePreds.push_back(p);
            if (outsidePreds.size() != 1)
                return false;
            BlockId preheader = outsidePreds[0];
            Terminator &preheaderTerm = fn.block(preheader).term();
            if (preheaderTerm.kind != TermKind::Br || preheaderTerm.targets[0].block != header)
                return false;  // not a plain fall-through into the header -- leave it alone

            std::unordered_map<ValueId::Value, BlockId> defBlock;
            for (BlockId::Value bv : body) {
                BlockId b(bv);
                for (ValueId p : fn.block(b).params())
                    defBlock[p.value()] = b;
                for (InstId iid : fn.block(b).insts())
                    if (fn.inst(iid).result.isValid())
                        defBlock[fn.inst(iid).result.value()] = b;
            }

            auto definedOutsideLoop = [&](ValueId v) {
                auto it = defBlock.find(v.value());
                return it == defBlock.end();  // not defined anywhere in the loop body -> must be from outside it
            };

            for (BlockId::Value bv : body) {
                BlockId b(bv);
                std::vector<InstId> &insts = fn.block(b).insts();
                for (std::size_t i = 0; i < insts.size(); ++i) {
                    Instruction &inst = fn.inst(insts[i]);
                    if (!isPure(inst.op) || !inst.result.isValid())
                        continue;
                    bool allOutside = true;
                    for (ValueId operand : inst.operands) {
                        if (!definedOutsideLoop(operand)) {
                            allOutside = false;
                            break;
                        }
                    }
                    if (!allOutside)
                        continue;

                    InstId id = insts[i];
                    insts.erase(insts.begin() + static_cast<long>(i));
                    fn.block(preheader).insts().push_back(id);
                    return true;
                }
            }
            return false;
        }
    }  // namespace

    bool LICMPass::run(Function &fn, AnalysisManager &am) {
        if (!fn.entry().isValid())
            return false;
        const DominatorTree &dt = am.dominatorTree(fn);

        for (std::size_t bi = 0; bi < fn.blockCount(); ++bi) {
            BlockId latch(static_cast<BlockId::Value>(bi));
            if (!dt.isReachable(latch))
                continue;
            for (const BlockRef &ref : fn.block(latch).term().targets) {
                BlockId header = ref.block;
                if (!dt.dominates(header, latch))
                    continue;  // not a back edge
                auto body = naturalLoopBody(dt, header, latch);
                if (tryHoistFromLoop(fn, dt, header, body))
                    return true;
            }
        }
        return false;
    }

}  // namespace zust::zir
