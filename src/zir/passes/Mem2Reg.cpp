#include "zir/passes/Mem2Reg.hpp"

#include <functional>
#include <unordered_map>
#include <unordered_set>

#include "zir/DominatorTree.hpp"

namespace zust::zir {

    namespace {
        using Key = ValueId::Value;

        struct Candidate {
            ValueId alloca;
            TypeId elemType;
        };

        std::vector<Candidate> collectEntryAllocas(Function &fn) {
            std::vector<Candidate> out;
            for (InstId iid : fn.block(fn.entry()).insts()) {
                const Instruction &inst = fn.inst(iid);
                if (inst.op == Opcode::Alloca)
                    out.push_back({inst.result, inst.elemType});
            }
            return out;
        }

        // Shape check only (an "address never escapes" check, in spirit):
        // every use of the alloca's own pointer value must be a Load's
        // pointer operand or a Store's *destination* operand -- never a
        // Store's value, never anything else. ZirGen never produces a
        // failing case (a local's address is only ever used for its own
        // load/store), so this exists mainly to keep the pass honest
        // against any future producer.
        bool isPromotableShape(const Function &fn, ValueId allocaId) {
            for (std::size_t bi = 0; bi < fn.blockCount(); ++bi) {
                BlockId b(static_cast<BlockId::Value>(bi));
                for (InstId iid : fn.block(b).insts()) {
                    const Instruction &inst = fn.inst(iid);
                    for (std::size_t oi = 0; oi < inst.operands.size(); ++oi) {
                        if (inst.operands[oi] != allocaId)
                            continue;
                        bool ok = (inst.op == Opcode::Load && oi == 0) || (inst.op == Opcode::Store && oi == 1);
                        if (!ok)
                            return false;
                    }
                }
                const Terminator &t = fn.block(b).term();
                if (t.cond == allocaId || t.retValue == allocaId)
                    return false;
                for (const BlockRef &ref : t.targets)
                    for (ValueId arg : ref.args)
                        if (arg == allocaId)
                            return false;
            }
            return true;
        }

        std::vector<BlockId> defBlocksOf(const Function &fn, ValueId allocaId) {
            std::vector<BlockId> defs;
            for (std::size_t bi = 0; bi < fn.blockCount(); ++bi) {
                BlockId b(static_cast<BlockId::Value>(bi));
                for (InstId iid : fn.block(b).insts()) {
                    const Instruction &inst = fn.inst(iid);
                    if (inst.op == Opcode::Store && inst.operands.size() == 2 && inst.operands[1] == allocaId) {
                        defs.push_back(b);
                        break;
                    }
                }
            }
            return defs;
        }

        // Iterated dominance frontier of a value's defining blocks -- the
        // set of blocks that need a merge (a new block parameter) for it.
        std::vector<BlockId> iteratedDominanceFrontier(const DominatorTree &dt, const std::vector<BlockId> &defs) {
            std::unordered_set<BlockId::Value> inSet;
            std::vector<BlockId> worklist = defs;
            std::vector<BlockId> result;
            while (!worklist.empty()) {
                BlockId d = worklist.back();
                worklist.pop_back();
                for (BlockId f : dt.dominanceFrontier(d)) {
                    if (inSet.insert(f.value()).second) {
                        result.push_back(f);
                        worklist.push_back(f);
                    }
                }
            }
            return result;
        }

        std::vector<std::vector<BlockId>> domTreeChildren(const Function &fn, const DominatorTree &dt) {
            std::vector<std::vector<BlockId>> children(fn.blockCount());
            for (std::size_t bi = 0; bi < fn.blockCount(); ++bi) {
                BlockId b(static_cast<BlockId::Value>(bi));
                if (!dt.isReachable(b) || b == fn.entry())
                    continue;
                children[dt.immediateDominator(b).value()].push_back(b);
            }
            return children;
        }

        // Does every load of `target` have a reaching store along every
        // dominator-tree path from entry? A block in `frontierBlocks`
        // counts as a (merge) definition point too, exactly as it will once
        // phase 2 actually inserts the parameter there.
        // Two distinct things have to hold for every path from entry, not
        // just the first: every *load* must see a reaching store (an
        // ordinary "used before defined" concern), AND every edge into a
        // block in this alloca's own frontier (a future merge point) must
        // come from a predecessor that already has a value to contribute --
        // otherwise the merge would need a value for an edge that has none,
        // which is exactly the "would need undef" case this pass declines
        // to invent (see the class-level comment). Missing the second check
        // and only verifying loads is not enough: a predecessor can have no
        // load of its own yet still be one of the edges a phi needs a
        // contribution from.
        bool hasReachingDefEverywhere(Function &fn, ValueId target, const std::vector<std::vector<BlockId>> &children,
                                     const std::unordered_set<BlockId::Value> &frontierBlocks) {
            bool safe = true;
            std::function<void(BlockId, bool)> visit = [&](BlockId b, bool definedIn) {
                bool defined = definedIn || frontierBlocks.count(b.value()) != 0;
                for (InstId iid : fn.block(b).insts()) {
                    const Instruction &inst = fn.inst(iid);
                    if (inst.op == Opcode::Store && inst.operands.size() == 2 && inst.operands[1] == target) {
                        defined = true;
                    } else if (inst.op == Opcode::Load && inst.operands.size() == 1 && inst.operands[0] == target) {
                        if (!defined)
                            safe = false;
                    }
                }
                if (!defined) {
                    for (const BlockRef &ref : fn.block(b).term().targets) {
                        if (frontierBlocks.count(ref.block.value()))
                            safe = false;
                    }
                }
                for (BlockId ch : children[b.value()]) visit(ch, defined);
            };
            visit(fn.entry(), false);
            return safe;
        }

    }  // namespace

    bool Mem2RegPass::run(Function &fn, AnalysisManager &) {
        if (!fn.entry().isValid())
            return false;

        std::vector<Candidate> shapeOk;
        for (const Candidate &c : collectEntryAllocas(fn))
            if (isPromotableShape(fn, c.alloca))
                shapeOk.push_back(c);
        if (shapeOk.empty())
            return false;

        DominatorTree dt(fn);
        auto children = domTreeChildren(fn, dt);

        std::unordered_map<Key, std::vector<BlockId>> frontierOf;
        for (const Candidate &c : shapeOk)
            frontierOf[c.alloca.value()] = iteratedDominanceFrontier(dt, defBlocksOf(fn, c.alloca));

        std::vector<Candidate> safe;
        for (const Candidate &c : shapeOk) {
            std::unordered_set<BlockId::Value> frontierBlocks;
            for (BlockId b : frontierOf[c.alloca.value()]) frontierBlocks.insert(b.value());
            if (hasReachingDefEverywhere(fn, c.alloca, children, frontierBlocks))
                safe.push_back(c);
        }
        if (safe.empty())
            return false;

        // block -> (alloca -> new param ValueId), for every merge point any
        // safe alloca actually needs.
        std::unordered_map<BlockId::Value, std::unordered_map<Key, ValueId>> paramFor;
        for (const Candidate &c : safe) {
            TypeId elemType = c.elemType;
            for (BlockId b : frontierOf[c.alloca.value()]) {
                ValueId param = fn.newValue(elemType);
                fn.block(b).params().push_back(param);
                paramFor[b.value()][c.alloca.value()] = param;
            }
        }

        std::unordered_set<Key> safeSet;
        for (const Candidate &c : safe) safeSet.insert(c.alloca.value());

        std::unordered_map<Key, ValueId> globalSubst;  // a removed load's result -> its reaching value
        std::unordered_set<InstId::Value> toRemove;    // dead alloca/store/load instructions

        std::function<void(BlockId, std::unordered_map<Key, ValueId>)> rename =
            [&](BlockId b, std::unordered_map<Key, ValueId> state) {
                auto pit = paramFor.find(b.value());
                if (pit != paramFor.end())
                    for (auto &entry : pit->second) state[entry.first] = entry.second;

                for (InstId iid : fn.block(b).insts()) {
                    Instruction &inst = fn.inst(iid);
                    if (inst.op == Opcode::Alloca && safeSet.count(inst.result.value())) {
                        toRemove.insert(iid.value());
                    } else if (inst.op == Opcode::Store && inst.operands.size() == 2 &&
                              safeSet.count(inst.operands[1].value())) {
                        ValueId value = inst.operands[0];
                        auto sit = globalSubst.find(value.value());
                        if (sit != globalSubst.end())
                            value = sit->second;
                        state[inst.operands[1].value()] = value;
                        toRemove.insert(iid.value());
                    } else if (inst.op == Opcode::Load && inst.operands.size() == 1 &&
                              safeSet.count(inst.operands[0].value())) {
                        globalSubst[inst.result.value()] = state.at(inst.operands[0].value());
                        toRemove.insert(iid.value());
                    }
                }

                Terminator &t = fn.block(b).term();
                for (BlockRef &ref : t.targets) {
                    auto ppit = paramFor.find(ref.block.value());
                    if (ppit == paramFor.end())
                        continue;
                    for (const Candidate &c : safe) {
                        auto it = ppit->second.find(c.alloca.value());
                        if (it == ppit->second.end())
                            continue;
                        ref.args.push_back(state.at(c.alloca.value()));
                    }
                }

                for (BlockId ch : children[b.value()]) rename(ch, state);
            };
        rename(fn.entry(), {});

        // Apply globalSubst to everything that survives, then drop the
        // instructions phase 2 marked dead.
        for (std::size_t bi = 0; bi < fn.blockCount(); ++bi) {
            BasicBlock &block = fn.block(BlockId(static_cast<BlockId::Value>(bi)));
            std::vector<InstId> kept;
            kept.reserve(block.insts().size());
            for (InstId iid : block.insts()) {
                if (toRemove.count(iid.value()))
                    continue;
                Instruction &inst = fn.inst(iid);
                for (ValueId &operand : inst.operands) {
                    auto it = globalSubst.find(operand.value());
                    if (it != globalSubst.end())
                        operand = it->second;
                }
                kept.push_back(iid);
            }
            block.insts() = std::move(kept);

            Terminator &t = block.term();
            auto substIn = [&](ValueId &v) {
                if (!v.isValid())
                    return;
                auto it = globalSubst.find(v.value());
                if (it != globalSubst.end())
                    v = it->second;
            };
            substIn(t.cond);
            substIn(t.retValue);
            for (BlockRef &ref : t.targets)
                for (ValueId &arg : ref.args) substIn(arg);
        }

        return true;
    }

}  // namespace zust::zir
