#include "zir/passes/SCCP.hpp"

#include <unordered_map>
#include <unordered_set>

namespace zust::zir {

    namespace {
        void collectConsts(const Function &fn, std::unordered_map<ValueId::Value, std::uint64_t> &bits) {
            for (std::size_t bi = 0; bi < fn.blockCount(); ++bi) {
                for (InstId iid : fn.block(BlockId(static_cast<BlockId::Value>(bi))).insts()) {
                    const Instruction &inst = fn.inst(iid);
                    if (inst.op == Opcode::Const && inst.result.isValid())
                        bits[inst.result.value()] = inst.constant.bits;
                }
            }
        }
    }  // namespace

    bool SCCPPass::run(Function &fn, AnalysisManager &) {
        if (!fn.entry().isValid())
            return false;

        std::unordered_map<ValueId::Value, std::uint64_t> constBits;
        collectConsts(fn, constBits);

        // Every param's incoming values, gathered by scanning every
        // terminator once: block -> paramIndex -> list of incoming values.
        std::unordered_map<BlockId::Value, std::vector<std::vector<ValueId>>> incoming;
        for (std::size_t bi = 0; bi < fn.blockCount(); ++bi) {
            for (const BlockRef &ref : fn.block(BlockId(static_cast<BlockId::Value>(bi))).term().targets) {
                auto &slots = incoming[ref.block.value()];
                slots.resize(ref.args.size());
                for (std::size_t i = 0; i < ref.args.size(); ++i) slots[i].push_back(ref.args[i]);
            }
        }

        std::unordered_map<ValueId::Value, ValueId> subst;
        std::vector<std::pair<BlockId, Instruction>> toPrepend;

        for (std::size_t bi = 0; bi < fn.blockCount(); ++bi) {
            BlockId b(static_cast<BlockId::Value>(bi));
            if (b == fn.entry())
                continue;  // entry's params are real function arguments, never a merge
            const std::vector<ValueId> &params = fn.block(b).params();
            if (params.empty())
                continue;
            auto it = incoming.find(b.value());
            if (it == incoming.end())
                continue;

            for (std::size_t i = 0; i < params.size() && i < it->second.size(); ++i) {
                const std::vector<ValueId> &values = it->second[i];
                if (values.empty())
                    continue;
                auto first = constBits.find(values[0].value());
                if (first == constBits.end())
                    continue;
                bool allSame = true;
                for (ValueId v : values) {
                    auto cit = constBits.find(v.value());
                    if (cit == constBits.end() || cit->second != first->second) {
                        allSame = false;
                        break;
                    }
                }
                if (!allSame)
                    continue;

                ValueId newConst = fn.newValue(fn.typeOf(params[i]));
                Instruction inst;
                inst.op = Opcode::Const;
                inst.type = fn.typeOf(params[i]);
                inst.result = newConst;
                inst.constant.bits = first->second;
                toPrepend.push_back({b, std::move(inst)});
                subst[params[i].value()] = newConst;
            }
        }

        if (subst.empty())
            return false;

        for (auto &[block, inst] : toPrepend) {
            InstId id = fn.addInst(block, std::move(inst));
            std::vector<InstId> &insts = fn.block(block).insts();
            // addInst appends; move it to the front so it textually precedes
            // whatever else in this block will use it.
            insts.insert(insts.begin(), insts.back());
            insts.pop_back();
            (void)id;
        }

        auto doSubst = [&](ValueId &v) {
            if (!v.isValid())
                return;
            auto it = subst.find(v.value());
            if (it != subst.end())
                v = it->second;
        };
        for (std::size_t bi = 0; bi < fn.blockCount(); ++bi) {
            BasicBlock &block = fn.block(BlockId(static_cast<BlockId::Value>(bi)));
            for (InstId iid : block.insts()) {
                Instruction &inst = fn.inst(iid);
                if (inst.op == Opcode::Const)
                    continue;  // never rewrite the very consts we just introduced
                for (ValueId &operand : inst.operands) doSubst(operand);
            }
            Terminator &t = block.term();
            doSubst(t.cond);
            doSubst(t.retValue);
            for (BlockRef &ref : t.targets)
                for (ValueId &arg : ref.args) doSubst(arg);
        }
        return true;
    }

}  // namespace zust::zir
