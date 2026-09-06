#include "zir/passes/GVN.hpp"

#include <functional>
#include <sstream>
#include <unordered_map>

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

        // A cheap, unambiguous textual key -- these functions are small
        // enough that string-building overhead is irrelevant next to
        // actually running a pass. `constant.bits` matters for exactly one
        // opcode (Const) but is always included: leaving it out would make
        // every `const i64 10` and `const i64 50` collide on the same key.
        std::string signatureOf(const Instruction &inst) {
            std::ostringstream ss;
            ss << static_cast<int>(inst.op) << ':' << static_cast<int>(inst.pred) << ':' << inst.type.value() << ':'
               << inst.elemType.value() << ':' << inst.global.value() << ':' << inst.callee.value() << ':'
               << inst.constant.bits;
            for (ValueId v : inst.operands)
                ss << ':' << v.value();
            return ss.str();
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
    }  // namespace

    bool GVNPass::run(Function &fn, AnalysisManager &) {
        if (!fn.entry().isValid())
            return false;

        DominatorTree dt(fn);
        auto children = domTreeChildren(fn, dt);
        std::unordered_map<ValueId::Value, ValueId> globalSubst;

        std::function<void(BlockId, std::unordered_map<std::string, ValueId>)> walk =
            [&](BlockId b, std::unordered_map<std::string, ValueId> available) {
                for (InstId iid : fn.block(b).insts()) {
                    Instruction &inst = fn.inst(iid);
                    if (!inst.result.isValid() || !isPure(inst.op))
                        continue;
                    std::string sig = signatureOf(inst);
                    auto it = available.find(sig);
                    if (it != available.end()) {
                        globalSubst[inst.result.value()] = it->second;
                    } else {
                        available[sig] = inst.result;
                    }
                }
                for (BlockId c : children[b.value()])
                    walk(c, available);
            };
        walk(fn.entry(), {});

        if (globalSubst.empty())
            return false;

        // Resolve chains (an instruction whose own operand was itself just
        // rewritten) before applying, so every use ends up pointing at a
        // value that is not itself scheduled for replacement.
        for (auto &entry : globalSubst) {
            ValueId v = entry.second;
            for (int guard = 0; guard < 64; ++guard) {
                auto it = globalSubst.find(v.value());
                if (it == globalSubst.end())
                    break;
                v = it->second;
            }
            entry.second = v;
        }

        auto subst = [&](ValueId &v) {
            if (!v.isValid())
                return;
            auto it = globalSubst.find(v.value());
            if (it != globalSubst.end())
                v = it->second;
        };
        for (std::size_t bi = 0; bi < fn.blockCount(); ++bi) {
            BasicBlock &block = fn.block(BlockId(static_cast<BlockId::Value>(bi)));
            for (InstId iid : block.insts()) {
                Instruction &inst = fn.inst(iid);
                for (ValueId &operand : inst.operands)
                    subst(operand);
            }
            Terminator &t = block.term();
            subst(t.cond);
            subst(t.retValue);
            for (BlockRef &ref : t.targets)
                for (ValueId &arg : ref.args)
                    subst(arg);
        }
        return true;
    }

}  // namespace zust::zir
