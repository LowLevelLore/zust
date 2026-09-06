#include "zir/passes/Inline.hpp"

#include <unordered_map>

namespace zust::zir {

    namespace {
        bool callsBack(Module &m, Function &callee, Function &caller) {
            for (InstId iid : callee.block(callee.entry()).insts())
                if (callee.inst(iid).op == Opcode::Call && &m.function(callee.inst(iid).callee) == &caller)
                    return true;
            return false;
        }

        // A callee's own alloca would land wherever the call site happens
        // to be, not necessarily the caller's entry block -- disallowed
        // (docs/IR-DESIGN.md check 6). By this point in the pipeline
        // (Wave 4.5's -O3 runs after -O1's mem2reg has had every chance to
        // promote it away already), this should be rare rather than a real
        // loss of inlining opportunity.
        bool hasAlloca(Function &fn) {
            for (InstId iid : fn.block(fn.entry()).insts())
                if (fn.inst(iid).op == Opcode::Alloca)
                    return true;
            return false;
        }

        ValueId substitute(ValueId v, const std::unordered_map<ValueId::Value, ValueId> &map) {
            if (!v.isValid())
                return v;
            auto it = map.find(v.value());
            return it != map.end() ? it->second : v;
        }

        // Tries to inline exactly one call site somewhere in `caller`.
        // Returns true (having mutated `caller`) on success.
        bool inlineOneCallSite(Module &m, Function &caller) {
            for (std::size_t bi = 0; bi < caller.blockCount(); ++bi) {
                BlockId bid(static_cast<BlockId::Value>(bi));
                std::vector<InstId> &insts = caller.block(bid).insts();
                for (std::size_t pos = 0; pos < insts.size(); ++pos) {
                    Instruction &call = caller.inst(insts[pos]);
                    if (call.op != Opcode::Call)
                        continue;
                    Function &callee = m.function(call.callee);
                    if (&callee == &caller || callee.isExtern() || callee.isVariadic())
                        continue;
                    if (callee.blockCount() != 1)
                        continue;
                    if (hasAlloca(callee))
                        continue;
                    if (callsBack(m, callee, caller))
                        continue;

                    // Seed the value map with callee params -> this call's
                    // actual arguments, then copy every callee instruction
                    // but its terminator, giving each a fresh ValueId in
                    // `caller` and substituting operands through the same
                    // map as we go (a callee instruction can only use a
                    // value the callee itself already defined earlier, by
                    // the same SSA-dominance property every ZIR function
                    // has, so a single forward pass resolves every operand
                    // correctly).
                    std::unordered_map<ValueId::Value, ValueId> valueMap;
                    const std::vector<ValueId> &calleeParams = callee.block(callee.entry()).params();
                    for (std::size_t i = 0; i < calleeParams.size() && i < call.operands.size(); ++i)
                        valueMap[calleeParams[i].value()] = call.operands[i];

                    std::vector<InstId> copied;
                    for (InstId cid : callee.block(callee.entry()).insts()) {
                        const Instruction &src = callee.inst(cid);
                        Instruction dst = src;
                        for (ValueId &operand : dst.operands)
                            operand = substitute(operand, valueMap);
                        if (dst.result.isValid()) {
                            ValueId fresh = caller.newValue(dst.type);
                            valueMap[dst.result.value()] = fresh;
                            dst.result = fresh;
                        }
                        InstId newId = caller.addInst(bid, std::move(dst));
                        copied.push_back(newId);
                        // addInst appended it to the *end* of this block's
                        // list; it belongs where the call was instead, so
                        // pull it back out here and place the whole run in
                        // order below.
                        insts.pop_back();
                    }

                    const Terminator &calleeTerm = callee.block(callee.entry()).term();
                    ValueId replacement;
                    if (calleeTerm.kind == TermKind::Ret && calleeTerm.retValue.isValid())
                        replacement = substitute(calleeTerm.retValue, valueMap);

                    // Splice the copied body in where the call was, then
                    // drop the call itself. If the call's own result was
                    // used, every use of it elsewhere in `caller` now needs
                    // to see `replacement` instead -- a plain function-wide
                    // substitution, same pattern every other pass here uses.
                    ValueId oldResult = call.result;
                    insts.erase(insts.begin() + static_cast<long>(pos));
                    insts.insert(insts.begin() + static_cast<long>(pos), copied.begin(), copied.end());

                    if (oldResult.isValid() && replacement.isValid()) {
                        for (std::size_t bi2 = 0; bi2 < caller.blockCount(); ++bi2) {
                            BasicBlock &block2 = caller.block(BlockId(static_cast<BlockId::Value>(bi2)));
                            for (InstId iid2 : block2.insts()) {
                                Instruction &inst2 = caller.inst(iid2);
                                for (ValueId &operand : inst2.operands)
                                    if (operand == oldResult)
                                        operand = replacement;
                            }
                            Terminator &t2 = block2.term();
                            if (t2.cond == oldResult)
                                t2.cond = replacement;
                            if (t2.retValue == oldResult)
                                t2.retValue = replacement;
                            for (BlockRef &ref : t2.targets)
                                for (ValueId &arg : ref.args)
                                    if (arg == oldResult)
                                        arg = replacement;
                        }
                    }
                    return true;
                }
            }
            return false;
        }
    }  // namespace

    bool InlinePass::run(Module &m, AnalysisManager &) {
        bool changedAny = false;
        for (std::size_t i = 0; i < m.functions().size(); ++i) {
            Function &fn = m.function(FuncId(static_cast<FuncId::Value>(i)));
            if (fn.isExtern())
                continue;
            while (inlineOneCallSite(m, fn))
                changedAny = true;
        }
        return changedAny;
    }

}  // namespace zust::zir
