#include "zir/passes/TailCall.hpp"

namespace zust::zir {

    namespace {
        FuncId findSelf(Module &m, const Function &fn) {
            for (std::size_t i = 0; i < m.functions().size(); ++i) {
                FuncId id(static_cast<FuncId::Value>(i));
                if (&m.function(id) == &fn)
                    return id;
            }
            return FuncId{};
        }

        bool isSelfTailCall(Function &fn, FuncId selfId, BlockId b, InstId &callOut) {
            BasicBlock &block = fn.block(b);
            const Terminator &t = block.term();
            if (t.kind != TermKind::Ret || block.insts().empty())
                return false;
            InstId lastId = block.insts().back();
            const Instruction &last = fn.inst(lastId);
            if (last.op != Opcode::Call || last.callee != selfId)
                return false;
            bool resultIsReturned = (t.retValue.isValid() && last.result.isValid() && t.retValue == last.result) ||
                                    (!t.retValue.isValid() && !last.result.isValid());
            if (!resultIsReturned)
                return false;
            callOut = lastId;
            return true;
        }
    }  // namespace

    bool TailCallPass::run(Function &fn, AnalysisManager &) {
        if (!fn.entry().isValid())
            return false;
        for (InstId iid : fn.block(fn.entry()).insts())
            if (fn.inst(iid).op == Opcode::Alloca)
                return false;  // re-entering entry would re-run one-time frame setup

        FuncId selfId = findSelf(module_, fn);
        if (!selfId.isValid())
            return false;

        std::vector<std::pair<BlockId, InstId>> candidates;
        for (std::size_t bi = 0; bi < fn.blockCount(); ++bi) {
            BlockId b(static_cast<BlockId::Value>(bi));
            InstId call;
            if (isSelfTailCall(fn, selfId, b, call))
                candidates.push_back({b, call});
        }
        if (candidates.empty())
            return false;

        // LLVM (unlike ZIR itself) hard-requires a function's entry block to
        // have no predecessors at all, so a tail call cannot simply branch
        // back to `fn.entry()` directly. Split it once instead: everything
        // entry used to hold moves to a brand new block (the real loop
        // header from here on, keeping every existing ValueId --
        // instructions elsewhere that reference entry's old params or
        // content need no rewriting at all, since it's the same content
        // under a different BlockId), and entry becomes a trivial one-time
        // forwarder with fresh parameters of its own. Every tail call
        // targets the new header, which -- unlike entry -- is allowed to
        // have as many predecessors as it likes.
        BlockId oldEntry = fn.entry();
        BlockId header = fn.addBlock(fn.block(oldEntry).label() + ".tailrec");

        std::vector<ValueId> oldParams = fn.block(oldEntry).params();
        fn.block(header).params() = oldParams;
        fn.block(header).insts() = std::move(fn.block(oldEntry).insts());
        fn.block(header).term() = fn.block(oldEntry).term();

        fn.block(oldEntry).insts().clear();
        fn.block(oldEntry).params().clear();
        std::vector<ValueId> forwardArgs;
        for (ValueId p : oldParams) {
            ValueId fresh = fn.newValue(fn.typeOf(p));
            fn.block(oldEntry).params().push_back(fresh);
            forwardArgs.push_back(fresh);
        }
        BlockRef forwardRef;
        forwardRef.block = header;
        forwardRef.args = forwardArgs;
        Terminator forwardTerm;
        forwardTerm.kind = TermKind::Br;
        forwardTerm.targets = {std::move(forwardRef)};
        fn.block(oldEntry).term() = std::move(forwardTerm);

        for (auto &[b, callId] : candidates) {
            // A candidate found directly in entry now physically lives in
            // `header` -- its content, including this very call, moved
            // there above.
            if (b == oldEntry)
                b = header;
            BasicBlock &block = fn.block(b);
            Instruction &call = fn.inst(callId);
            BlockRef ref;
            ref.block = header;
            ref.args = call.operands;
            Terminator t;
            t.kind = TermKind::Br;
            t.targets = {std::move(ref)};
            block.term() = std::move(t);
            block.insts().pop_back();  // the call itself is gone -- no frame is ever pushed for it now
        }
        return true;
    }

}  // namespace zust::zir
