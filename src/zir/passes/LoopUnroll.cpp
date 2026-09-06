#include "zir/passes/LoopUnroll.hpp"

#include <optional>
#include <unordered_map>

#include "zir/DominatorTree.hpp"

namespace zust::zir {

    namespace {
        constexpr std::uint64_t kMaxUnrollIterations = 64;

        std::int64_t signExtendFrom(std::uint32_t bits, std::uint64_t v) {
            if (bits >= 64)
                return static_cast<std::int64_t>(v);
            std::uint64_t signBit = std::uint64_t{1} << (bits - 1);
            std::uint64_t mask = (std::uint64_t{1} << bits) - 1;
            std::uint64_t m = v & mask;
            return static_cast<std::int64_t>((m ^ signBit) - signBit);
        }

        std::uint64_t maskTo(std::uint32_t bits, std::uint64_t v) {
            return bits >= 64 ? v : (v & ((std::uint64_t{1} << bits) - 1));
        }

        bool evalIcmp(CmpPred pred, std::uint32_t bits, std::uint64_t a, std::uint64_t b) {
            switch (pred) {
                case CmpPred::Eq:
                    return maskTo(bits, a) == maskTo(bits, b);
                case CmpPred::Ne:
                    return maskTo(bits, a) != maskTo(bits, b);
                case CmpPred::Slt:
                    return signExtendFrom(bits, a) < signExtendFrom(bits, b);
                case CmpPred::Sle:
                    return signExtendFrom(bits, a) <= signExtendFrom(bits, b);
                case CmpPred::Sgt:
                    return signExtendFrom(bits, a) > signExtendFrom(bits, b);
                case CmpPred::Sge:
                    return signExtendFrom(bits, a) >= signExtendFrom(bits, b);
                case CmpPred::Ult:
                    return maskTo(bits, a) < maskTo(bits, b);
                case CmpPred::Ule:
                    return maskTo(bits, a) <= maskTo(bits, b);
                case CmpPred::Ugt:
                    return maskTo(bits, a) > maskTo(bits, b);
                case CmpPred::Uge:
                    return maskTo(bits, a) >= maskTo(bits, b);
                default:
                    return false;
            }
        }

        std::uint64_t applyStep(Opcode op, std::uint32_t bits, std::uint64_t v, std::uint64_t step) {
            return op == Opcode::Add ? maskTo(bits, v + step) : maskTo(bits, v - step);
        }

        // Everything this pass needs to know about one header/latch loop,
        // resolved once up front; recognize() returns nullopt the moment
        // any expectation isn't exactly met.
        struct LoopShape {
            std::size_t counterIndex = 0;
            ValueId counterParam;
            std::uint32_t bits = 0;
            CmpPred pred = CmpPred::None;
            std::uint64_t bound = 0;
            BlockId exitTarget;
            Opcode stepOp = Opcode::Add;
            std::uint64_t step = 0;
            BlockId preheader;
            std::uint64_t initial = 0;
        };

        std::optional<std::uint64_t> constValue(Function &fn, ValueId v) {
            for (std::size_t bi = 0; bi < fn.blockCount(); ++bi) {
                for (InstId iid : fn.block(BlockId(static_cast<BlockId::Value>(bi))).insts()) {
                    const Instruction &inst = fn.inst(iid);
                    if (inst.result == v && inst.op == Opcode::Const)
                        return inst.constant.bits;
                }
            }
            return std::nullopt;
        }

        std::optional<LoopShape> recognize(Module &m, Function &fn, const DominatorTree &dt, BlockId header,
                                           BlockId latch) {
            if (header == latch)
                return std::nullopt;
            // Exactly one loop-carried value: the counter itself. A second
            // header parameter would be some other value threaded around
            // the loop (an accumulator read inside the body, say) whose
            // per-iteration value this pass does not track -- cloning the
            // body would then leave a reference to the header's own
            // parameter, and header stops dominating anything once the
            // loop is unrolled away. Handling more than the counter would
            // need every loop-carried value threaded through the clone
            // chain the same way the counter already is; not attempted
            // here.
            if (fn.block(header).params().size() != 1)
                return std::nullopt;
            Terminator &headerTerm = fn.block(header).term();
            if (headerTerm.kind != TermKind::CondBr)
                return std::nullopt;
            if (headerTerm.targets[0].block != latch && headerTerm.targets[1].block != latch)
                return std::nullopt;

            LoopShape shape;
            bool found = false;
            const std::vector<ValueId> &headerParams = fn.block(header).params();
            for (InstId iid : fn.block(header).insts()) {
                const Instruction &inst = fn.inst(iid);
                if (inst.op != Opcode::ICmp || inst.result != headerTerm.cond || inst.operands.size() != 2)
                    continue;
                for (std::size_t pi = 0; pi < headerParams.size() && !found; ++pi) {
                    if (inst.operands[0] != headerParams[pi])
                        continue;
                    auto bound = constValue(fn, inst.operands[1]);
                    if (!bound)
                        continue;
                    shape.counterIndex = pi;
                    shape.counterParam = headerParams[pi];
                    shape.bits = m.types().get(fn.typeOf(shape.counterParam)).bits;
                    shape.pred = inst.pred;
                    shape.bound = *bound;
                    found = true;
                }
                if (found)
                    break;
            }
            if (!found)
                return std::nullopt;

            shape.exitTarget =
                headerTerm.targets[0].block == latch ? headerTerm.targets[1].block : headerTerm.targets[0].block;

            Terminator &latchTerm = fn.block(latch).term();
            if (latchTerm.kind != TermKind::Br || latchTerm.targets[0].block != header)
                return std::nullopt;
            if (shape.counterIndex >= latchTerm.targets[0].args.size())
                return std::nullopt;
            ValueId nextVal = latchTerm.targets[0].args[shape.counterIndex];
            bool foundStep = false;
            for (InstId iid : fn.block(latch).insts()) {
                const Instruction &inst = fn.inst(iid);
                if (inst.result != nextVal)
                    continue;
                if (inst.operands.size() != 2 || (inst.op != Opcode::Add && inst.op != Opcode::Sub) ||
                    inst.operands[0] != shape.counterParam)
                    return std::nullopt;
                auto step = constValue(fn, inst.operands[1]);
                if (!step)
                    return std::nullopt;
                shape.stepOp = inst.op;
                shape.step = *step;
                foundStep = true;
                break;
            }
            if (!foundStep)
                return std::nullopt;

            std::vector<BlockId> outside;
            for (BlockId p : dt.predecessors(header))
                if (p != latch)
                    outside.push_back(p);
            if (outside.size() != 1)
                return std::nullopt;
            shape.preheader = outside[0];
            Terminator &preheaderTerm = fn.block(shape.preheader).term();
            if (preheaderTerm.kind != TermKind::Br || preheaderTerm.targets[0].block != header)
                return std::nullopt;
            if (shape.counterIndex >= preheaderTerm.targets[0].args.size())
                return std::nullopt;
            auto initial = constValue(fn, preheaderTerm.targets[0].args[shape.counterIndex]);
            if (!initial)
                return std::nullopt;
            shape.initial = *initial;
            return shape;
        }

        ValueId substitute(ValueId v, const std::unordered_map<ValueId::Value, ValueId> &map) {
            if (!v.isValid())
                return v;
            auto it = map.find(v.value());
            return it != map.end() ? it->second : v;
        }

        // Attempts to unroll exactly one recognized loop. Returns true
        // (having mutated `fn`) on success.
        bool tryUnroll(Module &m, Function &fn, const DominatorTree &dt, BlockId header, BlockId latch) {
            // The loop body must be exactly {header, latch} -- the shape
            // after -O1's SimplifyCFG has already merged away a separate
            // post-block, the common case by the time this pass runs in
            // the same fixpoint.
            for (BlockId p : dt.predecessors(latch))
                if (p != header && dt.dominates(header, p))
                    return false;

            auto shapeOpt = recognize(m, fn, dt, header, latch);
            if (!shapeOpt)
                return false;
            const LoopShape &shape = *shapeOpt;
            if (shape.bits == 0)
                return false;

            // Simulate the loop at compile time to get its exact trip
            // count, bailing if it doesn't obviously terminate within the
            // cap (an infinite or merely-too-large loop is left alone).
            std::vector<std::uint64_t> counterValues;
            std::uint64_t counter = shape.initial;
            while (evalIcmp(shape.pred, shape.bits, counter, shape.bound)) {
                counterValues.push_back(counter);
                if (counterValues.size() > kMaxUnrollIterations)
                    return false;
                counter = applyStep(shape.stepOp, shape.bits, counter, shape.step);
            }

            // Build one straight-line chain of blocks, one per iteration,
            // each a copy of latch's own instructions (the loop body's real
            // work) with the counter substituted for that iteration's
            // concrete constant. The header's own condition/branch is never
            // re-executed -- the trip count is already known.
            BlockId cursor = shape.preheader;
            for (std::size_t i = 0; i < counterValues.size(); ++i) {
                BlockId clone = fn.addBlock(fn.block(latch).label() + ".unroll" + std::to_string(i));

                std::unordered_map<ValueId::Value, ValueId> valueMap;
                ValueId counterConst = fn.newValue(fn.typeOf(shape.counterParam));
                Instruction counterInst;
                counterInst.op = Opcode::Const;
                counterInst.type = fn.typeOf(shape.counterParam);
                counterInst.result = counterConst;
                counterInst.constant.bits = counterValues[i];
                fn.addInst(clone, std::move(counterInst));
                valueMap[shape.counterParam.value()] = counterConst;

                for (InstId iid : fn.block(latch).insts()) {
                    const Instruction &src = fn.inst(iid);
                    Instruction dst = src;
                    for (ValueId &operand : dst.operands)
                        operand = substitute(operand, valueMap);
                    if (dst.result.isValid()) {
                        ValueId fresh = fn.newValue(dst.type);
                        valueMap[dst.result.value()] = fresh;
                        dst.result = fresh;
                    }
                    fn.addInst(clone, std::move(dst));
                }

                BlockRef intoClone;
                intoClone.block = clone;
                Terminator t;
                t.kind = TermKind::Br;
                t.targets = {intoClone};
                fn.block(cursor).term() = std::move(t);
                cursor = clone;
            }

            // After the last iteration (or immediately, for zero
            // iterations), branch straight to the exit -- copying whatever
            // arguments the header's own false-edge originally carried
            // verbatim (any of *those* that happen to be the counter get
            // caught by the function-wide substitution below, same as
            // everything else).
            for (const BlockRef &edge : fn.block(header).term().targets) {
                if (edge.block != shape.exitTarget)
                    continue;
                Terminator t;
                t.kind = TermKind::Br;
                t.targets = {edge};
                fn.block(cursor).term() = std::move(t);
                break;
            }

            // The header block (and everything in it, including the
            // counter parameter itself) is unreachable from here on --
            // SimplifyCFG's own dead-block sweep clears it out later in the
            // same fixpoint. But a descendant can read a value a dominating
            // block defined *directly*, with no block argument ever
            // involved (ZIR only needs a block parameter at an actual
            // merge point) -- exactly how `^end: ret i64 %i` reads the
            // header's own loop counter in the canonical `for` shape. Any
            // such direct reference has to be replaced with the concrete
            // exit-time value everywhere it might still occur, not just
            // along the one exit edge's argument list.
            std::uint64_t exitCounter = counterValues.empty()
                                            ? shape.initial
                                            : applyStep(shape.stepOp, shape.bits, counterValues.back(), shape.step);
            ValueId exitConst = fn.newValue(fn.typeOf(shape.counterParam));
            Instruction exitInst;
            exitInst.op = Opcode::Const;
            exitInst.type = fn.typeOf(shape.counterParam);
            exitInst.result = exitConst;
            exitInst.constant.bits = exitCounter;
            fn.addInst(cursor, std::move(exitInst));

            for (std::size_t bi = 0; bi < fn.blockCount(); ++bi) {
                BasicBlock &block = fn.block(BlockId(static_cast<BlockId::Value>(bi)));
                for (InstId iid : block.insts()) {
                    Instruction &inst = fn.inst(iid);
                    for (ValueId &operand : inst.operands)
                        if (operand == shape.counterParam)
                            operand = exitConst;
                }
                Terminator &t2 = block.term();
                if (t2.cond == shape.counterParam)
                    t2.cond = exitConst;
                if (t2.retValue == shape.counterParam)
                    t2.retValue = exitConst;
                for (BlockRef &ref : t2.targets)
                    for (ValueId &arg : ref.args)
                        if (arg == shape.counterParam)
                            arg = exitConst;
            }

            // header and latch now point only at each other -- neither
            // "unreferenced" on its own, which is exactly the "dangling"
            // shape the Verifier's predecessor check rejects (as opposed to
            // "simply dead"), so this pass has to clear them itself rather
            // than counting on a later SimplifyCFG round in the same
            // fixpoint to do it (docs/PRD-ZIR.md's "verifier clean after
            // every pass" -- true of each pass on its own, not just the
            // pipeline as a whole).
            fn.block(header).insts().clear();
            fn.block(header).term() = Terminator{};
            fn.block(latch).insts().clear();
            fn.block(latch).term() = Terminator{};

            return true;
        }
    }  // namespace

    bool LoopUnrollPass::run(Function &fn, AnalysisManager &) {
        if (!fn.entry().isValid())
            return false;
        DominatorTree dt(fn);

        for (std::size_t bi = 0; bi < fn.blockCount(); ++bi) {
            BlockId latch(static_cast<BlockId::Value>(bi));
            if (!dt.isReachable(latch))
                continue;
            for (const BlockRef &ref : fn.block(latch).term().targets) {
                BlockId header = ref.block;
                if (!dt.dominates(header, latch))
                    continue;
                if (tryUnroll(module_, fn, dt, header, latch))
                    return true;
            }
        }
        return false;
    }

}  // namespace zust::zir
