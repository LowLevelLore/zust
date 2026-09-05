#include "zir/Verifier.hpp"

#include <unordered_map>
#include <unordered_set>

namespace zust::zir {

    const char *toString(VerifierCheck check) {
        switch (check) {
            case VerifierCheck::Terminator:
                return "terminator";
            case VerifierCheck::Dominance:
                return "dominance";
            case VerifierCheck::OperandTypes:
                return "operand-types";
            case VerifierCheck::BranchArgs:
                return "branch-args";
            case VerifierCheck::Predecessors:
                return "predecessors";
            case VerifierCheck::AllocaPlacement:
                return "alloca-placement";
            case VerifierCheck::SingleDef:
                return "single-def";
            case VerifierCheck::ReturnType:
                return "return-type";
        }
        return "unknown";
    }

    namespace {

        void fail(std::vector<VerifierFailure> &out, VerifierCheck check, const Function &fn, std::string detail) {
            out.push_back({check, fn.name(), std::move(detail)});
        }

        std::vector<std::vector<BlockId>> computeSuccessors(const Function &fn) {
            std::vector<std::vector<BlockId>> succ(fn.blockCount());
            for (std::size_t i = 0; i < fn.blockCount(); ++i) {
                const Terminator &t = fn.block(BlockId(static_cast<BlockId::Value>(i))).term();
                for (const BlockRef &ref : t.targets) {
                    if (ref.block.isValid() && ref.block.value() < fn.blockCount())
                        succ[i].push_back(ref.block);
                }
            }
            return succ;
        }

        std::vector<std::vector<BlockId>> computePredecessors(const Function &fn,
                                                               const std::vector<std::vector<BlockId>> &succ) {
            std::vector<std::vector<BlockId>> preds(fn.blockCount());
            for (std::size_t i = 0; i < succ.size(); ++i) {
                for (BlockId s : succ[i]) preds[s.value()].push_back(BlockId(static_cast<BlockId::Value>(i)));
            }
            return preds;
        }

        std::vector<bool> reachableFromEntry(const Function &fn, const std::vector<std::vector<BlockId>> &succ) {
            std::vector<bool> reachable(fn.blockCount(), false);
            if (!fn.entry().isValid())
                return reachable;
            std::vector<BlockId> stack = {fn.entry()};
            reachable[fn.entry().value()] = true;
            while (!stack.empty()) {
                BlockId b = stack.back();
                stack.pop_back();
                for (BlockId s : succ[b.value()]) {
                    if (!reachable[s.value()]) {
                        reachable[s.value()] = true;
                        stack.push_back(s);
                    }
                }
            }
            return reachable;
        }

        // Standard iterative dominator dataflow (correct for any CFG shape,
        // not just reducible ones -- these functions are small enough that
        // asymptotic niceties like Lengauer-Tarjan buy nothing here).
        // dom[u][d] means "block d dominates block u". Blocks unreachable
        // from entry get an all-false row: nothing is checked for dominance
        // inside dead code.
        std::vector<std::vector<bool>> computeDominators(const Function &fn,
                                                          const std::vector<std::vector<BlockId>> &preds,
                                                          const std::vector<bool> &reachable) {
            std::size_t n = fn.blockCount();
            std::vector<std::vector<bool>> dom(n, std::vector<bool>(n, false));
            if (!fn.entry().isValid())
                return dom;
            std::size_t entryIdx = fn.entry().value();
            dom[entryIdx][entryIdx] = true;
            for (std::size_t i = 0; i < n; ++i) {
                if (reachable[i] && i != entryIdx) {
                    for (std::size_t j = 0; j < n; ++j) dom[i][j] = reachable[j];
                }
            }

            bool changed = true;
            while (changed) {
                changed = false;
                for (std::size_t i = 0; i < n; ++i) {
                    if (!reachable[i] || i == entryIdx)
                        continue;
                    std::vector<bool> newDom;
                    bool first = true;
                    for (BlockId p : preds[i]) {
                        if (!reachable[p.value()])
                            continue;
                        if (first) {
                            newDom = dom[p.value()];
                            first = false;
                        } else {
                            for (std::size_t k = 0; k < n; ++k) newDom[k] = newDom[k] && dom[p.value()][k];
                        }
                    }
                    if (first)
                        newDom.assign(n, false);
                    newDom[i] = true;
                    if (newDom != dom[i]) {
                        dom[i] = newDom;
                        changed = true;
                    }
                }
            }
            return dom;
        }

        struct DefSite {
            BlockId block;
            // -1 for a block parameter (defined at block entry, before every
            // instruction); otherwise the instruction's index within that
            // block's insts() list.
            long long pos = -1;
        };

        std::unordered_map<ValueId::Value, DefSite> buildDefSites(const Function &fn) {
            std::unordered_map<ValueId::Value, DefSite> defs;
            for (std::size_t bi = 0; bi < fn.blockCount(); ++bi) {
                BlockId b(static_cast<BlockId::Value>(bi));
                for (ValueId p : fn.block(b).params()) defs[p.value()] = DefSite{b, -1};
                const auto &insts = fn.block(b).insts();
                for (std::size_t ii = 0; ii < insts.size(); ++ii) {
                    const Instruction &inst = fn.inst(insts[ii]);
                    if (inst.result.isValid())
                        defs[inst.result.value()] = DefSite{b, static_cast<long long>(ii)};
                }
            }
            return defs;
        }

        // ---- Check 1: terminator well-formed ----
        // A BasicBlock holds exactly one Terminator (a struct field, not a
        // list), so "exactly one terminator, nowhere else" cannot fail by
        // construction in this implementation -- there is no way to build a
        // block with zero or multiple terminators through any API this
        // codebase exposes. What CAN go wrong, and what this check actually
        // looks for, is the terminator's *shape* being inconsistent with its
        // own kind (wrong target count, a dangling/out-of-range BlockId).
        void checkTerminator(const Function &fn, std::vector<VerifierFailure> &out) {
            for (std::size_t bi = 0; bi < fn.blockCount(); ++bi) {
                const Terminator &t = fn.block(BlockId(static_cast<BlockId::Value>(bi))).term();
                auto checkTarget = [&](const BlockRef &ref, const char *label) {
                    if (!ref.block.isValid() || ref.block.value() >= fn.blockCount()) {
                        fail(out, VerifierCheck::Terminator, fn,
                             "block " + std::to_string(bi) + "'s " + label + " target is not a valid block in this function");
                    }
                };
                switch (t.kind) {
                    case TermKind::Br:
                        if (t.targets.size() != 1)
                            fail(out, VerifierCheck::Terminator, fn,
                                 "block " + std::to_string(bi) + "'s br has " + std::to_string(t.targets.size()) +
                                     " targets, expected 1");
                        else
                            checkTarget(t.targets[0], "br");
                        break;
                    case TermKind::CondBr:
                        if (t.targets.size() != 2)
                            fail(out, VerifierCheck::Terminator, fn,
                                 "block " + std::to_string(bi) + "'s condbr has " + std::to_string(t.targets.size()) +
                                     " targets, expected 2");
                        else {
                            checkTarget(t.targets[0], "condbr then");
                            checkTarget(t.targets[1], "condbr else");
                        }
                        break;
                    case TermKind::Switch:
                        if (t.targets.empty())
                            fail(out, VerifierCheck::Terminator, fn,
                                 "block " + std::to_string(bi) + "'s switch has no default target");
                        else {
                            checkTarget(t.targets[0], "switch default");
                            if (t.targets.size() != t.caseValues.size() + 1)
                                fail(out, VerifierCheck::Terminator, fn,
                                     "block " + std::to_string(bi) + "'s switch has " + std::to_string(t.targets.size()) +
                                         " targets but " + std::to_string(t.caseValues.size()) + " case values");
                            for (std::size_t i = 1; i < t.targets.size(); ++i) checkTarget(t.targets[i], "switch case");
                        }
                        break;
                    case TermKind::Ret:
                    case TermKind::Unreachable:
                        break;  // no targets to validate
                }
            }
        }

        // ---- Check 2: dominance ----
        void checkDominance(const Function &fn, const std::vector<bool> &reachable,
                            const std::vector<std::vector<bool>> &dom, std::vector<VerifierFailure> &out) {
            auto defSites = buildDefSites(fn);

            auto checkUse = [&](ValueId used, BlockId useBlock, long long usePos) {
                auto it = defSites.find(used.value());
                if (it == defSites.end()) {
                    fail(out, VerifierCheck::Dominance, fn, "use of a value with no recorded definition");
                    return;
                }
                const DefSite &def = it->second;
                if (def.block == useBlock) {
                    if (def.pos >= usePos) {
                        fail(out, VerifierCheck::Dominance, fn,
                             "value used before its definition within the same block");
                    }
                    return;
                }
                if (!dom[useBlock.value()][def.block.value()]) {
                    fail(out, VerifierCheck::Dominance, fn,
                         "value's defining block does not dominate the block that uses it");
                }
            };

            for (std::size_t bi = 0; bi < fn.blockCount(); ++bi) {
                if (!reachable[bi])
                    continue;  // dead code is not verified, matching common practice
                BlockId b(static_cast<BlockId::Value>(bi));
                const auto &insts = fn.block(b).insts();
                for (std::size_t ii = 0; ii < insts.size(); ++ii) {
                    for (ValueId operand : fn.inst(insts[ii]).operands)
                        checkUse(operand, b, static_cast<long long>(ii));
                }
                const Terminator &t = fn.block(b).term();
                long long termPos = static_cast<long long>(insts.size());
                if (t.kind == TermKind::CondBr)
                    checkUse(t.cond, b, termPos);
                if (t.kind == TermKind::Ret && t.retValue.isValid())
                    checkUse(t.retValue, b, termPos);
                if (t.kind == TermKind::Switch)
                    checkUse(t.cond, b, termPos);
                for (const BlockRef &ref : t.targets)
                    for (ValueId arg : ref.args) checkUse(arg, b, termPos);
            }
        }

        // ---- Check 3: operand types ----
        void checkOperandTypes(const Module &m, const Function &fn, std::vector<VerifierFailure> &out) {
            auto sameType = [&](TypeId a, TypeId b, const char *what) {
                if (a != b)
                    fail(out, VerifierCheck::OperandTypes, fn, std::string(what) + " type mismatch");
            };
            for (std::size_t bi = 0; bi < fn.blockCount(); ++bi) {
                for (InstId id : fn.block(BlockId(static_cast<BlockId::Value>(bi))).insts()) {
                    const Instruction &inst = fn.inst(id);
                    switch (inst.op) {
                        case Opcode::Add:
                        case Opcode::Sub:
                        case Opcode::Mul:
                        case Opcode::SDiv:
                        case Opcode::UDiv:
                        case Opcode::SRem:
                        case Opcode::URem:
                        case Opcode::And:
                        case Opcode::Or:
                        case Opcode::Xor:
                        case Opcode::Shl:
                        case Opcode::LShr:
                        case Opcode::AShr:
                        case Opcode::FAdd:
                        case Opcode::FSub:
                        case Opcode::FMul:
                        case Opcode::FDiv:
                            if (inst.operands.size() == 2) {
                                sameType(fn.typeOf(inst.operands[0]), inst.type, "binop lhs");
                                sameType(fn.typeOf(inst.operands[1]), inst.type, "binop rhs");
                            }
                            break;
                        case Opcode::ICmp:
                        case Opcode::FCmp:
                            if (inst.operands.size() == 2)
                                sameType(fn.typeOf(inst.operands[0]), fn.typeOf(inst.operands[1]), "comparison operand");
                            if (inst.type != m.types().boolType())
                                fail(out, VerifierCheck::OperandTypes, fn, "comparison result must be bool");
                            break;
                        case Opcode::Neg:
                        case Opcode::Not:
                            if (!inst.operands.empty())
                                sameType(fn.typeOf(inst.operands[0]), inst.type, "unary operand");
                            break;
                        case Opcode::Load:
                            if (!inst.operands.empty() &&
                                m.types().get(fn.typeOf(inst.operands[0])).kind != TypeKind::Ptr)
                                fail(out, VerifierCheck::OperandTypes, fn, "load's pointer operand is not a ptr");
                            break;
                        case Opcode::Store:
                            if (inst.operands.size() == 2 &&
                                m.types().get(fn.typeOf(inst.operands[1])).kind != TypeKind::Ptr)
                                fail(out, VerifierCheck::OperandTypes, fn, "store's pointer operand is not a ptr");
                            break;
                        case Opcode::Gep:
                            if (!inst.operands.empty() &&
                                m.types().get(fn.typeOf(inst.operands[0])).kind != TypeKind::Ptr)
                                fail(out, VerifierCheck::OperandTypes, fn, "gep's base operand is not a ptr");
                            break;
                        case Opcode::Select:
                            if (inst.operands.size() == 3) {
                                if (fn.typeOf(inst.operands[0]) != m.types().boolType())
                                    fail(out, VerifierCheck::OperandTypes, fn, "select's condition is not bool");
                                sameType(fn.typeOf(inst.operands[1]), inst.type, "select true-value");
                                sameType(fn.typeOf(inst.operands[2]), inst.type, "select false-value");
                            }
                            break;
                        case Opcode::Call: {
                            const Function &callee = m.function(inst.callee);
                            const Type &sig = m.types().get(callee.signature());
                            for (std::size_t i = 0; i < sig.params.size() && i < inst.operands.size(); ++i) {
                                sameType(fn.typeOf(inst.operands[i]), sig.params[i], "call argument");
                            }
                            if (inst.result.isValid())
                                sameType(inst.type, sig.ret, "call result");
                            break;
                        }
                        case Opcode::Const:
                        case Opcode::Alloca:
                        case Opcode::Trunc:
                        case Opcode::ZExt:
                        case Opcode::SExt:
                        case Opcode::FPTrunc:
                        case Opcode::FPExt:
                        case Opcode::FPToSI:
                        case Opcode::FPToUI:
                        case Opcode::SIToFP:
                        case Opcode::UIToFP:
                        case Opcode::PtrToInt:
                        case Opcode::IntToPtr:
                        case Opcode::Bitcast:
                            // No general cross-type rule checked here yet --
                            // per-cast-kind width/kind rules are a refinement
                            // for when ZIRGen (Wave 3) starts emitting casts.
                            break;
                    }
                }
            }
        }

        // ---- Check 4: branch args match target params ----
        void checkBranchArgs(const Function &fn, std::vector<VerifierFailure> &out) {
            for (std::size_t bi = 0; bi < fn.blockCount(); ++bi) {
                const Terminator &t = fn.block(BlockId(static_cast<BlockId::Value>(bi))).term();
                for (const BlockRef &ref : t.targets) {
                    if (!ref.block.isValid() || ref.block.value() >= fn.blockCount())
                        continue;  // already reported by checkTerminator
                    const auto &params = fn.block(ref.block).params();
                    if (ref.args.size() != params.size()) {
                        fail(out, VerifierCheck::BranchArgs, fn,
                             "branch to block " + std::to_string(ref.block.value()) + " passes " +
                                 std::to_string(ref.args.size()) + " args, block expects " +
                                 std::to_string(params.size()));
                        continue;
                    }
                    for (std::size_t i = 0; i < params.size(); ++i) {
                        if (fn.typeOf(ref.args[i]) != fn.typeOf(params[i])) {
                            fail(out, VerifierCheck::BranchArgs, fn,
                                 "branch to block " + std::to_string(ref.block.value()) + " arg " + std::to_string(i) +
                                     " type does not match the block parameter's type");
                        }
                    }
                }
            }
        }

        // ---- Check 5: predecessors ----
        // "Every block except entry has a predecessor, or is unreferenced
        // and removable" -- in this implementation, a block with zero
        // predecessors is completely ordinary (dead code a later pass will
        // remove; Builder produces this transiently while wiring up control
        // flow, and it is never itself a bug). What this check actually
        // flags: a block that is unreachable from entry yet still has a
        // predecessor -- i.e. it is reachable only from *within* its own
        // disconnected island of blocks, not simply orphaned. That is not
        // "unreferenced and removable", it's a dangling piece of graph
        // something still intends to reach but structurally can't.
        void checkPredecessors(const Function &fn, const std::vector<bool> &reachable,
                               const std::vector<std::vector<BlockId>> &preds, std::vector<VerifierFailure> &out) {
            for (std::size_t bi = 0; bi < fn.blockCount(); ++bi) {
                if (fn.entry().isValid() && bi == fn.entry().value())
                    continue;
                if (!reachable[bi] && !preds[bi].empty()) {
                    fail(out, VerifierCheck::Predecessors, fn,
                         "block " + std::to_string(bi) +
                             " is unreachable from entry but has a predecessor (dangling, not simply dead)");
                }
            }
        }

        // ---- Check 6: alloca only in entry ----
        void checkAllocaPlacement(const Function &fn, std::vector<VerifierFailure> &out) {
            if (!fn.entry().isValid())
                return;
            for (std::size_t bi = 0; bi < fn.blockCount(); ++bi) {
                if (bi == fn.entry().value())
                    continue;
                for (InstId id : fn.block(BlockId(static_cast<BlockId::Value>(bi))).insts()) {
                    if (fn.inst(id).op == Opcode::Alloca) {
                        fail(out, VerifierCheck::AllocaPlacement, fn,
                             "alloca in block " + std::to_string(bi) + ", must be in the entry block");
                    }
                }
            }
        }

        // ---- Check 7: each ValueId defined exactly once ----
        void checkSingleDef(const Function &fn, std::vector<VerifierFailure> &out) {
            std::unordered_map<ValueId::Value, int> defCount;
            for (std::size_t bi = 0; bi < fn.blockCount(); ++bi) {
                BlockId b(static_cast<BlockId::Value>(bi));
                for (ValueId p : fn.block(b).params()) defCount[p.value()]++;
                for (InstId id : fn.block(b).insts()) {
                    const Instruction &inst = fn.inst(id);
                    if (inst.result.isValid())
                        defCount[inst.result.value()]++;
                }
            }
            for (const auto &[value, count] : defCount) {
                if (count > 1) {
                    fail(out, VerifierCheck::SingleDef, fn,
                         "value v" + std::to_string(value) + " is defined " + std::to_string(count) + " times");
                }
            }
        }

        // ---- Check 8: return type matches ----
        void checkReturnType(const Module &m, const Function &fn, std::vector<VerifierFailure> &out) {
            TypeId declaredRet = m.types().get(fn.signature()).ret;
            for (std::size_t bi = 0; bi < fn.blockCount(); ++bi) {
                const Terminator &t = fn.block(BlockId(static_cast<BlockId::Value>(bi))).term();
                if (t.kind != TermKind::Ret)
                    continue;
                if (t.retValue.isValid()) {
                    if (fn.typeOf(t.retValue) != declaredRet) {
                        fail(out, VerifierCheck::ReturnType, fn,
                             "block " + std::to_string(bi) + " returns a value whose type does not match the "
                                                              "function's declared return type");
                    }
                } else if (declaredRet != m.types().voidType()) {
                    fail(out, VerifierCheck::ReturnType, fn,
                         "block " + std::to_string(bi) + " returns void but the function's declared return type is "
                                                          "not void");
                }
            }
        }

    }  // namespace

    std::vector<VerifierFailure> Verifier::verify(const Module &m) {
        std::vector<VerifierFailure> out;
        for (const Function &fn : m.functions()) {
            if (fn.isExtern())
                continue;

            checkTerminator(fn, out);

            auto succ = computeSuccessors(fn);
            auto preds = computePredecessors(fn, succ);
            auto reachable = reachableFromEntry(fn, succ);
            auto dom = computeDominators(fn, preds, reachable);

            checkDominance(fn, reachable, dom, out);
            checkOperandTypes(m, fn, out);
            checkBranchArgs(fn, out);
            checkPredecessors(fn, reachable, preds, out);
            checkAllocaPlacement(fn, out);
            checkSingleDef(fn, out);
            checkReturnType(m, fn, out);
        }
        return out;
    }

}  // namespace zust::zir
