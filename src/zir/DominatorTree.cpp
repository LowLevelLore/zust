#include "zir/DominatorTree.hpp"

namespace zust::zir {

    namespace {

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
                for (BlockId s : succ[i])
                    preds[s.value()].push_back(BlockId(static_cast<BlockId::Value>(i)));
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

        // Standard iterative dominator dataflow -- see Verifier.cpp's own
        // (independent) copy of this algorithm for the fuller rationale;
        // this one exists for the pass pipeline rather than the verifier.
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
                    for (std::size_t j = 0; j < n; ++j)
                        dom[i][j] = reachable[j];
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
                            for (std::size_t k = 0; k < n; ++k)
                                newDom[k] = newDom[k] && dom[p.value()][k];
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

        // Among a block's strict dominators, dominance is a total order
        // (they all lie on the one path from entry to that block) -- the
        // immediate dominator is the strict dominator with the most
        // dominators of its own, i.e. the one deepest in that chain.
        std::vector<BlockId> computeImmediateDominators(const Function &fn, const std::vector<bool> &reachable,
                                                        const std::vector<std::vector<bool>> &dom) {
            std::size_t n = fn.blockCount();
            std::vector<BlockId> idom(n);
            if (!fn.entry().isValid())
                return idom;
            std::size_t entryIdx = fn.entry().value();
            for (std::size_t i = 0; i < n; ++i) {
                if (!reachable[i] || i == entryIdx)
                    continue;
                long long best = -1;
                std::size_t bestCount = 0;
                for (std::size_t d = 0; d < n; ++d) {
                    if (d == i || !dom[i][d])
                        continue;
                    std::size_t count = 0;
                    for (std::size_t k = 0; k < n; ++k)
                        if (dom[d][k])
                            ++count;
                    if (best < 0 || count > bestCount) {
                        best = static_cast<long long>(d);
                        bestCount = count;
                    }
                }
                if (best >= 0)
                    idom[i] = BlockId(static_cast<BlockId::Value>(best));
            }
            return idom;
        }

        // Cytron, Ferrante, Rosen, Wegman, Zadeck's dominance-frontier
        // algorithm: for every join point (a block with >=2 predecessors),
        // walk each predecessor up its idom chain, adding the join block to
        // every node visited strictly before reaching the join's own
        // immediate dominator.
        std::vector<std::vector<BlockId>> computeDominanceFrontiers(const Function &fn,
                                                                    const std::vector<bool> &reachable,
                                                                    const std::vector<std::vector<BlockId>> &preds,
                                                                    const std::vector<BlockId> &idom) {
            std::size_t n = fn.blockCount();
            std::vector<std::vector<bool>> dfSet(n, std::vector<bool>(n, false));
            for (std::size_t bi = 0; bi < n; ++bi) {
                if (!reachable[bi])
                    continue;
                if (preds[bi].size() < 2)
                    continue;
                for (BlockId p : preds[bi]) {
                    if (!reachable[p.value()])
                        continue;
                    // Climb p's immediate-dominator chain, marking every node
                    // strictly before bi's own immediate dominator -- that
                    // climb is guaranteed to reach idom[bi] (never run off
                    // the top past entry) because idom[bi] dominates p by
                    // construction. `runner.isValid()` is a pure safety net,
                    // never expected to trip on a correct idom_ array.
                    BlockId runner = p;
                    while (runner.isValid() && runner != idom[bi]) {
                        dfSet[runner.value()][bi] = true;
                        runner = idom[runner.value()];
                    }
                }
            }
            std::vector<std::vector<BlockId>> result(n);
            for (std::size_t i = 0; i < n; ++i)
                for (std::size_t j = 0; j < n; ++j)
                    if (dfSet[i][j])
                        result[i].push_back(BlockId(static_cast<BlockId::Value>(j)));
            return result;
        }

    }  // namespace

    DominatorTree::DominatorTree(const Function &fn) : n_(fn.blockCount()) {
        succ_ = computeSuccessors(fn);
        preds_ = computePredecessors(fn, succ_);
        reachable_ = reachableFromEntry(fn, succ_);
        dom_ = computeDominators(fn, preds_, reachable_);
        idom_ = computeImmediateDominators(fn, reachable_, dom_);
        domFrontier_ = computeDominanceFrontiers(fn, reachable_, preds_, idom_);
    }

    bool DominatorTree::isReachable(BlockId b) const {
        return b.isValid() && b.value() < n_ && reachable_[b.value()];
    }

    bool DominatorTree::dominates(BlockId a, BlockId b) const {
        if (!isReachable(a) || !isReachable(b))
            return false;
        return dom_[b.value()][a.value()];
    }

    BlockId DominatorTree::immediateDominator(BlockId b) const {
        if (!isReachable(b))
            return BlockId{};
        return idom_[b.value()];
    }

    const std::vector<BlockId> &DominatorTree::predecessors(BlockId b) const {
        return preds_[b.value()];
    }

    const std::vector<BlockId> &DominatorTree::successors(BlockId b) const {
        return succ_[b.value()];
    }

    const std::vector<BlockId> &DominatorTree::dominanceFrontier(BlockId b) const {
        return domFrontier_[b.value()];
    }

}  // namespace zust::zir
