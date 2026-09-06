#pragma once

#include <vector>

#include "zir/Module.hpp"

// CFG + dominance analysis for one Function (docs/IR-DESIGN.md "Pass
// manager": "Analyses (DominatorTree, LoopInfo, AliasAnalysis) are cached in
// the AnalysisManager"). Built once per Function on demand and invalidated
// by any pass that changes control flow (docs/PRD-ZIR.md Wave 4.3).
//
// The Verifier's own dominance check (docs/IR-DESIGN.md check 2) computes
// the same dataflow independently rather than depending on this class --
// that check predates the pass infrastructure (Wave 1.5) and is exercised by
// its own frozen-by-tests fixtures, so it is left alone rather than
// refactored onto a shared implementation opportunistically. A future wave
// can unify them; this class exists for the pass pipeline, not to replace
// the Verifier's copy.

namespace zust::zir {

    class DominatorTree {
    public:
        explicit DominatorTree(const Function &fn);

        bool isReachable(BlockId b) const;

        // Does `a` dominate `b`? True when a == b (every block dominates
        // itself); false for anything involving an unreachable block.
        bool dominates(BlockId a, BlockId b) const;

        // Invalid for the entry block and for an unreachable block.
        BlockId immediateDominator(BlockId b) const;

        const std::vector<BlockId> &predecessors(BlockId b) const;
        const std::vector<BlockId> &successors(BlockId b) const;

        // Blocks in `b`'s dominance frontier (Cytron et al.) -- every block
        // reachable along some path from `b` without going through a block
        // `b` strictly dominates. Empty for an unreachable block. This is
        // what mem2reg (docs/PRD-ZIR.md Wave 4.4) needs to know where to
        // place a merge (a block parameter, ZIR's phi-equivalent) when
        // promoting an alloca with more than one reaching store out of
        // memory.
        const std::vector<BlockId> &dominanceFrontier(BlockId b) const;

        std::size_t blockCount() const { return n_; }

    private:
        std::size_t n_;
        std::vector<std::vector<BlockId>> succ_;
        std::vector<std::vector<BlockId>> preds_;
        std::vector<bool> reachable_;
        std::vector<std::vector<bool>> dom_;  // dom_[b][d]: d dominates b
        std::vector<BlockId> idom_;
        std::vector<std::vector<BlockId>> domFrontier_;
    };

}  // namespace zust::zir
