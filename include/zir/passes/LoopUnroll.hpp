#pragma once

#include "zir/PassManager.hpp"

// docs/PRD-ZIR.md Wave 4.5, -O3. Fully unrolls a small, simple counting
// loop with a compile-time-known trip count: exactly two blocks (header +
// latch -- the shape a `for`/`while` loop is left in after -O1's SimplifyCFG
// has already merged away a separate post-block, which is the common case
// by the time this pass runs in the same fixpoint), a header block
// parameter serving as the induction variable, a header `icmp` against a
// constant bound, and a latch that advances the same parameter by a
// constant step before branching back. Bails out (does nothing) the moment
// any of that shape isn't exactly met, or the computed trip count exceeds a
// small cap -- this is a real but intentionally narrow transformation, not
// a general one.

namespace zust::zir {

    class LoopUnrollPass : public Pass {
    public:
        // Needs the owning Module for TypeTable access (the counter's bit
        // width and signedness), same reason ConstFoldPass does.
        explicit LoopUnrollPass(Module &m) : module_(m) {}

        const char *name() const override { return "loop-unroll"; }

        bool run(Function &fn, AnalysisManager &am) override;

    private:
        Module &module_;
    };

}  // namespace zust::zir
