#pragma once

#include "zir/PassManager.hpp"

// docs/PRD-ZIR.md Wave 4.5, -O2. Sparse conditional constant
// propagation, scoped to what ConstFold and SimplifyCFG's own
// constant-condition handling cannot already see on their own: a block
// parameter (ZIR's phi-node equivalent) whose every incoming value, across
// every predecessor edge, turns out to be the exact same constant. Once
// recognized, every *use* of that parameter is rewritten to a fresh `const`
// -- the parameter declaration and the now-redundant branch arguments feeding
// it are left in place (dead, for DCE/future SimplifyCFG work to clean up
// later) rather than renumbering every other parameter at that block to
// remove it, which is not worth the bookkeeping risk for what is already a
// narrow, real win: a loop-invariant merge (most commonly a value nothing in
// the loop body ever actually changes) becomes a plain constant.

namespace zust::zir {

    class SCCPPass : public Pass {
    public:
        const char *name() const override { return "sccp"; }
        bool run(Function &fn, AnalysisManager &am) override;
    };

}  // namespace zust::zir
