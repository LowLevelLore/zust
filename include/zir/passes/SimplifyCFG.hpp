#pragma once

#include "zir/PassManager.hpp"

// docs/PRD-ZIR.md Wave 4.4, -O1. Two transformations, applied one category
// at a time per run() (so each always sees a CFG it can trust -- see
// SimplifyCFG.cpp):
//   1. A `condbr` whose condition is already `const` becomes an
//      unconditional `br` to whichever side is actually taken.
//   2. A block that ends in an unconditional `br` to a target with no other
//      predecessor gets that target's whole body appended to it (with the
//      target's block parameters, if any, substituted for the branch's
//      arguments) -- the target becomes an orphaned, unreferenced block,
//      which the Verifier's predecessor check explicitly tolerates.

namespace zust::zir {

    class SimplifyCFGPass : public Pass {
    public:
        const char *name() const override { return "simplifycfg"; }
        bool run(Function &fn, AnalysisManager &am) override;
    };

}  // namespace zust::zir
