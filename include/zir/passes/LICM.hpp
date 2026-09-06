#pragma once

#include "zir/PassManager.hpp"

// docs/PRD-ZIR.md Wave 4.5, -O3. Loop-invariant code motion: finds a
// natural loop from a back edge (an edge N -> H where H dominates N),
// scoped to the case where H has exactly one predecessor outside the loop
// body with a plain, argument-matching `br` into H -- exactly the shape
// ZirGen's own for/while lowering always produces, so this is the common
// case rather than a narrow one. That predecessor serves as the preheader:
// a pure instruction (the same purity notion GVN uses) inside the loop
// whose operands are all defined outside it gets moved there, once per
// pass invocation (the fixpoint loop picks up anything a first hoist
// exposes).

namespace zust::zir {

    class LICMPass : public Pass {
    public:
        const char *name() const override { return "licm"; }

        bool run(Function &fn, AnalysisManager &am) override;
    };

}  // namespace zust::zir
