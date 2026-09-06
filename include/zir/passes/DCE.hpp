#pragma once

#include "zir/PassManager.hpp"

// docs/PRD-ZIR.md Wave 4.4, -O1. Removes an instruction whose result has no
// uses, provided it has no side effect worth preserving. `Store` and `Call`
// are never removed regardless of use count (docs/IR-DESIGN.md behavior
// inventory: "DCE must treat calls as side-effecting, or -O1 deletes the
// printf every golden depends on" -- a void call has no result to even be
// "unused", but a non-void call whose result nobody reads must survive
// too). Everything else -- arithmetic, casts, comparisons, loads, allocas,
// GlobalAddr -- is pure with respect to the rest of the program and safe to
// drop when dead.

namespace zust::zir {

    class DCEPass : public Pass {
    public:
        const char *name() const override { return "dce"; }

        bool run(Function &fn, AnalysisManager &am) override;
    };

}  // namespace zust::zir
