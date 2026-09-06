#pragma once

#include "zir/PassManager.hpp"

// docs/PRD-ZIR.md Wave 4.5, -O2. Global value numbering, scoped to pure
// instructions (every binop/fbinop, icmp/fcmp, unop, cast, and GlobalAddr --
// never Load, Store, Alloca, Call, Select, or Gep, since those either have
// side effects or would need real alias analysis to CSE safely, which this
// pipeline does not have). Walks the dominator tree so an "already computed
// this" table built along one path is only ever visible to blocks that path
// actually dominates -- the standard way a single hash-map lookup can stand
// in for a real dominance check. Redundant instructions are left in place,
// unused, for DCE to remove; this pass only rewrites *uses* to point at the
// earlier, equivalent computation.

namespace zust::zir {

    class GVNPass : public Pass {
    public:
        const char *name() const override { return "gvn"; }

        bool run(Function &fn, AnalysisManager &am) override;
    };

}  // namespace zust::zir
