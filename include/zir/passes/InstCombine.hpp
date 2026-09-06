#pragma once

#include "zir/PassManager.hpp"

// docs/PRD-ZIR.md Wave 4.5, -O2. Peephole algebraic simplifications that
// don't need any constant folding on both sides -- `x + 0`, `x * 1`, `x * 0`,
// `x - 0`, `x ^ 0`, `x & x`, `x | x`, `x - x`, and a same-type cast (a no-op
// that can appear after other passes rewrite operand types). Each rewrite
// replaces every use of the instruction's result with the simpler value
// (never removing the now-dead original instruction itself -- DCE's job).

namespace zust::zir {

    class InstCombinePass : public Pass {
    public:
        const char *name() const override { return "instcombine"; }
        bool run(Function &fn, AnalysisManager &am) override;
    };

}  // namespace zust::zir
