#pragma once

#include "zir/PassManager.hpp"

// docs/PRD-ZIR.md Wave 4.4, -O1. Promotes an entry-block alloca whose every
// use is a plain Load or Store (through it, never of it -- its address
// never escapes into a Call argument, a Gep base, a cast, or anywhere else)
// out of memory entirely, into direct SSA values threaded through block
// parameters at merge points (ZIR's phi-node equivalent,
// docs/IR-DESIGN.md "Block arguments instead of phi nodes"), via the
// standard dominance-frontier phi-placement + dominator-tree-order rename
// algorithm (Cytron et al.).
//
// A promotable-by-shape alloca is still excluded if any of its loads has no
// reaching store along every path from entry (the known "block scoping is
// currently disabled" gap, CLAUDE.md -- `let` inside an if/loop leaks into
// the enclosing scope, so a variable declared inside a branch that was not
// taken can, in today's undefined-but-not-crashing behavior, be read with
// whatever was on the stack). Promoting it anyway would require inventing an
// `undef` value ZIR does not have; leaving it as alloca/load/store instead
// preserves that existing behavior exactly rather than changing it.

namespace zust::zir {

    class Mem2RegPass : public Pass {
    public:
        const char *name() const override { return "mem2reg"; }
        bool run(Function &fn, AnalysisManager &am) override;
    };

}  // namespace zust::zir
