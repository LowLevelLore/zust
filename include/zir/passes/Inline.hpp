#pragma once

#include "zir/PassManager.hpp"

// docs/PRD-ZIR.md Wave 4.5, -O3. Inlines a call to a small, simple,
// non-recursive callee: no control flow of its own (exactly one block,
// ending in `ret`/`ret void`), not `extern`, not variadic, and not part of
// an immediate two-function call cycle with the caller (a cheap, real guard
// against unbounded mutual-recursion growth across the pass-manager
// fixpoint -- true self-recursion is TailCall's job, not this one's, and
// never qualifies here anyway since a self-recursive function always has
// more than one block).
//
// A ModulePass, unlike every other pass so far, because inlining is
// docs/IR-DESIGN.md's own example of something that genuinely needs
// whole-module knowledge (the callee's body lives in a different Function
// than the one being rewritten).

namespace zust::zir {

    class InlinePass : public ModulePass {
    public:
        const char *name() const override { return "inline"; }

        bool run(Module &m, AnalysisManager &am) override;
    };

}  // namespace zust::zir
