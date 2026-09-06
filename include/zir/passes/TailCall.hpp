#pragma once

#include "zir/PassManager.hpp"

// docs/PRD-ZIR.md Wave 4.5, -O2. Turns a self-recursive call in tail
// position -- `%r = call @f(args...); ret %r` (or `call void @f(args...);
// ret void`) inside `@f` itself -- into a jump back to the entry block with
// `args...` as its new incoming values, eliminating the call and its stack
// frame entirely.
//
// Only applies when the entry block contains no `alloca`: re-entering entry
// re-executes everything in it, and this pipeline's whole mental model
// (docs/IR-DESIGN.md check 6) is that an entry-block alloca runs exactly
// once, as one-time frame setup -- looping back into one would be unsound.
// Mem2Reg runs earlier in the same pipeline and ordinarily clears every
// alloca a function has, so this is the common case by the time -O2's
// pipeline reaches this pass, not a rare one.

namespace zust::zir {

    class TailCallPass : public Pass {
    public:
        // Needs the owning Module to recognize "this call's callee is the
        // function we're currently running over" -- `Instruction::callee`
        // is a FuncId, and Pass::run only receives the Function itself.
        explicit TailCallPass(Module &m) : module_(m) {}

        const char *name() const override { return "tailcall"; }
        bool run(Function &fn, AnalysisManager &am) override;

    private:
        Module &module_;
    };

}  // namespace zust::zir
