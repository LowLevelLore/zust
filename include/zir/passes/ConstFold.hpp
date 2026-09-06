#pragma once

#include "zir/PassManager.hpp"

// docs/PRD-ZIR.md Wave 4.4, -O1. Folds an instruction whose operands are all
// already `const` into a `const` result in place -- same ValueId, same
// type, so nothing referencing the result needs to change. Deliberately
// scoped to opcodes with an unambiguous, side-effect-free compile-time
// value (every binop/fbinop, icmp/fcmp, unop, and cast; not Select, Gep,
// Call, Load/Store/Alloca, or a pointer-producing cast) -- see
// ConstFold.cpp for exactly which and why.

namespace zust::zir {

    class ConstFoldPass : public Pass {
    public:
        explicit ConstFoldPass(Module &m) : module_(m) {}

        const char *name() const override { return "constfold"; }
        bool run(Function &fn, AnalysisManager &am) override;

    private:
        Module &module_;
    };

}  // namespace zust::zir
