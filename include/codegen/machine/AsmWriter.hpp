#pragma once

#include <ostream>
#include <vector>

#include "codegen/machine/MachineIR.hpp"
#include "codegen/machine/TargetABI.hpp"
#include "codegen/machine/X86InstSel.hpp"
#include "zir/Module.hpp"

// docs/PRD-ZIR.md Wave 5.6: AsmWriterAtt and AsmWriterIntel. Both take the
// exact same already-allocated, frame-laid-out MachineFunctions and the
// same TargetABI -- everything target-specific happened earlier
// (X86InstSel/LinearScan/FrameLayout); a writer only decides how to *spell*
// what's already been decided (operand order, register/immediate/memory
// syntax, directives). Each writer inserts a function's prologue once and
// its epilogue before every `ret` it finds -- MachineInst carries no
// prologue/epilogue of its own (Wave 5.5's FrameLayout only computes sizes
// and offsets), since which instructions those are is itself a syntax
// choice (`push`/`pop` vs `mov`, `leave` vs `mov rsp,rbp; pop rbp`) callers
// of this layer shouldn't need to know about.

namespace zust::codegen::machine {

    class AsmWriterIntel {
    public:
        static void emit(const zir::Module &m, const std::vector<MachineFunction> &externs,
                         const std::vector<MachineFunction> &funcs,
                         const std::vector<X86InstSel::FloatConstant> &floatConsts, const TargetABI &abi,
                         std::ostream &out);
    };

    class AsmWriterAtt {
    public:
        static void emit(const zir::Module &m, const std::vector<MachineFunction> &externs,
                         const std::vector<MachineFunction> &funcs,
                         const std::vector<X86InstSel::FloatConstant> &floatConsts, const TargetABI &abi,
                         std::ostream &out);
    };

}  // namespace zust::codegen::machine
