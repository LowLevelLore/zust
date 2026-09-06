#pragma once

#include "codegen/machine/MachineIR.hpp"
#include "codegen/machine/TargetABI.hpp"

// docs/PRD-ZIR.md Wave 5.5: "the frame computed once, post-allocation
// (locals + spills + callee-saves + shadow space), replacing 'reserve
// during emission'." Runs after LinearScan (Wave 5.4), once every frame
// slot a function needs -- ZIR `alloca`s and LinearScan's own spill slots
// alike -- already exists in `MachineFunction::frameSlots`.
//
// Every slot (local, spill, or a callee-saved register's own save area) is
// simply 8 bytes here regardless of its declared size: every ZIR scalar
// type this pipeline lowers is 8 bytes or smaller, so packing tighter would
// only complicate offset arithmetic for no benefit at -O0. Callee-saved
// registers are saved/restored via plain `mov`/`movsd` into their own
// slots in the same frame rather than `push`/`pop` -- one uniform
// `sub rsp, N` in the prologue with `N` always a multiple of 16 (Win64's
// stack-alignment requirement), instead of also having to reason about how
// an odd number of 8-byte pushes shifts alignment before that `sub`. Only
// the low 64 bits of a callee-saved XMM register are required to survive a
// call (Win64 ABI), so `movsd` suffices even for saving one that held a
// 32-bit float.
namespace zust::codegen::machine {

    class FrameLayout {
    public:
        static void compute(MachineFunction &mf, const TargetABI &abi);
    };

}  // namespace zust::codegen::machine
