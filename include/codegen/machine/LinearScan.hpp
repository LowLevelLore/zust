#pragma once

#include "codegen/machine/MachineIR.hpp"
#include "codegen/machine/TargetABI.hpp"

// docs/PRD-ZIR.md Wave 5.4: "LinearScan with spilling, live-range
// splitting, move coalescing, and explicit crossesCall handling."
//
// Scoped down with LiveIntervals (see its own header comment): live ranges
// are block-local at -O0, so this runs the classic greedy algorithm
// (Poletto & Sarkar) independently per block rather than over one
// function-wide numbering, needs no live-range splitting (a block-local
// interval is never split by anything -- there's no cross-block join to
// split around), and needs no explicit "crosses a call" check: the
// allocatable pool (TargetABI::allocatableGpr/Xmm) *is* exactly the
// callee-saved registers already (see Win64Abi.cpp), so every vreg lands
// somewhere call-safe unconditionally. Move coalescing (recognizing a
// `mov dst, src` where dst and src could just be the same register) is not
// attempted -- a real loss at higher optimization levels, immaterial at
// -O0 where nothing is being coalesced away regardless.
//
// Rewrites `mf` in place: every virtual register operand becomes either a
// physical register or, if spilled, a reload/store around the instruction
// using the ABI's two reserved scratch registers per class (never a third
// spilled operand in one instruction -- not a shape -O0 codegen produces;
// see LinearScan.cpp).

namespace zust::codegen::machine {

    class LinearScan {
    public:
        explicit LinearScan(const TargetABI &abi) : abi_(abi) {}

        void run(MachineFunction &mf);

    private:
        const TargetABI &abi_;

        void allocateBlock(MachineFunction &mf, MachineBasicBlock &block);
    };

}  // namespace zust::codegen::machine
