#pragma once

#include "codegen/machine/MachineIR.hpp"

// docs/PRD-ZIR.md Wave 5.3: "LiveIntervals over the linearized function."
//
// Scoped to what this pipeline's own X86InstSel actually produces at -O0
// (the level Wave 6.2's exit criterion is stated at): every virtual
// register's live range is confined to the one MachineBasicBlock that
// defines it. This holds because every ZIR value ZirGen (Wave 3) emits at
// -O0 is itself block-local -- a local variable is memory (an `alloca`,
// reloaded fresh via a real `load` at every use, never carried as a bare
// SSA value across a block boundary) and every other temporary is consumed
// by the very next instruction or two in the same block. A vreg used in a
// block other than the one that defines it is not a shape this scope
// covers -- raising this past -O0 (mem2reg's cross-block merges,
// Wave 6.4) needs real interprocedural-within-a-function liveness
// (live-in/live-out per block via backward dataflow, the way
// zir::DominatorTree's own analysis works), not attempted here.

namespace zust::codegen::machine {

    struct LiveInterval {
        std::uint32_t vreg;
        int start;  // instruction index within the block, where defined
        int end;    // last instruction index within the block that uses it (>= start)
    };

    class LiveIntervals {
    public:
        // Sorted by `start`. One vreg used in this block without ever
        // being defined in it (see the class comment -- not expected to
        // happen) gets a synthetic interval starting at its first use
        // instead of being silently dropped.
        static std::vector<LiveInterval> compute(const MachineBasicBlock &block);
    };

}  // namespace zust::codegen::machine
