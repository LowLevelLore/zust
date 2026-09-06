#pragma once

#include <vector>

#include "zir/Module.hpp"

// A ValueId's use count within one Function -- shared by every pass that
// needs to know "is this value dead" (DCE) or "does this pointer ever
// escape beyond load/store" (Mem2Reg's promotability check) without each
// re-deriving its own def-use walk.

namespace zust::zir {

    // Indexed by ValueId::value(); counts every operand reference across
    // every instruction (Instruction::operands) and terminator (branch
    // arguments, a CondBr/Switch's condition, a Ret's value) in `fn`. A
    // block parameter or instruction result with count 0 has no uses at all
    // -- dead, if also free of side effects.
    std::vector<int> countUses(const Function &fn);

}  // namespace zust::zir
