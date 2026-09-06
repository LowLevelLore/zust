#pragma once

#include <ostream>

#include "codegen/machine/TargetABI.hpp"
#include "zir/Module.hpp"

// Orchestrates Wave 5's whole pipeline over one already-lowered-and-
// verified zir::Module: X86InstSel -> LinearScan -> FrameLayout for every
// function, then the syntax-appropriate AsmWriter. The one piece of this
// that is genuinely target-specific is which TargetABI is passed in
// (docs/PRD-ZIR.md Wave 6.1/6.2's whole point); `intelSyntax` only picks
// which AsmWriter renders the result, independent of the ABI itself
// (Wave 5.6 exists specifically so that isn't coupled).

namespace zust::codegen::machine {

    void emitNative(zir::Module &m, const TargetABI &abi, bool intelSyntax, std::ostream &out);

}  // namespace zust::codegen::machine
