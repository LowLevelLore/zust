#pragma once

#include "codegen/machine/TargetABI.hpp"

// docs/PRD-ZIR.md Wave 6.2. The `TargetABI` value for Win64: 4 shared
// int/xmm argument slots, callee-saved rbx/rdi/rsi/r12-r15 + xmm6-xmm15,
// 32-byte shadow space, no red zone, variadic floats duplicated into their
// paired GPR, Intel/MASM syntax.

namespace zust::codegen::machine {

    const TargetABI &win64Abi();

}  // namespace zust::codegen::machine
