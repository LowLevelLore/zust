#pragma once

#include "codegen/machine/TargetABI.hpp"

// docs/PRD-ZIR.md Wave 6.1. The `TargetABI` value for x86-64 SysV (Linux):
// 6 int arg regs (rdi rsi rdx rcx r8 r9), 8 XMM arg regs, *independent*
// GPR/XMM argument counters (not shared slots), callee-saved rbx/r12-r15,
// 128-byte red zone, no shadow space, variadic rule = `al` holds the count
// of vector registers used, AT&T syntax.
//
// Shares every line of the Wave 5 machine layer with Win64Abi -- the two
// differ only in this value and in which AsmWriter NativeBackend picks.

namespace zust::codegen::machine {

    const TargetABI &sysVAbi();

}  // namespace zust::codegen::machine
