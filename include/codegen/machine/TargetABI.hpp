#pragma once

#include <cstdint>
#include <vector>

#include "codegen/machine/MachineIR.hpp"

// docs/PRD-ZIR.md Wave 5.2 / Wave 6: everything about a calling convention
// that X86InstSel, LinearScan, and FrameLayout need, collected in one value
// so 6.1 (SysVAbi) and 6.2 (Win64Abi) differ only in *which TargetABI they
// build*, never in a branch inside the shared machine layer -- matching
// CLAUDE.md's "no layer outside src/codegen/ may branch on target", and
// within src/codegen/, ideally no branch on target at all outside this one
// value's construction.

namespace zust::codegen::machine {

    enum class VariadicRule : std::uint8_t {
        // Win64: a variadic call duplicates each float argument into its
        // paired integer register (slot N's GPR and XMM alias the same
        // argument), since a variadic callee (e.g. printf) has no way to
        // know which slots were passed as floats.
        DuplicateFloatIntoPairedGpr,
        // SysV: `al` holds the count of vector (XMM) registers used for
        // arguments, read by the callee's va_start machinery.
        AlHoldsVectorCount,
    };

    struct TargetABI {
        const char *name = "";

        std::vector<PhysReg> intArgRegs;
        std::vector<PhysReg> xmmArgRegs;
        // Win64: true -- argument slot N is *either* the Nth int-arg reg or
        // the Nth xmm-arg reg, never both (a float in slot 1 still burns
        // rdx). SysV: false -- independent GPR/XMM counters.
        bool sharedArgSlots = false;

        std::vector<PhysReg> calleeSavedGpr;
        std::vector<PhysReg> calleeSavedXmm;
        // The pools LinearScan actually draws from -- callee-saved lists
        // above are a *subset* of these (RSP/RBP and the two reserved
        // scratch registers per class are excluded from both).
        std::vector<PhysReg> allocatableGpr;
        std::vector<PhysReg> allocatableXmm;
        PhysReg scratchGpr1 = PhysReg::None, scratchGpr2 = PhysReg::None;
        PhysReg scratchXmm1 = PhysReg::None, scratchXmm2 = PhysReg::None;

        PhysReg returnGpr = PhysReg::RAX;
        PhysReg returnXmm = PhysReg::XMM0;

        std::uint32_t shadowSpaceBytes = 0;
        std::uint32_t redZoneBytes = 0;
        std::uint32_t stackAlignBytes = 16;

        VariadicRule variadicRule = VariadicRule::AlHoldsVectorCount;
        bool isIntelSyntax = false;

        bool isCalleeSaved(PhysReg r) const {
            for (PhysReg c : calleeSavedGpr)
                if (c == r)
                    return true;
            for (PhysReg c : calleeSavedXmm)
                if (c == r)
                    return true;
            return false;
        }
    };

}  // namespace zust::codegen::machine
