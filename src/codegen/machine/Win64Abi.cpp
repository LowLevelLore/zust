#include "codegen/machine/Win64Abi.hpp"

namespace zust::codegen::machine {

    const TargetABI &win64Abi() {
        static const TargetABI abi = [] {
            TargetABI a;
            a.name = "win64";
            a.intArgRegs = {PhysReg::RCX, PhysReg::RDX, PhysReg::R8, PhysReg::R9};
            a.xmmArgRegs = {PhysReg::XMM0, PhysReg::XMM1, PhysReg::XMM2, PhysReg::XMM3};
            a.sharedArgSlots = true;

            a.calleeSavedGpr = {PhysReg::RBX, PhysReg::RDI, PhysReg::RSI, PhysReg::R12,
                               PhysReg::R13, PhysReg::R14, PhysReg::R15};
            a.calleeSavedXmm = {PhysReg::XMM6,  PhysReg::XMM7,  PhysReg::XMM8,  PhysReg::XMM9,
                               PhysReg::XMM10, PhysReg::XMM11, PhysReg::XMM12, PhysReg::XMM13,
                               PhysReg::XMM14, PhysReg::XMM15};

            // The allocatable pool is deliberately *only* the callee-saved
            // registers (never rax/rcx/rdx/r8/r9/r10/r11 or xmm0-5): those
            // are used directly, as fixed physical registers, for argument
            // passing, return values, and idiv/div's implicit rdx:rax --
            // all momentarily, around a call or a single instruction, by
            // X86InstSel itself (never assigned to a vreg by LinearScan).
            // Keeping them completely out of the general pool sidesteps
            // needing precise interference tracking between "a vreg
            // LinearScan happened to put in rcx" and "this call's own
            // argument temporarily lives in rcx" -- they're simply never
            // the same register. It also means Wave 5.4's "a value live
            // across a call must land in a callee-saved register" is true
            // of every vreg unconditionally, not just the ones that
            // actually cross a call.
            a.allocatableGpr = {PhysReg::RBX, PhysReg::RDI, PhysReg::RSI, PhysReg::R12,
                               PhysReg::R13, PhysReg::R14, PhysReg::R15};
            a.scratchGpr1 = PhysReg::R10;
            a.scratchGpr2 = PhysReg::R11;

            // xmm14/xmm15 held back as scratch for the same reason.
            a.allocatableXmm = {PhysReg::XMM6, PhysReg::XMM7,  PhysReg::XMM8,  PhysReg::XMM9,
                               PhysReg::XMM10, PhysReg::XMM11, PhysReg::XMM12, PhysReg::XMM13};
            a.scratchXmm1 = PhysReg::XMM14;
            a.scratchXmm2 = PhysReg::XMM15;

            a.returnGpr = PhysReg::RAX;
            a.returnXmm = PhysReg::XMM0;

            a.shadowSpaceBytes = 32;
            a.redZoneBytes = 0;
            a.stackAlignBytes = 16;

            a.variadicRule = VariadicRule::DuplicateFloatIntoPairedGpr;
            a.isIntelSyntax = true;
            return a;
        }();
        return abi;
    }

}  // namespace zust::codegen::machine
