#include "codegen/machine/SysVAbi.hpp"

namespace zust::codegen::machine {

    const TargetABI &sysVAbi() {
        static const TargetABI abi = [] {
            TargetABI a;
            a.name = "sysv";
            a.intArgRegs = {PhysReg::RDI, PhysReg::RSI, PhysReg::RDX, PhysReg::RCX, PhysReg::R8, PhysReg::R9};
            a.xmmArgRegs = {PhysReg::XMM0, PhysReg::XMM1, PhysReg::XMM2, PhysReg::XMM3,
                            PhysReg::XMM4, PhysReg::XMM5, PhysReg::XMM6, PhysReg::XMM7};
            // SysV keeps independent GPR and XMM argument counters -- a
            // float in argument position 2 still takes xmm0 if it's the
            // first float, and does not burn rdx.
            a.sharedArgSlots = false;

            a.calleeSavedGpr = {PhysReg::RBX, PhysReg::R12, PhysReg::R13, PhysReg::R14, PhysReg::R15};
            a.calleeSavedXmm = {};  // SysV has no callee-saved XMM registers

            // Same design choice as Win64Abi: the allocatable GPR pool is
            // exactly the callee-saved set, so a vreg LinearScan assigns
            // can never alias a register X86InstSel uses momentarily as a
            // fixed physical register (argument passing, return values,
            // idiv's rdx:rax), and every vreg is call-safe unconditionally.
            // r10/r11 are held back as spill scratch (caller-saved, never
            // allocated -- their contents never need to survive anything).
            a.allocatableGpr = {PhysReg::RBX, PhysReg::R12, PhysReg::R13, PhysReg::R14, PhysReg::R15};
            a.scratchGpr1 = PhysReg::R10;
            a.scratchGpr2 = PhysReg::R11;

            // SysV has no callee-saved XMM registers, so no XMM vreg can be
            // made call-safe by register choice alone. This is sound only
            // because -O0 ZirGen never leaves a bare float SSA value live
            // across a call (every local is memory, reloaded per use; a
            // call argument is materialized immediately before the call and
            // a call result copied out immediately after) and LinearScan's
            // live ranges are block-local -- the same assumption Wave 5.3/
            // 5.4 document. xmm8-xmm13 are the allocatable pool (kept off
            // the xmm0-xmm7 argument registers), xmm14/xmm15 the scratch.
            a.allocatableXmm = {PhysReg::XMM8,  PhysReg::XMM9,  PhysReg::XMM10,
                                PhysReg::XMM11, PhysReg::XMM12, PhysReg::XMM13};
            a.scratchXmm1 = PhysReg::XMM14;
            a.scratchXmm2 = PhysReg::XMM15;

            a.returnGpr = PhysReg::RAX;
            a.returnXmm = PhysReg::XMM0;

            a.shadowSpaceBytes = 0;
            a.redZoneBytes = 128;
            a.stackAlignBytes = 16;

            a.variadicRule = VariadicRule::AlHoldsVectorCount;
            a.isIntelSyntax = false;
            return a;
        }();
        return abi;
    }

}  // namespace zust::codegen::machine
