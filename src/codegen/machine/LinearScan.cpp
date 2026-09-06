#include "codegen/machine/LinearScan.hpp"

#include <algorithm>
#include <stdexcept>
#include <unordered_map>

#include "codegen/machine/LiveIntervals.hpp"

namespace zust::codegen::machine {

    namespace {
        struct Active {
            int end;
            std::uint32_t vreg;
            PhysReg reg;
        };

        std::uint32_t bytesFor(std::uint32_t widthBits) {
            return widthBits <= 8 ? 1 : widthBits / 8;
        }

        // Greedy per-class allocation. Returns two maps: vreg -> assigned
        // physical register (only for vregs that got one), and
        // vreg -> new frame slot index (only for vregs that got spilled).
        void allocateClass(MachineFunction &mf, const std::vector<LiveInterval> &intervals,
                           const std::vector<PhysReg> &pool, std::unordered_map<std::uint32_t, PhysReg> &assign,
                           std::unordered_map<std::uint32_t, std::int32_t> &spillSlot) {
            std::vector<Active> active;
            std::vector<PhysReg> free = pool;

            for (const LiveInterval &interval : intervals) {
                // Expire anything that ended before this interval starts.
                for (std::size_t i = 0; i < active.size();) {
                    if (active[i].end < interval.start) {
                        free.push_back(active[i].reg);
                        active.erase(active.begin() + static_cast<long>(i));
                    } else {
                        ++i;
                    }
                }

                if (!free.empty()) {
                    PhysReg r = free.back();
                    free.pop_back();
                    assign[interval.vreg] = r;
                    active.push_back(Active{interval.end, interval.vreg, r});
                    continue;
                }

                // No free register: spill either `interval` itself or
                // whichever active interval ends furthest in the future --
                // whichever leaves the shorter-lived value in a register.
                std::size_t worst = 0;
                for (std::size_t i = 1; i < active.size(); ++i)
                    if (active[i].end > active[worst].end)
                        worst = i;

                if (!active.empty() && active[worst].end > interval.end) {
                    PhysReg r = active[worst].reg;
                    std::uint32_t evicted = active[worst].vreg;
                    spillSlot[evicted] =
                        mf.newFrameSlot(bytesFor(mf.vregWidth[evicted]), bytesFor(mf.vregWidth[evicted]),
                                        /*isSpill=*/true);
                    assign.erase(evicted);
                    active[worst] = Active{interval.end, interval.vreg, r};
                    assign[interval.vreg] = r;
                } else {
                    spillSlot[interval.vreg] =
                        mf.newFrameSlot(bytesFor(mf.vregWidth[interval.vreg]), bytesFor(mf.vregWidth[interval.vreg]),
                                        /*isSpill=*/true);
                }
            }
        }

        // Rewrites every virtual-register operand in `block` to a physical
        // one, inserting reload/store instructions around a spilled
        // vreg's every appearance. A spilled operand is always reloaded
        // before the instruction (even a pure "define" -- reloading stale
        // data that's about to be overwritten is wasted work, never wrong,
        // and far simpler than distinguishing "pure define" from
        // "read-modify-write" per mnemonic; every 2-operand ALU op on x86
        // is the latter) and stored back after if `defIndices` marks it.
        void rewriteBlock(MachineFunction &mf, MachineBasicBlock &block, const TargetABI &abi,
                          const std::unordered_map<std::uint32_t, PhysReg> &assign,
                          const std::unordered_map<std::uint32_t, std::int32_t> &spillSlot) {
            std::vector<MachineInst> result;
            result.reserve(block.insts.size());

            for (MachineInst &inst : block.insts) {
                std::unordered_map<std::uint32_t, PhysReg> scratchInThisInst;
                bool gprScratchUsed = false, xmmScratchUsed = false;

                std::vector<MachineInst> reloads, stores;

                for (std::size_t oi = 0; oi < inst.operands.size(); ++oi) {
                    MachineOperand &op = inst.operands[oi];
                    if (op.kind != OperandKind::Reg || !op.isVirtual)
                        continue;

                    auto ait = assign.find(op.vreg);
                    if (ait != assign.end()) {
                        op.isVirtual = false;
                        op.preg = ait->second;
                        continue;
                    }

                    auto sit = spillSlot.find(op.vreg);
                    if (sit == spillSlot.end())
                        throw std::runtime_error("LinearScan: vreg neither assigned nor spilled");

                    PhysReg scratch;
                    bool alreadyReloaded = true;
                    auto cit = scratchInThisInst.find(op.vreg);
                    if (cit != scratchInThisInst.end()) {
                        scratch = cit->second;
                    } else if (op.regClass == RegClass::GPR) {
                        scratch = gprScratchUsed ? abi.scratchGpr2 : abi.scratchGpr1;
                        gprScratchUsed = true;
                        scratchInThisInst[op.vreg] = scratch;
                        alreadyReloaded = false;
                    } else {
                        scratch = xmmScratchUsed ? abi.scratchXmm2 : abi.scratchXmm1;
                        xmmScratchUsed = true;
                        scratchInThisInst[op.vreg] = scratch;
                        alreadyReloaded = false;
                    }

                    std::uint32_t width = mf.vregWidth[op.vreg];
                    MachineOperand slot = MachineOperand::frame(sit->second, width, op.regClass);
                    MachineOperand scratchOp =
                        MachineOperand::pregOp(scratch, op.regClass == RegClass::XMM ? width : 64);
                    if (op.regClass == RegClass::GPR)
                        scratchOp.widthBits = width;

                    if (!alreadyReloaded) {
                        MachineInst reload;
                        reload.mnemonic = op.regClass == RegClass::XMM ? (width == 32 ? "movss" : "movsd") : "mov";
                        reload.operands = {scratchOp, slot};
                        reload.defIndices = {0};
                        reloads.push_back(std::move(reload));
                    }

                    bool isDef = std::find(inst.defIndices.begin(), inst.defIndices.end(), oi) != inst.defIndices.end();
                    if (isDef) {
                        MachineInst store;
                        store.mnemonic = op.regClass == RegClass::XMM ? (width == 32 ? "movss" : "movsd") : "mov";
                        store.operands = {slot, scratchOp};
                        stores.push_back(std::move(store));
                    }

                    op = scratchOp;
                }

                for (MachineInst &r : reloads)
                    result.push_back(std::move(r));
                result.push_back(std::move(inst));
                for (MachineInst &s : stores)
                    result.push_back(std::move(s));
            }

            block.insts = std::move(result);
        }
    }  // namespace

    void LinearScan::run(MachineFunction &mf) {
        for (MachineBasicBlock &block : mf.blocks)
            allocateBlock(mf, block);
    }

    void LinearScan::allocateBlock(MachineFunction &mf, MachineBasicBlock &block) {
        std::vector<LiveInterval> all = LiveIntervals::compute(block);
        std::vector<LiveInterval> gpr, xmm;
        for (const LiveInterval &iv : all)
            (mf.vregClass[iv.vreg] == RegClass::GPR ? gpr : xmm).push_back(iv);

        std::unordered_map<std::uint32_t, PhysReg> assign;
        std::unordered_map<std::uint32_t, std::int32_t> spillSlot;
        allocateClass(mf, gpr, abi_.allocatableGpr, assign, spillSlot);
        allocateClass(mf, xmm, abi_.allocatableXmm, assign, spillSlot);

        rewriteBlock(mf, block, abi_, assign, spillSlot);

        for (auto &[vreg, reg] : assign)
            if (abi_.isCalleeSaved(reg) &&
                std::find(mf.calleeSavedUsed.begin(), mf.calleeSavedUsed.end(), reg) == mf.calleeSavedUsed.end())
                mf.calleeSavedUsed.push_back(reg);
    }

}  // namespace zust::codegen::machine
