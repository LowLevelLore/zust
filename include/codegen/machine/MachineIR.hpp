#pragma once

#include <cstdint>
#include <string>
#include <vector>

// docs/PRD-ZIR.md Wave 5.1: MachineFunction/MachineInst/MachineOperand, the
// x86-64 machine-level IR shared by every native backend (Wave 6's Windows
// target now, Linux later). Deliberately syntax-agnostic -- operands are
// stored in a single canonical (Intel, dest-first) order and either
// AsmWriter renders the same MachineFunction its own way, never touching
// these structures. A virtual register is 1:1 with the ZIR SSA value that
// produced it (X86InstSel never reuses one for two different ZIR values),
// so every vreg has exactly one definition point -- what makes computing
// LiveIntervals (Wave 5.3) tractable without a general def-use-chain
// rebuild.

namespace zust::codegen::machine {

    enum class RegClass : std::uint8_t { GPR, XMM };

    // Physical register identity, independent of width -- "RAX" covers
    // rax/eax/ax/al alike; MachineOperand::widthBits picks the sub-name at
    // print time. None is the sentinel for "not a physical register"
    // (either unassigned or this operand isn't a register at all).
    enum class PhysReg : std::uint8_t {
        None = 0,
        RAX,
        RBX,
        RCX,
        RDX,
        RSI,
        RDI,
        RBP,
        RSP,
        R8,
        R9,
        R10,
        R11,
        R12,
        R13,
        R14,
        R15,
        XMM0,
        XMM1,
        XMM2,
        XMM3,
        XMM4,
        XMM5,
        XMM6,
        XMM7,
        XMM8,
        XMM9,
        XMM10,
        XMM11,
        XMM12,
        XMM13,
        XMM14,
        XMM15,
    };

    bool isXmm(PhysReg r);
    const char *physRegName(PhysReg r, std::uint32_t widthBits, bool intelSyntax);

    // ZIR global names aren't necessarily valid assembler symbols -- every
    // string literal ZirGen (Wave 3) synthesizes is named ".strN", and a
    // leading '.' is a directive prefix to MASM (`.data`, `.code`, ...), a
    // syntax error anywhere else. Replaces every '.' with '_'; applied once
    // here rather than at each of the several places a global's name
    // becomes an operand or a data-section label, so they can never drift
    // out of sync with each other.
    std::string sanitizeSymbol(const std::string &name);

    enum class OperandKind : std::uint8_t {
        Reg,
        Imm,
        FrameIndex,  // resolved to an rbp-relative offset by FrameLayout (Wave 5.5)
        Global,      // a module-level symbol, referenced by name
        Block,       // a jump target, referenced by this function's block label
        Func,        // a call target, referenced by function name
    };

    struct MachineOperand {
        OperandKind kind = OperandKind::Imm;

        // Reg
        bool isVirtual = false;
        std::uint32_t vreg = 0;              // meaningful when isVirtual
        PhysReg preg = PhysReg::None;         // meaningful when !isVirtual
        RegClass regClass = RegClass::GPR;
        std::uint32_t widthBits = 64;         // GPR: 8/16/32/64. XMM: 32 (float) or 64 (double).
        bool isMemory = false;                // true: this Reg operand is really "[reg]" (a pointer dereference)
        std::int64_t memDisp = 0;             // byte displacement added to isMemory's address

        // Imm
        std::uint64_t immBits = 0;  // raw bits; immIsFloat says how to read them
        bool immIsFloat = false;

        // FrameIndex
        std::int32_t frameIndex = -1;
        std::int64_t frameExtraOffset = 0;  // e.g. a struct-like access into a multi-slot local (unused today)

        // Global / Block / Func
        std::string symbol;

        static MachineOperand vregOp(std::uint32_t id, RegClass rc, std::uint32_t width) {
            MachineOperand o;
            o.kind = OperandKind::Reg;
            o.isVirtual = true;
            o.vreg = id;
            o.regClass = rc;
            o.widthBits = width;
            return o;
        }

        static MachineOperand pregOp(PhysReg r, std::uint32_t width) {
            MachineOperand o;
            o.kind = OperandKind::Reg;
            o.isVirtual = false;
            o.preg = r;
            o.regClass = isXmm(r) ? RegClass::XMM : RegClass::GPR;
            o.widthBits = width;
            return o;
        }

        // "[reg + disp]" -- a physical-register-relative memory access
        // (used for [rbp + slotOffset] once FrameLayout has resolved a
        // FrameIndex, and for reading through a pointer already sitting in
        // a register).
        static MachineOperand memOp(PhysReg base, std::int64_t disp, std::uint32_t width, RegClass rc = RegClass::GPR) {
            MachineOperand o = pregOp(base, 64);
            o.isMemory = true;
            o.memDisp = disp;
            o.widthBits = width;
            o.regClass = rc;  // the *value* being loaded/stored may be GPR or XMM regardless of the base register
            return o;
        }

        static MachineOperand imm(std::int64_t v, std::uint32_t width = 64) {
            MachineOperand o;
            o.kind = OperandKind::Imm;
            o.immBits = static_cast<std::uint64_t>(v);
            o.widthBits = width;
            return o;
        }

        static MachineOperand immU(std::uint64_t v, std::uint32_t width = 64) {
            MachineOperand o;
            o.kind = OperandKind::Imm;
            o.immBits = v;
            o.widthBits = width;
            return o;
        }

        static MachineOperand frame(std::int32_t idx, std::uint32_t width, RegClass rc = RegClass::GPR) {
            MachineOperand o;
            o.kind = OperandKind::FrameIndex;
            o.frameIndex = idx;
            o.widthBits = width;
            o.regClass = rc;
            return o;
        }

        static MachineOperand global(std::string name) {
            MachineOperand o;
            o.kind = OperandKind::Global;
            o.symbol = std::move(name);
            return o;
        }

        static MachineOperand block(std::string label) {
            MachineOperand o;
            o.kind = OperandKind::Block;
            o.symbol = std::move(label);
            return o;
        }

        static MachineOperand func(std::string name) {
            MachineOperand o;
            o.kind = OperandKind::Func;
            o.symbol = std::move(name);
            return o;
        }
    };

    // One instruction. `mnemonic` is the canonical (Intel-spelling) name --
    // AsmWriterAtt translates a handful that differ (e.g. suffixed
    // "movq"/"movl" forms) and reorders/decorates operands; it never needs
    // a different mnemonic table, since x86 mnemonics themselves are
    // syntax-independent. Operands are in Intel order: destination first.
    struct MachineInst {
        std::string mnemonic;
        std::vector<MachineOperand> operands;
        std::string comment;

        // Bookkeeping the allocator and frame layout need without having to
        // re-derive it from the mnemonic string:
        bool isCall = false;
        bool isReturn = false;
        // Which operand indices this instruction *defines* (writes) versus
        // merely reads -- almost always just operands[0] for a 2-operand
        // instruction, but e.g. `idiv` implicitly defines rax/rdx too
        // (recorded as extra synthetic Reg operands appended after the
        // explicit ones, so LiveIntervals sees them without special-casing
        // the mnemonic).
        std::vector<std::uint32_t> defIndices;
    };

    struct MachineBasicBlock {
        std::string label;
        std::vector<MachineInst> insts;
    };

    // One stack-frame slot -- a local (from a ZIR `alloca`) or a spill
    // (from LinearScan). Never given a concrete offset until FrameLayout
    // runs (Wave 5.5): "the frame computed once, post-allocation."
    struct FrameSlot {
        std::uint32_t sizeBytes = 8;
        std::uint32_t alignBytes = 8;
        bool isSpill = false;
        std::string debugName;  // the local's source name, if any -- purely for asm comments
    };

    struct MachineFunction {
        std::string name;
        bool isExternDecl = false;
        bool isVariadic = false;
        std::vector<MachineBasicBlock> blocks;

        std::uint32_t vregCount = 0;
        std::vector<RegClass> vregClass;
        std::vector<std::uint32_t> vregWidth;

        std::vector<FrameSlot> frameSlots;

        // ---- filled in by FrameLayout (Wave 5.5) ----
        std::int64_t frameSize = 0;
        std::vector<std::int64_t> frameSlotOffsets;  // parallel to frameSlots, rbp-relative (negative)
        std::vector<PhysReg> calleeSavedUsed;         // in save order; epilogue restores in reverse
        std::vector<std::int64_t> calleeSavedOffsets;  // parallel to calleeSavedUsed, rbp-relative
        bool needsFramePointer = true;

        std::uint32_t newVReg(RegClass rc, std::uint32_t width) {
            vregClass.push_back(rc);
            vregWidth.push_back(width);
            return vregCount++;
        }

        std::int32_t newFrameSlot(std::uint32_t sizeBytes, std::uint32_t alignBytes, bool isSpill = false,
                                  std::string debugName = "") {
            frameSlots.push_back(FrameSlot{sizeBytes, alignBytes, isSpill, std::move(debugName)});
            return static_cast<std::int32_t>(frameSlots.size() - 1);
        }

        MachineBasicBlock &block(std::size_t i) { return blocks[i]; }
    };

}  // namespace zust::codegen::machine
