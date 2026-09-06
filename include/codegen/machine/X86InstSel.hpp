#pragma once

#include <unordered_map>
#include <unordered_set>

#include "codegen/machine/MachineIR.hpp"
#include "codegen/machine/TargetABI.hpp"
#include "zir/Module.hpp"

// docs/PRD-ZIR.md Wave 5.2: ZIR -> MachineInst on virtual registers,
// argument/return placement parameterized by TargetABI so 6.1/6.2 share
// every line here and differ only in which TargetABI they pass in.
//
// Scope: lowers exactly the ZIR this pipeline's own ZirGen (Wave 3) and
// pass pipeline (Wave 4) actually produce. A ZIR value's pointer provenance
// (an `alloca`'s frame slot, or a `GlobalAddr`'s symbol) is tracked
// explicitly so a `load`/`store` through it becomes one direct
// memory-operand instruction rather than first materializing an address
// into a register and then dereferencing it -- ZirGen never lets an
// alloca's or a global's address escape anywhere else (Wave 4.4's Mem2Reg
// promotability check established this for allocas; GlobalAddr's only
// other use is a same-type Bitcast, which just forwards its operand's
// provenance). A pointer with no recorded provenance reaching a load/store
// is not a shape this pipeline produces yet and is reported as such rather
// than guessed at.
//
// docs/PRD-ZIR.md Wave 6.4: at -O0, ZirGen never lets a bare ZIR SSA value
// be used anywhere but the block that defines it (every local is memory,
// reloaded fresh per use). Wave 4.4's Mem2Reg breaks that at -O1+ in two
// different ways: a merge introduces a genuinely new value (a block
// parameter) at a join point, but *far* more commonly, promoting an
// `alloca` with a single reaching definition (a function parameter that's
// never reassigned, say) just turns every one of its loads into direct
// uses of one SSA value that dominates them all -- no merge involved, yet
// still very much used across a block boundary the moment any use isn't in
// the same block as the definition.
//
// Both cases get the same fix: a "cross-block" value -- computed once up
// front, by comparing every use's block against its value's defining block
// (a block's own parameter counts as defined in that block) -- gets a
// dedicated frame slot instead of ever needing a vreg live across a block
// boundary. A block parameter's slot is written by every predecessor edge
// that branches to it (storeBranchArgs); an ordinary cross-block
// instruction result's slot is written once, immediately after the
// instruction that computes it. Every read goes through a fresh load from
// the slot, in whichever block does the reading -- exactly like an
// ordinary local. This is what keeps LiveIntervals/LinearScan's block-local
// scope (see their own headers) sound once mem2reg is in the picture: no
// vreg's live range ever needs to cross a block boundary, at the cost of a
// cross-block value living in memory across that one boundary instead of a
// register.

namespace zust::codegen::machine {

    class X86InstSel {
    public:
        X86InstSel(zir::Module &m, const TargetABI &abi) : m_(m), abi_(abi) {}

        MachineFunction select(zir::Function &fn);

        // A `Const` of float type has no x86 immediate-load form at all --
        // it has to come from memory, so selecting one appends an entry
        // here (a small pool shared across every function this X86InstSel
        // instance selects, deduplication left to whoever emits the
        // section: identical bit patterns are rare enough across a whole
        // program not to bother with here). The caller's `.CONST`-section
        // writer reads this after selecting every function.
        struct FloatConstant {
            std::string label;
            std::uint64_t bits;
            std::uint32_t widthBits;
        };
        const std::vector<FloatConstant> &floatConstants() const { return floatConstants_; }

    private:
        struct PtrProvenance {
            bool isFrame = false;
            std::int32_t frameIndex = -1;
            std::string globalName;
        };

        zir::Module &m_;
        const TargetABI &abi_;
        zir::Function *fn_ = nullptr;
        MachineFunction *mf_ = nullptr;

        std::vector<bool> hasVReg_;
        std::vector<std::uint32_t> vregOf_;
        std::unordered_map<zir::ValueId::Value, PtrProvenance> provenance_;
        // Every value's defining block index, or -1 if it has none tracked
        // here (a value with no def instruction/param this pipeline ever
        // reads as an operand, e.g. an unused result) -- built once up
        // front by computeCrossBlockValues() and never mutated after.
        std::vector<std::int32_t> defBlockOf_;
        // Every value used somewhere outside its own defining block -- see
        // the class comment. Drives which values get a slot_ entry below;
        // a non-entry block parameter always gets one regardless (it's
        // never legitimately read from a live vreg in the first place,
        // since nothing ever assigns it one).
        std::unordered_set<zir::ValueId::Value> crossBlock_;
        // A cross-block value's dedicated frame slot (see the class
        // comment): every non-entry block parameter unconditionally, plus
        // any other value found in crossBlock_ above (entry's own
        // parameters, and ordinary instruction results used outside the
        // block that computes them).
        std::unordered_map<zir::ValueId::Value, std::int32_t> slot_;
        // Which block selectBlock/selectEntryParamCopyIn is currently
        // emitting into -- regOperand needs this to tell "reading a
        // cross-block value from the very block that just defined it"
        // (use the live vreg directly, cheaper and still sound under
        // per-block LinearScan) apart from "reading it from anywhere else"
        // (always reload fresh from its slot instead).
        std::size_t curBlock_ = 0;
        std::string blockLabelPrefix_;
        std::vector<FloatConstant> floatConstants_;
        std::uint32_t floatConstCounter_ = 0;

        RegClass classOf(zir::TypeId t) const;
        std::uint32_t widthOf(zir::TypeId t) const;
        bool isFloatType(zir::TypeId t) const;

        std::uint32_t vregFor(zir::ValueId v);
        MachineOperand regOperand(zir::ValueId v);
        std::string blockLabel(zir::BlockId b) const;

        // Resolves a pointer ZIR value to the concrete memory operand a
        // load/store through it should use, given the provenance rules
        // above. Throws if the pointer has no known provenance.
        MachineOperand memoryOperandFor(zir::ValueId ptr, std::uint32_t width, RegClass rc);

        // Builds defBlockOf_ and crossBlock_ for the whole function, before
        // any block is selected -- a predecessor earlier in block order
        // than a value's use (the common case for a loop back-edge, say)
        // still needs to know up front whether that value will need a slot.
        void computeCrossBlockValues();
        void allocateNonEntryParamSlots();
        // Gives `result` a slot_ entry and stores its just-computed vreg
        // into it, if (and only if) `result` is in crossBlock_ and isn't
        // slotted already -- called right after selectInst/selectCall
        // finish computing it, and right after selectEntryParamCopyIn's
        // ABI copy-in for entry's own parameters.
        void maybeStoreCrossBlockResult(zir::ValueId result);
        void selectEntryParamCopyIn();
        void selectBlock(zir::BlockId b);
        void selectInst(const zir::Instruction &inst);
        void selectCall(const zir::Instruction &inst);
        void selectTerminator(const zir::Terminator &t);
        // Stores every branch argument into its target's param slots --
        // called for every edge with args, right before the jump/branch
        // instruction that takes it.
        void storeBranchArgs(zir::BlockId target, const std::vector<zir::ValueId> &args);

        // The label a `jmp`/`jne`/`je` for this specific edge should
        // target. If the edge carries no arguments, that's just the
        // target's own label. If it does, a `condbr` can't store them
        // unconditionally (the *other* edge might be taken instead) -- so
        // this creates a small trampoline block that stores this edge's
        // own arguments and then jumps on to the real target, and returns
        // the trampoline's label instead. An unconditional `br` never needs
        // one (nothing else could run first) and stores directly instead;
        // this only exists for `condbr`.
        std::string edgeLabel(zir::BlockId target, const std::vector<zir::ValueId> &args);
        std::uint32_t edgeCounter_ = 0;

        void emit(MachineInst inst);
        MachineBasicBlock *cur_ = nullptr;
    };

}  // namespace zust::codegen::machine
