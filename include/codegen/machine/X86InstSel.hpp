#pragma once

#include <unordered_map>

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

        void selectEntryParamCopyIn();
        void selectBlock(zir::BlockId b);
        void selectInst(const zir::Instruction &inst);
        void selectCall(const zir::Instruction &inst);
        void selectTerminator(const zir::Terminator &t);

        void emit(MachineInst inst);
        MachineBasicBlock *cur_ = nullptr;
    };

}  // namespace zust::codegen::machine
