#pragma once

#include <cstdint>
#include <vector>

#include "zir/Ids.hpp"

// Instruction and terminator shapes from docs/IR-DESIGN.md "Instructions".
// A flat struct rather than one class per opcode (mirroring zir::Type) --
// only the fields relevant to `op` are meaningful, the rest sit at default.

namespace zust::zir {

    enum class Opcode : std::uint8_t {
        Const,
        Alloca,
        Load,
        Store,
        // binop (int)
        Add,
        Sub,
        Mul,
        SDiv,
        UDiv,
        SRem,
        URem,
        And,
        Or,
        Xor,
        Shl,
        LShr,
        AShr,
        // fbinop (float) -- same shape as binop, distinct opcodes so the
        // printer/backends never have to re-derive int-vs-float from the type.
        FAdd,
        FSub,
        FMul,
        FDiv,
        ICmp,
        FCmp,
        // unop
        Neg,
        Not,
        // cast
        Trunc,
        ZExt,
        SExt,
        FPTrunc,
        FPExt,
        FPToSI,
        FPToUI,
        SIToFP,
        UIToFP,
        PtrToInt,
        IntToPtr,
        Bitcast,
        Gep,
        Call,
        Select,
    };

    enum class CmpPred : std::uint8_t {
        None,
        // icmp
        Eq,
        Ne,
        Slt,
        Sle,
        Sgt,
        Sge,
        Ult,
        Ule,
        Ugt,
        Uge,
        // fcmp (ordered predicates, per docs/IR-DESIGN.md)
        Oeq,
        One,
        Olt,
        Ole,
        Ogt,
        Oge,
    };

    // A constant's value, stored as raw bits regardless of whether it is an
    // integer or a float -- decided in docs/PRD-ZIR.md's behavior inventory:
    // storing the exact IEEE bit pattern at lowering time (rather than a
    // decimal string re-derived at print time by three different pipelines,
    // as the legacy backends do) removes a whole class of divergence. `type`
    // on the owning Instruction says how to interpret `bits`.
    struct ConstValue {
        std::uint64_t bits = 0;
    };

    // Every instruction lives in a Function's instruction arena, indexed by
    // InstId. `result` is the ValueId it defines (kInvalid for Store and a
    // void Call, which define nothing).
    struct Instruction {
        Opcode op = Opcode::Const;
        TypeId type;     // result type; unset/Void for non-value instructions
        ValueId result;  // kInvalid for non-value instructions

        // Operand order matches docs/IR-DESIGN.md's instruction table:
        //   binop/fbinop/icmp/fcmp: [a, b]
        //   unop/cast:              [a]
        //   load:                   [ptr]
        //   store:                  [value, ptr]
        //   gep:                    [base, idx...]
        //   call:                   [args...]
        //   select:                 [cond, a, b]
        std::vector<ValueId> operands;

        CmpPred pred = CmpPred::None;  // icmp/fcmp
        ConstValue constant;           // const
        FuncId callee;                 // call
        // The type being addressed, for the two opcodes that need one even
        // though ZIR pointers are otherwise opaque ("ptr", no pointee
        // tracked at the type level): alloca's allocated type, or gep's
        // element type (the thing `operands[0]` points at, analogous to
        // LLVM's typed getelementptr first argument).
        TypeId elemType;
        std::uint32_t align = 0;  // alloca
    };

    // A branch target: the block plus the values passed as its block
    // arguments (docs/IR-DESIGN.md "Block arguments instead of phi nodes").
    struct BlockRef {
        BlockId block;
        std::vector<ValueId> args;
    };

    enum class TermKind : std::uint8_t { Br, CondBr, Ret, Switch, Unreachable };

    // Exactly one of these ends every basic block.
    struct Terminator {
        TermKind kind = TermKind::Unreachable;

        ValueId cond;                          // CondBr
        ValueId retValue;                      // Ret (kInvalid == `ret void`)
        std::vector<BlockRef> targets;         // Br: [target]; CondBr: [then, else]; Switch: [default, cases...]
        std::vector<std::int64_t> caseValues;  // Switch, parallel to targets[1..]
    };

}  // namespace zust::zir
