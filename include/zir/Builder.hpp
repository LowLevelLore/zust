#pragma once

#include <vector>

#include "zir/Module.hpp"

// The only way to create instructions in a Function -- direct
// Function::addInst calls are the arena's raw API (used by the printer,
// verifier, and text parser to inspect/reconstruct instructions), not how
// lowering or a pass should build them, since it does not allocate result
// ValueIds or maintain the current insertion point.

namespace zust::zir {

    class Builder {
    public:
        Builder(Module &module, Function &fn) : module_(module), fn_(fn) {}

        BlockId createBlock(std::string label) { return fn_.addBlock(std::move(label)); }

        ValueId addBlockParam(BlockId block, TypeId type) {
            ValueId v = fn_.newValue(type);
            fn_.block(block).params().push_back(v);
            return v;
        }

        void setInsertBlock(BlockId block) { insertBlock_ = block; }

        BlockId insertBlock() const { return insertBlock_; }

        ValueId constInt(TypeId ty, std::uint64_t bits) {
            Instruction inst;
            inst.op = Opcode::Const;
            inst.type = ty;
            inst.constant.bits = bits;
            return emit(std::move(inst));
        }

        // `bitsPattern` is the raw IEEE-754 bit pattern (32 or 64 bits,
        // matching `ty`), not a reinterpreted double -- see ConstValue.
        ValueId constFloatBits(TypeId ty, std::uint64_t bitsPattern) {
            Instruction inst;
            inst.op = Opcode::Const;
            inst.type = ty;
            inst.constant.bits = bitsPattern;
            return emit(std::move(inst));
        }

        ValueId alloca_(TypeId elemType, std::uint32_t align = 0) {
            Instruction inst;
            inst.op = Opcode::Alloca;
            inst.type = module_.types().ptrType(elemType);
            inst.elemType = elemType;
            inst.align = align;
            return emit(std::move(inst));
        }

        // The global's own type (module_.global(g).type) already says what
        // it points at, so unlike alloca/gep this needs no separate elemType
        // -- Ptr's own opaqueness means the result type is just "ptr".
        ValueId globalAddr(GlobalId g) {
            Instruction inst;
            inst.op = Opcode::GlobalAddr;
            inst.type = module_.types().ptrType(module_.global(g).type);
            inst.global = g;
            return emit(std::move(inst));
        }

        ValueId load(TypeId ty, ValueId ptr) {
            Instruction inst;
            inst.op = Opcode::Load;
            inst.type = ty;
            inst.operands = {ptr};
            return emit(std::move(inst));
        }

        void store(ValueId value, ValueId ptr) {
            Instruction inst;
            inst.op = Opcode::Store;
            inst.operands = {value, ptr};
            emitVoid(std::move(inst));
        }

        // Covers both binop and fbinop from docs/IR-DESIGN.md -- callers
        // pass the specific Opcode (Add vs FAdd, etc); the split only
        // matters to the printer/parser's textual keyword, not to how the
        // instruction is built.
        ValueId binop(Opcode op, TypeId ty, ValueId a, ValueId b) {
            Instruction inst;
            inst.op = op;
            inst.type = ty;
            inst.operands = {a, b};
            return emit(std::move(inst));
        }

        ValueId icmp(CmpPred pred, TypeId boolTy, ValueId a, ValueId b) {
            Instruction inst;
            inst.op = Opcode::ICmp;
            inst.type = boolTy;
            inst.pred = pred;
            inst.operands = {a, b};
            return emit(std::move(inst));
        }

        ValueId fcmp(CmpPred pred, TypeId boolTy, ValueId a, ValueId b) {
            Instruction inst;
            inst.op = Opcode::FCmp;
            inst.type = boolTy;
            inst.pred = pred;
            inst.operands = {a, b};
            return emit(std::move(inst));
        }

        ValueId unop(Opcode op, TypeId ty, ValueId a) {
            Instruction inst;
            inst.op = op;
            inst.type = ty;
            inst.operands = {a};
            return emit(std::move(inst));
        }

        ValueId cast(Opcode op, ValueId v, TypeId toType) {
            Instruction inst;
            inst.op = op;
            inst.type = toType;
            inst.operands = {v};
            return emit(std::move(inst));
        }

        ValueId gep(TypeId resultTy, TypeId elemType, ValueId base, std::vector<ValueId> indices) {
            Instruction inst;
            inst.op = Opcode::Gep;
            inst.type = resultTy;
            inst.elemType = elemType;
            inst.operands.push_back(base);
            for (ValueId idx : indices)
                inst.operands.push_back(idx);
            return emit(std::move(inst));
        }

        ValueId call(FuncId callee, TypeId retTy, std::vector<ValueId> args) {
            Instruction inst;
            inst.op = Opcode::Call;
            inst.type = retTy;
            inst.callee = callee;
            inst.operands = std::move(args);
            return emit(std::move(inst));
        }

        void callVoid(FuncId callee, std::vector<ValueId> args) {
            Instruction inst;
            inst.op = Opcode::Call;
            inst.callee = callee;
            inst.operands = std::move(args);
            emitVoid(std::move(inst));
        }

        ValueId select(TypeId ty, ValueId cond, ValueId a, ValueId b) {
            Instruction inst;
            inst.op = Opcode::Select;
            inst.type = ty;
            inst.operands = {cond, a, b};
            return emit(std::move(inst));
        }

        void br(BlockId target, std::vector<ValueId> args = {}) {
            Terminator t;
            t.kind = TermKind::Br;
            t.targets = {BlockRef{target, std::move(args)}};
            fn_.block(insertBlock_).term() = std::move(t);
        }

        void condBr(ValueId cond, BlockId thenB, std::vector<ValueId> thenArgs, BlockId elseB,
                    std::vector<ValueId> elseArgs) {
            Terminator t;
            t.kind = TermKind::CondBr;
            t.cond = cond;
            t.targets = {BlockRef{thenB, std::move(thenArgs)}, BlockRef{elseB, std::move(elseArgs)}};
            fn_.block(insertBlock_).term() = std::move(t);
        }

        void ret(ValueId v) {
            Terminator t;
            t.kind = TermKind::Ret;
            t.retValue = v;
            fn_.block(insertBlock_).term() = std::move(t);
        }

        void retVoid() {
            Terminator t;
            t.kind = TermKind::Ret;
            fn_.block(insertBlock_).term() = std::move(t);
        }

        void unreachable() {
            Terminator t;
            t.kind = TermKind::Unreachable;
            fn_.block(insertBlock_).term() = std::move(t);
        }

    private:
        ValueId emit(Instruction inst) {
            ValueId result = fn_.newValue(inst.type);
            inst.result = result;
            fn_.addInst(insertBlock_, std::move(inst));
            return result;
        }

        void emitVoid(Instruction inst) {
            inst.type = module_.types().voidType();
            fn_.addInst(insertBlock_, std::move(inst));
        }

        Module &module_;
        Function &fn_;
        BlockId insertBlock_;
    };

}  // namespace zust::zir
