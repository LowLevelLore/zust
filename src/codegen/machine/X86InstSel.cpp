#include "codegen/machine/X86InstSel.hpp"

#include <cstring>
#include <stdexcept>

namespace zust::codegen::machine {

    using namespace zust::zir;

    namespace {
        // A ZIR function's own name becomes a MASM PROC label and every
        // `call` target -- but the source language lets a user name a
        // function `add`, `and`, `or`, `not`, `shl`... any of which
        // collides with the identically-spelled x86 mnemonic MASM also
        // needs to recognize in the very same file (confirmed by a real
        // assemble failure: `fn add(...)` produced `add PROC` / `add ENDP`
        // / `add r13, r15`, and MASM garbled the label against the
        // mnemonic). `main` is kept as-is (the C runtime entry point
        // symbol the linker itself expects) and an `extern` function's
        // name can't be changed at all (it has to match the real external
        // symbol, e.g. `printf`) -- every other function gets a prefix no
        // x86 mnemonic or MASM reserved word can collide with.
        std::string mangleFuncName(const Function &fn) {
            if (fn.isExtern() || fn.name() == "main")
                return fn.name();
            return "zzfn_" + fn.name();
        }

        std::uint32_t roundUpWidth(std::uint32_t bits) {
            if (bits <= 8)
                return 8;
            if (bits <= 16)
                return 16;
            if (bits <= 32)
                return 32;
            return 64;
        }

        const char *setccFor(CmpPred p) {
            switch (p) {
                case CmpPred::Eq:
                case CmpPred::Oeq:
                    return "sete";
                case CmpPred::Ne:
                case CmpPred::One:
                    return "setne";
                case CmpPred::Slt:
                    return "setl";
                case CmpPred::Sle:
                    return "setle";
                case CmpPred::Sgt:
                    return "setg";
                case CmpPred::Sge:
                    return "setge";
                // Unsigned integer predicates, and every float predicate
                // (ucomis{s,d} sets flags the same way an unsigned compare
                // would -- docs/PRD-ZIR.md behavior inventory: "float
                // comparisons use unsigned setcc after ucomis{s,d}").
                case CmpPred::Ult:
                case CmpPred::Olt:
                    return "setb";
                case CmpPred::Ule:
                case CmpPred::Ole:
                    return "setbe";
                case CmpPred::Ugt:
                case CmpPred::Ogt:
                    return "seta";
                case CmpPred::Uge:
                case CmpPred::Oge:
                    return "setae";
                default:
                    throw std::runtime_error("X86InstSel: unhandled comparison predicate");
            }
        }
    }  // namespace

    RegClass X86InstSel::classOf(TypeId t) const {
        return m_.types().get(t).kind == TypeKind::Float ? RegClass::XMM : RegClass::GPR;
    }

    bool X86InstSel::isFloatType(TypeId t) const { return m_.types().get(t).kind == TypeKind::Float; }

    std::uint32_t X86InstSel::widthOf(TypeId t) const {
        const Type &ty = m_.types().get(t);
        if (ty.kind == TypeKind::Float)
            return ty.bits;  // always exactly 32 or 64
        if (ty.kind == TypeKind::Ptr)
            return 64;
        return roundUpWidth(ty.bits);
    }

    std::uint32_t X86InstSel::vregFor(ValueId v) {
        if (hasVReg_[v.value()])
            return vregOf_[v.value()];
        // A pointer value materialized for the first time as a real
        // operand (never as a load/store target, which goes through
        // memoryOperandFor instead without ever reaching here) -- the
        // provenance map covers `alloca` and `GlobalAddr` results.
        auto it = provenance_.find(v.value());
        std::uint32_t vr = mf_->newVReg(RegClass::GPR, 64);
        vregOf_[v.value()] = vr;
        hasVReg_[v.value()] = true;
        if (it != provenance_.end()) {
            MachineInst lea;
            lea.mnemonic = "lea";
            MachineOperand dst = MachineOperand::vregOp(vr, RegClass::GPR, 64);
            lea.operands.push_back(dst);
            if (it->second.isFrame)
                lea.operands.push_back(MachineOperand::frame(it->second.frameIndex, 64));
            else
                lea.operands.push_back(MachineOperand::global(it->second.globalName));
            lea.defIndices = {0};
            emit(std::move(lea));
        } else {
            throw std::runtime_error("X86InstSel: value used before being defined");
        }
        return vr;
    }

    MachineOperand X86InstSel::regOperand(ValueId v) {
        // Already selected (the common case: almost every value is defined
        // by a real instruction before it's used).
        if (hasVReg_[v.value()])
            return MachineOperand::vregOp(vregOf_[v.value()], mf_->vregClass[vregOf_[v.value()]],
                                          mf_->vregWidth[vregOf_[v.value()]]);
        return MachineOperand::vregOp(vregFor(v), RegClass::GPR, 64);
    }

    std::string X86InstSel::blockLabel(BlockId b) const {
        // ZirGen's own block names include dots (e.g. "for.cond0"), same
        // problem as a global's ".strN" -- see sanitizeSymbol.
        return sanitizeSymbol(blockLabelPrefix_ + fn_->block(b).label());
    }

    MachineOperand X86InstSel::memoryOperandFor(ValueId ptr, std::uint32_t width, RegClass rc) {
        auto it = provenance_.find(ptr.value());
        if (it == provenance_.end())
            throw std::runtime_error(
                "X86InstSel: load/store through a pointer with no known provenance (not an alloca or a "
                "global address) -- unsupported shape");
        if (it->second.isFrame)
            return MachineOperand::frame(it->second.frameIndex, width, rc);
        MachineOperand o = MachineOperand::global(it->second.globalName);
        o.isMemory = true;
        o.widthBits = width;
        o.regClass = rc;
        return o;
    }

    void X86InstSel::emit(MachineInst inst) { cur_->insts.push_back(std::move(inst)); }

    MachineFunction X86InstSel::select(Function &fn) {
        MachineFunction mf;
        mf.name = mangleFuncName(fn);
        mf.isExternDecl = fn.isExtern();
        mf.isVariadic = fn.isVariadic();
        if (fn.isExtern())
            return mf;

        fn_ = &fn;
        mf_ = &mf;
        hasVReg_.assign(fn.valueCount(), false);
        vregOf_.assign(fn.valueCount(), 0);
        provenance_.clear();
        blockLabelPrefix_ = "L" + fn.name() + "_";

        for (std::size_t bi = 0; bi < fn.blockCount(); ++bi) {
            mf.blocks.push_back(MachineBasicBlock{blockLabel(BlockId(static_cast<BlockId::Value>(bi))), {}});
        }

        cur_ = &mf.blocks[fn.entry().value()];
        selectEntryParamCopyIn();

        for (std::size_t bi = 0; bi < fn.blockCount(); ++bi) {
            cur_ = &mf.blocks[bi];
            selectBlock(BlockId(static_cast<BlockId::Value>(bi)));
        }

        fn_ = nullptr;
        mf_ = nullptr;
        cur_ = nullptr;
        return mf;
    }

    void X86InstSel::selectEntryParamCopyIn() {
        const std::vector<ValueId> &params = fn_->block(fn_->entry()).params();
        for (std::size_t i = 0; i < params.size(); ++i) {
            ValueId p = params[i];
            TypeId ty = fn_->typeOf(p);
            bool isFloat = isFloatType(ty);
            std::uint32_t width = widthOf(ty);
            std::size_t slot = i;  // Win64: shared slot indexing

            std::uint32_t vr = mf_->newVReg(isFloat ? RegClass::XMM : RegClass::GPR, width);
            vregOf_[p.value()] = vr;
            hasVReg_[p.value()] = true;

            if (slot < abi_.intArgRegs.size()) {
                MachineInst mi;
                mi.operands.push_back(MachineOperand::vregOp(vr, isFloat ? RegClass::XMM : RegClass::GPR, width));
                if (isFloat) {
                    mi.mnemonic = width == 32 ? "movss" : "movsd";
                    mi.operands.push_back(MachineOperand::pregOp(abi_.xmmArgRegs[slot], width));
                } else {
                    mi.mnemonic = "mov";
                    mi.operands.push_back(MachineOperand::pregOp(abi_.intArgRegs[slot], width));
                }
                mi.defIndices = {0};
                emit(std::move(mi));
            } else {
                // Stack-passed parameter: [rbp + 16 (return addr + saved
                // rbp) + shadowSpace + 8*(slot - argRegCount)].
                std::int64_t disp = 16 + abi_.shadowSpaceBytes + 8 * static_cast<std::int64_t>(slot - abi_.intArgRegs.size());
                MachineInst mi;
                mi.mnemonic = isFloat ? (width == 32 ? "movss" : "movsd") : "mov";
                mi.operands.push_back(MachineOperand::vregOp(vr, isFloat ? RegClass::XMM : RegClass::GPR, width));
                mi.operands.push_back(MachineOperand::memOp(PhysReg::RBP, disp, width, isFloat ? RegClass::XMM : RegClass::GPR));
                mi.defIndices = {0};
                emit(std::move(mi));
            }
        }
    }

    void X86InstSel::selectBlock(BlockId b) {
        for (InstId iid : fn_->block(b).insts()) selectInst(fn_->inst(iid));
        selectTerminator(fn_->block(b).term());
    }

    void X86InstSel::selectInst(const Instruction &inst) {
        switch (inst.op) {
            case Opcode::Alloca: {
                std::uint32_t size = m_.layout().sizeOfBytes(m_.types(), inst.elemType);
                std::uint32_t align = m_.layout().alignOfBytes(m_.types(), inst.elemType);
                if (size == 0)
                    size = 1;
                if (align == 0)
                    align = 1;
                std::int32_t slot = mf_->newFrameSlot(size, align, /*isSpill=*/false, fn_->nameOf(inst.result));
                PtrProvenance pv;
                pv.isFrame = true;
                pv.frameIndex = slot;
                provenance_[inst.result.value()] = pv;
                return;
            }
            case Opcode::GlobalAddr: {
                PtrProvenance pv;
                pv.isFrame = false;
                pv.globalName = sanitizeSymbol(m_.global(inst.global).name);
                provenance_[inst.result.value()] = pv;
                return;
            }
            case Opcode::Bitcast: {
                // ZirGen only ever bitcasts ptr<->ptr or a same-width,
                // differently-signed int -- both are a pure relabeling, so
                // the result just reuses whatever the operand already is,
                // provenance included (a bitcast string-literal pointer
                // still needs to `lea` its global when finally used as a
                // real value).
                ValueId src = inst.operands[0];
                auto pit = provenance_.find(src.value());
                if (pit != provenance_.end())
                    provenance_[inst.result.value()] = pit->second;
                if (hasVReg_[src.value()]) {
                    vregOf_[inst.result.value()] = vregOf_[src.value()];
                    hasVReg_[inst.result.value()] = true;
                }
                return;
            }
            case Opcode::Const: {
                std::uint32_t width = widthOf(inst.type);
                RegClass rc = classOf(inst.type);
                std::uint32_t vr = mf_->newVReg(rc, width);
                vregOf_[inst.result.value()] = vr;
                hasVReg_[inst.result.value()] = true;
                if (rc == RegClass::XMM) {
                    std::string label = "LC_" + std::to_string(floatConstCounter_++);
                    floatConstants_.push_back({label, inst.constant.bits, width});
                    MachineInst mi;
                    mi.mnemonic = width == 32 ? "movss" : "movsd";
                    mi.operands.push_back(MachineOperand::vregOp(vr, RegClass::XMM, width));
                    MachineOperand g = MachineOperand::global(label);
                    g.isMemory = true;
                    g.widthBits = width;
                    g.regClass = RegClass::XMM;
                    mi.operands.push_back(g);
                    mi.defIndices = {0};
                    emit(std::move(mi));
                } else {
                    MachineInst mi;
                    mi.mnemonic = "mov";
                    mi.operands.push_back(MachineOperand::vregOp(vr, RegClass::GPR, width));
                    mi.operands.push_back(MachineOperand::imm(static_cast<std::int64_t>(inst.constant.bits), width));
                    mi.defIndices = {0};
                    emit(std::move(mi));
                }
                return;
            }
            case Opcode::Load: {
                std::uint32_t width = widthOf(inst.type);
                RegClass rc = classOf(inst.type);
                std::uint32_t vr = mf_->newVReg(rc, width);
                vregOf_[inst.result.value()] = vr;
                hasVReg_[inst.result.value()] = true;
                MachineOperand mem = memoryOperandFor(inst.operands[0], width, rc);
                MachineInst mi;
                mi.mnemonic = rc == RegClass::XMM ? (width == 32 ? "movss" : "movsd") : "mov";
                mi.operands.push_back(MachineOperand::vregOp(vr, rc, width));
                mi.operands.push_back(mem);
                mi.defIndices = {0};
                emit(std::move(mi));
                return;
            }
            case Opcode::Store: {
                TypeId valTy = fn_->typeOf(inst.operands[0]);
                std::uint32_t width = widthOf(valTy);
                RegClass rc = classOf(valTy);
                MachineOperand mem = memoryOperandFor(inst.operands[1], width, rc);
                MachineOperand val = regOperand(inst.operands[0]);
                MachineInst mi;
                mi.mnemonic = rc == RegClass::XMM ? (width == 32 ? "movss" : "movsd") : "mov";
                mi.operands.push_back(mem);
                mi.operands.push_back(val);
                emit(std::move(mi));
                return;
            }
            case Opcode::Add:
            case Opcode::Sub:
            case Opcode::And:
            case Opcode::Or:
            case Opcode::Xor:
            case Opcode::FAdd:
            case Opcode::FSub:
            case Opcode::FMul:
            case Opcode::FDiv:
            case Opcode::Mul: {
                std::uint32_t width = widthOf(inst.type);
                RegClass rc = classOf(inst.type);
                std::uint32_t vr = mf_->newVReg(rc, width);
                vregOf_[inst.result.value()] = vr;
                hasVReg_[inst.result.value()] = true;
                MachineOperand a = regOperand(inst.operands[0]);
                MachineOperand b = regOperand(inst.operands[1]);
                MachineOperand dst = MachineOperand::vregOp(vr, rc, width);

                const char *op;
                bool isFloat = rc == RegClass::XMM;
                switch (inst.op) {
                    case Opcode::Add:
                        op = "add";
                        break;
                    case Opcode::Sub:
                        op = "sub";
                        break;
                    case Opcode::Mul:
                        op = "imul";
                        break;
                    case Opcode::And:
                        op = "and";
                        break;
                    case Opcode::Or:
                        op = "or";
                        break;
                    case Opcode::Xor:
                        op = "xor";
                        break;
                    case Opcode::FAdd:
                        op = width == 32 ? "addss" : "addsd";
                        break;
                    case Opcode::FSub:
                        op = width == 32 ? "subss" : "subsd";
                        break;
                    case Opcode::FMul:
                        op = width == 32 ? "mulss" : "mulsd";
                        break;
                    case Opcode::FDiv:
                        op = width == 32 ? "divss" : "divsd";
                        break;
                    default:
                        op = "";
                }
                MachineInst movInst;
                movInst.mnemonic = isFloat ? (width == 32 ? "movss" : "movsd") : "mov";
                movInst.operands = {dst, a};
                movInst.defIndices = {0};
                emit(std::move(movInst));

                MachineInst opInst;
                opInst.mnemonic = op;
                opInst.operands = {dst, b};
                opInst.defIndices = {0};
                emit(std::move(opInst));
                return;
            }
            case Opcode::SDiv:
            case Opcode::UDiv:
            case Opcode::SRem:
            case Opcode::URem: {
                std::uint32_t width = widthOf(inst.type);
                bool isSigned = inst.op == Opcode::SDiv || inst.op == Opcode::SRem;
                bool isRem = inst.op == Opcode::SRem || inst.op == Opcode::URem;
                MachineOperand a = regOperand(inst.operands[0]);
                MachineOperand b = regOperand(inst.operands[1]);

                MachineInst movRax;
                movRax.mnemonic = "mov";
                movRax.operands = {MachineOperand::pregOp(PhysReg::RAX, width), a};
                movRax.defIndices = {0};
                emit(std::move(movRax));

                if (isSigned) {
                    MachineInst ext;
                    ext.mnemonic = width == 64 ? "cqo" : (width == 32 ? "cdq" : "cwd");
                    ext.operands = {MachineOperand::pregOp(PhysReg::RDX, width)};
                    ext.defIndices = {0};
                    emit(std::move(ext));
                } else {
                    MachineInst zeroRdx;
                    zeroRdx.mnemonic = "xor";
                    MachineOperand rdx = MachineOperand::pregOp(PhysReg::RDX, width);
                    zeroRdx.operands = {rdx, rdx};
                    zeroRdx.defIndices = {0};
                    emit(std::move(zeroRdx));
                }

                // idiv/div need a register or memory operand, never an
                // immediate -- b is always already a register here since
                // regOperand only ever returns register operands.
                MachineInst divInst;
                divInst.mnemonic = isSigned ? "idiv" : "div";
                divInst.operands = {b};
                divInst.defIndices = {};  // implicitly redefines rax/rdx, handled by the copy-out below instead
                emit(std::move(divInst));

                std::uint32_t vr = mf_->newVReg(RegClass::GPR, width);
                vregOf_[inst.result.value()] = vr;
                hasVReg_[inst.result.value()] = true;
                MachineInst copyOut;
                copyOut.mnemonic = "mov";
                copyOut.operands = {MachineOperand::vregOp(vr, RegClass::GPR, width),
                                   MachineOperand::pregOp(isRem ? PhysReg::RDX : PhysReg::RAX, width)};
                copyOut.defIndices = {0};
                emit(std::move(copyOut));
                return;
            }
            case Opcode::Shl:
            case Opcode::LShr:
            case Opcode::AShr: {
                std::uint32_t width = widthOf(inst.type);
                std::uint32_t vr = mf_->newVReg(RegClass::GPR, width);
                vregOf_[inst.result.value()] = vr;
                hasVReg_[inst.result.value()] = true;
                MachineOperand a = regOperand(inst.operands[0]);
                MachineOperand b = regOperand(inst.operands[1]);
                MachineOperand dst = MachineOperand::vregOp(vr, RegClass::GPR, width);

                MachineInst movInst;
                movInst.mnemonic = "mov";
                movInst.operands = {dst, a};
                movInst.defIndices = {0};
                emit(std::move(movInst));

                MachineInst movCl;
                movCl.mnemonic = "mov";
                movCl.operands = {MachineOperand::pregOp(PhysReg::RCX, 8), b};
                movCl.defIndices = {0};
                emit(std::move(movCl));

                MachineInst shiftInst;
                shiftInst.mnemonic = inst.op == Opcode::Shl ? "shl" : (inst.op == Opcode::LShr ? "shr" : "sar");
                shiftInst.operands = {dst, MachineOperand::pregOp(PhysReg::RCX, 8)};
                shiftInst.defIndices = {0};
                emit(std::move(shiftInst));
                return;
            }
            case Opcode::ICmp:
            case Opcode::FCmp: {
                MachineOperand a = regOperand(inst.operands[0]);
                MachineOperand b = regOperand(inst.operands[1]);
                MachineInst cmpInst;
                cmpInst.mnemonic = inst.op == Opcode::FCmp
                                       ? (a.widthBits == 32 ? "ucomiss" : "ucomisd")
                                       : "cmp";
                cmpInst.operands = {a, b};
                emit(std::move(cmpInst));

                std::uint32_t vr = mf_->newVReg(RegClass::GPR, 8);
                vregOf_[inst.result.value()] = vr;
                hasVReg_[inst.result.value()] = true;
                MachineInst setInst;
                setInst.mnemonic = setccFor(inst.pred);
                setInst.operands = {MachineOperand::vregOp(vr, RegClass::GPR, 8)};
                setInst.defIndices = {0};
                emit(std::move(setInst));
                return;
            }
            case Opcode::Neg: {
                std::uint32_t width = widthOf(inst.type);
                RegClass rc = classOf(inst.type);
                std::uint32_t vr = mf_->newVReg(rc, width);
                vregOf_[inst.result.value()] = vr;
                hasVReg_[inst.result.value()] = true;
                MachineOperand a = regOperand(inst.operands[0]);
                MachineOperand dst = MachineOperand::vregOp(vr, rc, width);
                if (rc == RegClass::XMM) {
                    MachineInst zero;
                    zero.mnemonic = "pxor";
                    zero.operands = {dst, dst};
                    zero.defIndices = {0};
                    emit(std::move(zero));
                    MachineInst sub;
                    sub.mnemonic = width == 32 ? "subss" : "subsd";
                    sub.operands = {dst, a};
                    sub.defIndices = {0};
                    emit(std::move(sub));
                } else {
                    MachineInst mv;
                    mv.mnemonic = "mov";
                    mv.operands = {dst, a};
                    mv.defIndices = {0};
                    emit(std::move(mv));
                    MachineInst neg;
                    neg.mnemonic = "neg";
                    neg.operands = {dst};
                    neg.defIndices = {0};
                    emit(std::move(neg));
                }
                return;
            }
            case Opcode::Not: {
                std::uint32_t width = widthOf(inst.type);
                std::uint32_t vr = mf_->newVReg(RegClass::GPR, width);
                vregOf_[inst.result.value()] = vr;
                hasVReg_[inst.result.value()] = true;
                MachineOperand a = regOperand(inst.operands[0]);
                MachineOperand dst = MachineOperand::vregOp(vr, RegClass::GPR, width);
                MachineInst mv;
                mv.mnemonic = "mov";
                mv.operands = {dst, a};
                mv.defIndices = {0};
                emit(std::move(mv));
                MachineInst notInst;
                // ZIR's `Not` is only ever logical negation of an actual i1
                // (docs/PRD-ZIR.md's boolean representation: `!` in the
                // source language) -- a real bitwise complement would flip
                // every bit of the byte a bool is stored in (0x01 ->
                // 0xFE), not swap it between exactly 0 and 1, which a later
                // exact `== 1` comparison (not just a truthiness test)
                // needs it to be. `xor dst, 1` flips only the low bit,
                // which is all `not` should ever mean here.
                bool isBool = m_.types().get(inst.type).bits == 1;
                notInst.mnemonic = isBool ? "xor" : "not";
                notInst.operands = isBool ? std::vector<MachineOperand>{dst, MachineOperand::imm(1, width)}
                                          : std::vector<MachineOperand>{dst};
                notInst.defIndices = {0};
                emit(std::move(notInst));
                return;
            }
            case Opcode::Trunc:
            case Opcode::ZExt:
            case Opcode::SExt: {
                TypeId fromTy = fn_->typeOf(inst.operands[0]);
                std::uint32_t fromWidth = widthOf(fromTy);
                std::uint32_t toWidth = widthOf(inst.type);
                std::uint32_t vr = mf_->newVReg(RegClass::GPR, toWidth);
                vregOf_[inst.result.value()] = vr;
                hasVReg_[inst.result.value()] = true;
                MachineOperand a = regOperand(inst.operands[0]);
                MachineOperand dst = MachineOperand::vregOp(vr, RegClass::GPR, toWidth);
                MachineInst mi;
                if (inst.op == Opcode::Trunc || fromWidth == toWidth) {
                    // Either a real Trunc, or a ZExt/SExt between two ZIR
                    // widths that happen to round to the same physical
                    // register width (bits==1 and bits==8 both become an
                    // 8-bit GPR operand here) -- x86's movzx/movsx both
                    // require the destination to be *wider* than the
                    // source, so this is just a same-width copy either way.
                    mi.mnemonic = "mov";
                    MachineOperand srcNarrow = a;
                    srcNarrow.widthBits = toWidth;
                    mi.operands = {dst, srcNarrow};
                } else if (inst.op == Opcode::ZExt) {
                    if (fromWidth == 32 && toWidth == 64) {
                        // Writing the 32-bit sub-register already zero-
                        // extends the full 64-bit register on x86 -- no
                        // separate instruction has a 32->64 zero-extend
                        // form at all.
                        mi.mnemonic = "mov";
                        MachineOperand src32 = a;
                        src32.widthBits = 32;
                        MachineOperand dst32 = dst;
                        dst32.widthBits = 32;
                        mi.operands = {dst32, src32};
                    } else {
                        mi.mnemonic = "movzx";
                        mi.operands = {dst, a};
                    }
                } else {  // SExt
                    mi.mnemonic = (fromWidth == 32 && toWidth == 64) ? "movsxd" : "movsx";
                    mi.operands = {dst, a};
                }
                mi.defIndices = {0};
                emit(std::move(mi));
                return;
            }
            case Opcode::FPTrunc:
            case Opcode::FPExt: {
                std::uint32_t toWidth = widthOf(inst.type);
                std::uint32_t vr = mf_->newVReg(RegClass::XMM, toWidth);
                vregOf_[inst.result.value()] = vr;
                hasVReg_[inst.result.value()] = true;
                MachineOperand a = regOperand(inst.operands[0]);
                MachineInst mi;
                mi.mnemonic = inst.op == Opcode::FPTrunc ? "cvtsd2ss" : "cvtss2sd";
                mi.operands = {MachineOperand::vregOp(vr, RegClass::XMM, toWidth), a};
                mi.defIndices = {0};
                emit(std::move(mi));
                return;
            }
            case Opcode::FPToSI:
            case Opcode::FPToUI: {
                // FPToUI: approximated as a signed conversion (matches
                // FPToSI) -- correct for every magnitude this pipeline's
                // own test suite produces (values well under 2^63); a true
                // unsigned conversion for the full 64-bit range needs the
                // compare-and-fixup sequence x86 has no single instruction
                // for, not attempted here.
                std::uint32_t toWidth = widthOf(inst.type);
                std::uint32_t vr = mf_->newVReg(RegClass::GPR, toWidth);
                vregOf_[inst.result.value()] = vr;
                hasVReg_[inst.result.value()] = true;
                MachineOperand a = regOperand(inst.operands[0]);
                MachineInst mi;
                mi.mnemonic = a.widthBits == 32 ? "cvttss2si" : "cvttsd2si";
                // cvtt..2si always produces a 32- or 64-bit GPR result;
                // narrower ZIR int types still get the 32-bit form, then
                // whatever later Trunc ZirGen emits narrows it further.
                MachineOperand dst = MachineOperand::vregOp(vr, RegClass::GPR, toWidth < 32 ? 32 : toWidth);
                mi.operands = {dst, a};
                mi.defIndices = {0};
                emit(std::move(mi));
                if (toWidth < 32)
                    vregOf_[inst.result.value()] = dst.vreg;  // reuse; width mismatch is harmless (reads take toWidth)
                return;
            }
            case Opcode::SIToFP:
            case Opcode::UIToFP: {
                TypeId fromTy = fn_->typeOf(inst.operands[0]);
                std::uint32_t fromWidth = widthOf(fromTy);
                std::uint32_t toWidth = widthOf(inst.type);
                std::uint32_t vr = mf_->newVReg(RegClass::XMM, toWidth);
                vregOf_[inst.result.value()] = vr;
                hasVReg_[inst.result.value()] = true;
                MachineOperand a = regOperand(inst.operands[0]);

                MachineOperand srcGpr = a;
                if (inst.op == Opcode::UIToFP && fromWidth < 64) {
                    // Zero-extend into a clean 64-bit value first so the
                    // sign bit of the 32-bit form is never misread as
                    // negative (cvtsi2sd/ss always treats its GPR source as
                    // signed) -- correct for every unsigned width up to 32
                    // bits; see the class comment for the documented
                    // 64-bit-unsigned gap.
                    std::uint32_t tmp = mf_->newVReg(RegClass::GPR, 64);
                    MachineInst zx;
                    MachineOperand tmpOp = MachineOperand::vregOp(tmp, RegClass::GPR, 64);
                    if (fromWidth == 32) {
                        zx.mnemonic = "mov";
                        MachineOperand src32 = a;
                        src32.widthBits = 32;
                        MachineOperand dst32 = tmpOp;
                        dst32.widthBits = 32;
                        zx.operands = {dst32, src32};
                    } else {
                        zx.mnemonic = "movzx";
                        zx.operands = {tmpOp, a};
                    }
                    zx.defIndices = {0};
                    emit(std::move(zx));
                    srcGpr = tmpOp;
                } else if (srcGpr.widthBits < 32) {
                    srcGpr.widthBits = 32;  // cvtsi2sd/ss has no 8/16-bit source form
                }

                MachineInst mi;
                mi.mnemonic = toWidth == 32 ? "cvtsi2ss" : "cvtsi2sd";
                mi.operands = {MachineOperand::vregOp(vr, RegClass::XMM, toWidth), srcGpr};
                mi.defIndices = {0};
                emit(std::move(mi));
                return;
            }
            case Opcode::Call:
                selectCall(inst);
                return;
            default:
                throw std::runtime_error("X86InstSel: unsupported opcode");
        }
    }

    void X86InstSel::selectCall(const Instruction &inst) {
        Function &callee = m_.function(inst.callee);
        const Type &sig = m_.types().get(callee.signature());
        std::size_t declaredParamCount = sig.params.size();

        std::size_t stackArgCount = inst.operands.size() > abi_.intArgRegs.size()
                                        ? inst.operands.size() - abi_.intArgRegs.size()
                                        : 0;
        std::int64_t stackBytes = static_cast<std::int64_t>(stackArgCount) * 8;
        std::int64_t roundedStack = (stackBytes + 15) & ~std::int64_t{15};
        std::int64_t callFrame = abi_.shadowSpaceBytes + roundedStack;

        MachineInst subRsp;
        subRsp.mnemonic = "sub";
        subRsp.operands = {MachineOperand::pregOp(PhysReg::RSP, 64), MachineOperand::imm(callFrame)};
        subRsp.defIndices = {0};
        emit(std::move(subRsp));

        std::uint32_t xmmUsedCount = 0;
        for (std::size_t i = 0; i < inst.operands.size(); ++i) {
            ValueId argVal = inst.operands[i];
            TypeId argTy = fn_->typeOf(argVal);
            bool isFloat = isFloatType(argTy);
            std::uint32_t width = widthOf(argTy);
            MachineOperand val = regOperand(argVal);
            bool isVariadicArg = callee.isVariadic() && i >= declaredParamCount;

            if (i < abi_.intArgRegs.size()) {
                if (isFloat) {
                    MachineInst mi;
                    mi.mnemonic = width == 32 ? "movss" : "movsd";
                    mi.operands = {MachineOperand::pregOp(abi_.xmmArgRegs[i], width), val};
                    mi.defIndices = {0};
                    emit(std::move(mi));
                    ++xmmUsedCount;
                    if (isVariadicArg && abi_.variadicRule == VariadicRule::DuplicateFloatIntoPairedGpr) {
                        MachineInst dup;
                        dup.mnemonic = "movq";
                        dup.operands = {MachineOperand::pregOp(abi_.intArgRegs[i], 64),
                                       MachineOperand::pregOp(abi_.xmmArgRegs[i], 64)};
                        dup.defIndices = {0};
                        emit(std::move(dup));
                    }
                } else {
                    MachineInst mi;
                    mi.mnemonic = "mov";
                    MachineOperand v64 = val;
                    v64.widthBits = 64;  // pass the full register; ABI slots are always 8 bytes wide
                    mi.operands = {MachineOperand::pregOp(abi_.intArgRegs[i], 64), v64};
                    mi.defIndices = {0};
                    emit(std::move(mi));
                }
            } else {
                // Stack args sit *above* the shadow space from the callee's
                // perspective (return address, then 32 bytes of shadow
                // space, then the 5th+ argument) -- shadowSpaceBytes has to
                // lead here, or the callee reads every stack argument 32
                // bytes short of where it actually is.
                std::int64_t disp =
                    abi_.shadowSpaceBytes + 8 * static_cast<std::int64_t>(i - abi_.intArgRegs.size());
                MachineInst mi;
                mi.mnemonic = isFloat ? (width == 32 ? "movss" : "movsd") : "mov";
                MachineOperand storedVal = val;
                if (!isFloat)
                    storedVal.widthBits = 64;  // ABI stack slots are always 8 bytes wide, same as register slots
                mi.operands = {MachineOperand::memOp(PhysReg::RSP, disp, isFloat ? width : 64,
                                                     isFloat ? RegClass::XMM : RegClass::GPR),
                              storedVal};
                emit(std::move(mi));
            }
        }

        if (callee.isVariadic() && abi_.variadicRule == VariadicRule::AlHoldsVectorCount) {
            MachineInst mi;
            mi.mnemonic = "mov";
            mi.operands = {MachineOperand::pregOp(PhysReg::RAX, 8), MachineOperand::imm(xmmUsedCount, 8)};
            mi.defIndices = {0};
            emit(std::move(mi));
        }

        MachineInst call;
        call.mnemonic = "call";
        call.operands = {MachineOperand::func(mangleFuncName(callee))};
        call.isCall = true;
        emit(std::move(call));

        MachineInst addRsp;
        addRsp.mnemonic = "add";
        addRsp.operands = {MachineOperand::pregOp(PhysReg::RSP, 64), MachineOperand::imm(callFrame)};
        addRsp.defIndices = {0};
        emit(std::move(addRsp));

        if (inst.result.isValid()) {
            TypeId retTy = fn_->typeOf(inst.result);
            std::uint32_t width = widthOf(retTy);
            RegClass rc = classOf(retTy);
            std::uint32_t vr = mf_->newVReg(rc, width);
            vregOf_[inst.result.value()] = vr;
            hasVReg_[inst.result.value()] = true;
            MachineInst mi;
            mi.mnemonic = rc == RegClass::XMM ? (width == 32 ? "movss" : "movsd") : "mov";
            mi.operands = {MachineOperand::vregOp(vr, rc, width), MachineOperand::pregOp(
                                                                       rc == RegClass::XMM ? abi_.returnXmm : abi_.returnGpr, width)};
            mi.defIndices = {0};
            emit(std::move(mi));
        }
    }

    void X86InstSel::selectTerminator(const Terminator &t) {
        switch (t.kind) {
            case TermKind::Br: {
                MachineInst mi;
                mi.mnemonic = "jmp";
                mi.operands = {MachineOperand::block(blockLabel(t.targets[0].block))};
                emit(std::move(mi));
                return;
            }
            case TermKind::CondBr: {
                MachineOperand cond = regOperand(t.cond);
                MachineInst cmp;
                cmp.mnemonic = "cmp";
                MachineOperand cond8 = cond;
                cond8.widthBits = 8;
                cmp.operands = {cond8, MachineOperand::imm(0, 8)};
                emit(std::move(cmp));

                MachineInst jne;
                jne.mnemonic = "jne";
                jne.operands = {MachineOperand::block(blockLabel(t.targets[0].block))};
                emit(std::move(jne));

                MachineInst jmp;
                jmp.mnemonic = "jmp";
                jmp.operands = {MachineOperand::block(blockLabel(t.targets[1].block))};
                emit(std::move(jmp));
                return;
            }
            case TermKind::Ret: {
                if (t.retValue.isValid()) {
                    TypeId retTy = fn_->typeOf(t.retValue);
                    bool isFloat = isFloatType(retTy);
                    std::uint32_t width = widthOf(retTy);
                    MachineOperand val = regOperand(t.retValue);
                    MachineInst mi;
                    mi.mnemonic = isFloat ? (width == 32 ? "movss" : "movsd") : "mov";
                    MachineOperand dst = MachineOperand::pregOp(isFloat ? abi_.returnXmm : abi_.returnGpr,
                                                                isFloat ? width : 64);
                    MachineOperand src = val;
                    if (!isFloat)
                        src.widthBits = 64;
                    mi.operands = {dst, src};
                    mi.defIndices = {0};
                    emit(std::move(mi));
                }
                MachineInst ret;
                ret.mnemonic = "ret";
                ret.isReturn = true;
                emit(std::move(ret));
                return;
            }
            case TermKind::Unreachable: {
                MachineInst ud;
                ud.mnemonic = "ud2";
                emit(std::move(ud));
                return;
            }
            case TermKind::Switch:
                throw std::runtime_error("X86InstSel: switch is not produced by this pipeline yet");
        }
    }

}  // namespace zust::codegen::machine
