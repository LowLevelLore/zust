#include <doctest/doctest.h>

#include "zir/Builder.hpp"
#include "zir/Verifier.hpp"

using namespace zust::zir;

namespace {
    bool hasCheck(const std::vector<VerifierFailure> &failures, VerifierCheck check) {
        for (const auto &f : failures) {
            if (f.check == check)
                return true;
        }
        return false;
    }
}  // namespace

TEST_CASE("Verifier reports nothing for a well-formed module") {
    Module m("t.zz");
    TypeId i64 = m.types().intType(64, true);
    TypeId sig = m.types().fnType({i64}, i64, false);
    FuncId f = m.addFunction(Function("id", sig, false, false));
    Function &fn = m.function(f);
    Builder b(m, fn);
    BlockId entry = b.createBlock("entry");
    fn.setEntry(entry);
    ValueId n = b.addBlockParam(entry, i64);
    b.setInsertBlock(entry);
    b.ret(n);

    CHECK(Verifier::verify(m).empty());
}

// Check 1: terminator well-formed. A Br terminator with zero targets can
// only be constructed by bypassing Builder (which always builds one) --
// exactly the point: this is not reachable through the normal API, only by
// directly assigning a malformed Terminator.
TEST_CASE("Verifier check 1 (terminator): a br with no target fails") {
    Module m("t.zz");
    TypeId voidTy = m.types().voidType();
    TypeId sig = m.types().fnType({}, voidTy, false);
    FuncId f = m.addFunction(Function("f", sig, false, false));
    Function &fn = m.function(f);
    BlockId entry = fn.addBlock("entry");
    fn.setEntry(entry);

    Terminator broken;
    broken.kind = TermKind::Br;  // targets left empty -- br requires exactly 1
    fn.block(entry).term() = broken;

    auto failures = Verifier::verify(m);
    CHECK(hasCheck(failures, VerifierCheck::Terminator));
}

// Check 2: dominance. `right` uses a value defined only in the sibling
// branch `left`, which does not dominate it.
TEST_CASE("Verifier check 2 (dominance): using a value from a non-dominating sibling block fails") {
    Module m("t.zz");
    TypeId i64 = m.types().intType(64, true);
    TypeId sig = m.types().fnType({}, i64, false);
    FuncId f = m.addFunction(Function("f", sig, false, false));
    Function &fn = m.function(f);
    Builder b(m, fn);

    BlockId entry = b.createBlock("entry");
    BlockId left = b.createBlock("left");
    BlockId right = b.createBlock("right");
    fn.setEntry(entry);

    b.setInsertBlock(entry);
    ValueId cond = b.constInt(m.types().intType(1, false), 1);
    b.condBr(cond, left, {}, right, {});

    b.setInsertBlock(left);
    ValueId x = b.constInt(i64, 42);
    b.ret(x);

    b.setInsertBlock(right);
    b.ret(x);  // `x` is only defined in `left`, which does not dominate `right`

    auto failures = Verifier::verify(m);
    CHECK(hasCheck(failures, VerifierCheck::Dominance));
}

// Check 3: operand types. Builder does not itself enforce that an
// instruction's declared result/operand types agree -- that is exactly the
// Verifier's job.
TEST_CASE("Verifier check 3 (operand types): an add with a mismatched operand type fails") {
    Module m("t.zz");
    TypeId i64 = m.types().intType(64, true);
    TypeId i32 = m.types().intType(32, true);
    TypeId sig = m.types().fnType({}, i64, false);
    FuncId f = m.addFunction(Function("f", sig, false, false));
    Function &fn = m.function(f);
    Builder b(m, fn);
    BlockId entry = b.createBlock("entry");
    fn.setEntry(entry);
    b.setInsertBlock(entry);

    ValueId a = b.constInt(i64, 1);
    ValueId c = b.constInt(i32, 2);  // wrong width for an i64 add
    ValueId sum = b.binop(Opcode::Add, i64, a, c);
    b.ret(sum);

    auto failures = Verifier::verify(m);
    CHECK(hasCheck(failures, VerifierCheck::OperandTypes));
}

// Check 4: branch args. The target block declares one i64 parameter; the
// branch passes none.
TEST_CASE("Verifier check 4 (branch args): wrong argument count to a block with params fails") {
    Module m("t.zz");
    TypeId i64 = m.types().intType(64, true);
    TypeId voidTy = m.types().voidType();
    TypeId sig = m.types().fnType({}, voidTy, false);
    FuncId f = m.addFunction(Function("f", sig, false, false));
    Function &fn = m.function(f);
    Builder b(m, fn);

    BlockId entry = b.createBlock("entry");
    BlockId target = b.createBlock("target");
    fn.setEntry(entry);
    b.addBlockParam(target, i64);  // `target` expects one argument

    b.setInsertBlock(entry);
    b.br(target, {});  // passes zero

    b.setInsertBlock(target);
    b.retVoid();

    auto failures = Verifier::verify(m);
    CHECK(hasCheck(failures, VerifierCheck::BranchArgs));
}

// Check 5: predecessors. `dead1` has zero predecessors (ordinary, harmless,
// dead code) but branches to `dead2`, which is therefore unreachable from
// entry yet still has a predecessor -- a dangling island, not simply
// "unreferenced and removable".
TEST_CASE("Verifier check 5 (predecessors): a block reachable only from an unreachable island fails") {
    Module m("t.zz");
    TypeId voidTy = m.types().voidType();
    TypeId sig = m.types().fnType({}, voidTy, false);
    FuncId f = m.addFunction(Function("f", sig, false, false));
    Function &fn = m.function(f);
    Builder b(m, fn);

    BlockId entry = b.createBlock("entry");
    BlockId dead1 = b.createBlock("dead1");
    BlockId dead2 = b.createBlock("dead2");
    fn.setEntry(entry);

    b.setInsertBlock(entry);
    b.retVoid();  // entry never references dead1 or dead2

    b.setInsertBlock(dead1);
    b.br(dead2, {});  // dead1 -> dead2, but nothing reaches dead1 either

    b.setInsertBlock(dead2);
    b.retVoid();

    auto failures = Verifier::verify(m);
    CHECK(hasCheck(failures, VerifierCheck::Predecessors));
}

// Check 6: alloca placement. Builder::alloca_ will happily emit into
// whatever block is currently selected -- placement discipline is the
// Verifier's job, not the Builder's.
TEST_CASE("Verifier check 6 (alloca placement): alloca outside the entry block fails") {
    Module m("t.zz");
    TypeId i64 = m.types().intType(64, true);
    TypeId voidTy = m.types().voidType();
    TypeId sig = m.types().fnType({}, voidTy, false);
    FuncId f = m.addFunction(Function("f", sig, false, false));
    Function &fn = m.function(f);
    Builder b(m, fn);

    BlockId entry = b.createBlock("entry");
    BlockId other = b.createBlock("other");
    fn.setEntry(entry);

    b.setInsertBlock(entry);
    b.br(other, {});

    b.setInsertBlock(other);
    b.alloca_(i64);  // not in the entry block
    b.retVoid();

    auto failures = Verifier::verify(m);
    CHECK(hasCheck(failures, VerifierCheck::AllocaPlacement));
}

// Check 7: single definition. Not reachable through Builder (it always
// allocates a fresh ValueId per instruction) -- only by using the raw
// Function::addInst arena API to force a second instruction to claim an
// already-defined ValueId as its own result.
TEST_CASE("Verifier check 7 (single def): the same ValueId defined by two instructions fails") {
    Module m("t.zz");
    TypeId i64 = m.types().intType(64, true);
    TypeId voidTy = m.types().voidType();
    TypeId sig = m.types().fnType({}, voidTy, false);
    FuncId f = m.addFunction(Function("f", sig, false, false));
    Function &fn = m.function(f);
    BlockId entry = fn.addBlock("entry");
    fn.setEntry(entry);

    ValueId shared = fn.newValue(i64);

    Instruction first;
    first.op = Opcode::Const;
    first.type = i64;
    first.result = shared;
    first.constant.bits = 1;
    fn.addInst(entry, first);

    Instruction second;
    second.op = Opcode::Const;
    second.type = i64;
    second.result = shared;  // reuses the same ValueId
    second.constant.bits = 2;
    fn.addInst(entry, second);

    Terminator ret;
    ret.kind = TermKind::Ret;
    fn.block(entry).term() = ret;

    auto failures = Verifier::verify(m);
    CHECK(hasCheck(failures, VerifierCheck::SingleDef));
}

// Check 8: return type. The function is declared to return i64; this
// returns an i32.
TEST_CASE("Verifier check 8 (return type): returning the wrong type fails") {
    Module m("t.zz");
    TypeId i64 = m.types().intType(64, true);
    TypeId i32 = m.types().intType(32, true);
    TypeId sig = m.types().fnType({}, i64, false);
    FuncId f = m.addFunction(Function("f", sig, false, false));
    Function &fn = m.function(f);
    Builder b(m, fn);
    BlockId entry = b.createBlock("entry");
    fn.setEntry(entry);
    b.setInsertBlock(entry);

    ValueId wrong = b.constInt(i32, 1);
    b.ret(wrong);

    auto failures = Verifier::verify(m);
    CHECK(hasCheck(failures, VerifierCheck::ReturnType));
}
