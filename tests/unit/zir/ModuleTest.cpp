#include <doctest/doctest.h>

#include "zir/Builder.hpp"

using namespace zust::zir;

TEST_CASE("a hand-built module's arena round-trips: ids resolve back to what created them") {
    Module m("t.zz");
    TypeTable &types = m.types();
    TypeId i64 = types.intType(64, true);

    TypeId fnSig = types.fnType({i64, i64}, i64, false);
    FuncId f = m.addFunction(Function("add2", fnSig, false, false));
    Function &fn = m.function(f);
    Builder b(m, fn);

    BlockId entry = b.createBlock("entry");
    fn.setEntry(entry);
    ValueId p0 = b.addBlockParam(entry, i64);
    ValueId p1 = b.addBlockParam(entry, i64);
    b.setInsertBlock(entry);
    ValueId sum = b.binop(Opcode::Add, i64, p0, p1);
    b.ret(sum);

    // Block arguments are ValueIds too, typed the same way instruction
    // results are.
    CHECK(fn.typeOf(p0) == i64);
    CHECK(fn.typeOf(p1) == i64);
    CHECK(fn.entry() == entry);
    CHECK(fn.block(entry).params().size() == 2);
    CHECK(fn.block(entry).params()[0] == p0);
    CHECK(fn.block(entry).params()[1] == p1);

    // Exactly one instruction was emitted, and it round-trips: its InstId
    // resolves back to an Add of the same operands, and its result ValueId
    // resolves back to the same type.
    REQUIRE(fn.block(entry).insts().size() == 1);
    InstId addInstId = fn.block(entry).insts()[0];
    const Instruction &addInst = fn.inst(addInstId);
    CHECK(addInst.op == Opcode::Add);
    CHECK(addInst.type == i64);
    CHECK(addInst.operands.size() == 2);
    CHECK(addInst.operands[0] == p0);
    CHECK(addInst.operands[1] == p1);
    CHECK(addInst.result == sum);
    CHECK(fn.typeOf(sum) == i64);

    // The terminator round-trips too.
    CHECK(fn.block(entry).term().kind == TermKind::Ret);
    CHECK(fn.block(entry).term().retValue == sum);

    // The module round-trips the function back by FuncId.
    CHECK(&m.function(f) == &fn);
    CHECK(m.function(f).name() == "add2");
}

TEST_CASE("ids are stable indices, not invalidated by later arena growth") {
    Module m("t.zz");
    TypeId i32 = m.types().intType(32, true);
    TypeId fnSig = m.types().fnType({}, i32, false);

    FuncId f = m.addFunction(Function("f", fnSig, false, false));
    Function &fn = m.function(f);
    Builder b(m, fn);
    BlockId entry = b.createBlock("entry");
    fn.setEntry(entry);
    b.setInsertBlock(entry);

    ValueId first = b.constInt(i32, 1);
    // Force the instruction arena to grow well past its initial capacity.
    for (int i = 0; i < 1000; ++i) {
        b.constInt(i32, static_cast<std::uint64_t>(i));
    }
    // `first`'s type is still resolvable correctly after all that growth --
    // ValueId/InstId are indices into a vector, not pointers/iterators a
    // reallocation would invalidate.
    CHECK(fn.typeOf(first) == i32);
}
