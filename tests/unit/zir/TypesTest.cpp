#include <doctest/doctest.h>

#include "zir/Types.hpp"

using namespace zust::zir;

TEST_CASE("TypeTable interns structurally identical types to the same TypeId") {
    TypeTable table;

    CHECK(table.intType(64, true) == table.intType(64, true));
    CHECK(table.intType(64, true) != table.intType(64, false));
    CHECK(table.intType(32, true) != table.intType(64, true));
    CHECK(table.floatType(64) == table.floatType(64));
    CHECK(table.floatType(32) != table.floatType(64));
    CHECK(table.intType(64, true) != table.floatType(64));

    TypeId p1 = table.ptrType(table.intType(8, false));
    TypeId p2 = table.ptrType(table.intType(8, false));
    CHECK(p1 == p2);

    TypeId a1 = table.arrayType(table.intType(32, true), 4);
    TypeId a2 = table.arrayType(table.intType(32, true), 4);
    TypeId a3 = table.arrayType(table.intType(32, true), 8);
    CHECK(a1 == a2);
    CHECK(a1 != a3);

    TypeId f1 = table.fnType({table.intType(64, true)}, table.voidType(), false);
    TypeId f2 = table.fnType({table.intType(64, true)}, table.voidType(), false);
    TypeId f3 = table.fnType({table.intType(64, true)}, table.voidType(), true);
    CHECK(f1 == f2);
    CHECK(f1 != f3);
}

TEST_CASE("bool is Int{1, false}") {
    TypeTable table;
    CHECK(table.boolType() == table.intType(1, false));
    const Type &b = table.get(table.boolType());
    CHECK(b.kind == TypeKind::Int);
    CHECK(b.bits == 1);
    CHECK(b.isSigned == false);
}

TEST_CASE("voidType is stable and distinct from every other type") {
    TypeTable table;
    CHECK(table.voidType() == table.voidType());
    CHECK(table.voidType() != table.intType(1, false));
}

TEST_CASE("TargetLayout sizes come from the layout, not a hardcoded width") {
    TypeTable table;
    TargetLayout layout64;
    layout64.pointerBits = 64;
    layout64.sizeTypeBits = 64;

    TargetLayout layout32;
    layout32.pointerBits = 32;
    layout32.sizeTypeBits = 32;

    TypeId ptrToI8 = table.ptrType(table.intType(8, false));
    CHECK(layout64.sizeOfBytes(table, ptrToI8) == 8);
    CHECK(layout32.sizeOfBytes(table, ptrToI8) == 4);

    // A 1-bit bool still occupies a full byte in memory.
    CHECK(layout64.sizeOfBytes(table, table.boolType()) == 1);

    CHECK(layout64.sizeOfBytes(table, table.intType(64, true)) == 8);
    CHECK(layout64.sizeOfBytes(table, table.intType(32, true)) == 4);
    CHECK(layout64.sizeOfBytes(table, table.floatType(64)) == 8);
    CHECK(layout64.sizeOfBytes(table, table.floatType(32)) == 4);

    TypeId arr = table.arrayType(table.intType(32, true), 4);
    CHECK(layout64.sizeOfBytes(table, arr) == 16);
    CHECK(layout64.alignOfBytes(table, arr) == 4);
}
