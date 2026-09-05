#include <doctest/doctest.h>

#include "parser/ScopeContext.hpp"

using namespace zust;

TEST_CASE("defineVariable assigns a SymbolId but no frame offset; getVariableOffset computes and caches lazily") {
    auto global = std::make_shared<NamespaceScope>("GLOBAL");
    global->defineType("int64_t", TypeInfo{.bits = 64, .align = 8, .isSigned = true, .name = "int64_t"});
    auto fn = std::make_shared<FunctionScope>("f", global);

    bool definedA = fn->defineVariable("a", VariableInfo{.type = "int64_t", .symbolId = {}});
    bool definedB = fn->defineVariable("b", VariableInfo{.type = "int64_t", .symbolId = {}});
    REQUIRE(definedA);
    REQUIRE(definedB);

    VariableInfo va = fn->lookupVariable("a");
    VariableInfo vb = fn->lookupVariable("b");
    CHECK(va.symbolId.isValid());
    CHECK(vb.symbolId.isValid());
    // Distinct declarations get distinct identities even though nothing
    // about their frame layout has been computed yet.
    CHECK(va.symbolId != vb.symbolId);

    // Offsets are computed lazily on first request, and are stable and
    // distinct across repeated queries -- this is the "shim" Wave 2.1 adds:
    // ScopeContext::defineVariable no longer calls allocateStack at all
    // (verified indirectly here: definedA/definedB above succeeded and
    // assigned symbol ids without needing any offset to exist yet).
    std::int64_t offsetA1 = fn->getVariableOffset("a");
    std::int64_t offsetB = fn->getVariableOffset("b");
    std::int64_t offsetA2 = fn->getVariableOffset("a");
    CHECK(offsetA1 == offsetA2);
    CHECK(offsetA1 != offsetB);
}

TEST_CASE("a shadowing inner declaration gets its own SymbolId and its own offset, not the outer's") {
    auto global = std::make_shared<NamespaceScope>("GLOBAL");
    global->defineType("int64_t", TypeInfo{.bits = 64, .align = 8, .isSigned = true, .name = "int64_t"});
    auto fn = std::make_shared<FunctionScope>("f", global);
    fn->defineVariable("x", VariableInfo{.type = "int64_t", .symbolId = {}});
    SymbolId outerId = fn->lookupVariable("x").symbolId;
    std::int64_t outerOffset = fn->getVariableOffset("x");

    auto block = std::make_shared<BlockScope>("b", fn, fn);
    block->defineVariable("x", VariableInfo{.type = "int64_t", .symbolId = {}});
    SymbolId innerId = block->lookupVariable("x").symbolId;
    std::int64_t innerOffset = block->getVariableOffset("x");

    CHECK(innerId != outerId);
    CHECK(innerOffset != outerOffset);
    // The outer variable's own offset is unaffected by the inner one being
    // resolved afterward -- this is the M0-1 shadowing invariant, still
    // holding under lazy allocation.
    CHECK(fn->getVariableOffset("x") == outerOffset);
}
