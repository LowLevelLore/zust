#include <doctest/doctest.h>

#include "zir/DominatorTree.hpp"
#include "zir/TextParser.hpp"

using namespace zust::zir;

namespace {
    Module parseOrFail(const char *text) {
        std::string error;
        std::optional<Module> m = TextParser::parse(text, error);
        REQUIRE_MESSAGE(m.has_value(), "parse failed: " << error);
        return std::move(*m);
    }

    BlockId blockNamed(const Function &fn, const std::string &label) {
        for (std::size_t i = 0; i < fn.blockCount(); ++i) {
            BlockId id(static_cast<BlockId::Value>(i));
            if (fn.block(id).label() == label)
                return id;
        }
        FAIL("no block named " << label);
        return BlockId{};
    }
}  // namespace

TEST_CASE("DominatorTree on a diamond CFG: entry dominates everything, neither arm dominates the other") {
    const char *text = R"(module "diamond.zz" target = "generic"

fn @f(%v0: i64) -> i64 {
^entry(%v0: i64):
    %c0   = const i64 0
    %cmp  = icmp sgt i64 %v0, %c0
    condbr %cmp, ^then, ^else

^then:
    br ^merge

^else:
    br ^merge

^merge:
    ret i64 %v0
}
)";
    Module m = parseOrFail(text);
    const Function &fn = m.functions()[0];
    DominatorTree dt(fn);

    BlockId entry = blockNamed(fn, "entry");
    BlockId thenB = blockNamed(fn, "then");
    BlockId elseB = blockNamed(fn, "else");
    BlockId merge = blockNamed(fn, "merge");

    for (BlockId b : {entry, thenB, elseB, merge})
        CHECK(dt.isReachable(b));

    CHECK(dt.dominates(entry, thenB));
    CHECK(dt.dominates(entry, elseB));
    CHECK(dt.dominates(entry, merge));
    CHECK_FALSE(dt.dominates(thenB, elseB));
    CHECK_FALSE(dt.dominates(elseB, thenB));
    // merge has two predecessors, neither of which individually dominates
    // it -- entry is merge's immediate dominator, not then/else.
    CHECK_FALSE(dt.dominates(thenB, merge));
    CHECK_FALSE(dt.dominates(elseB, merge));
    CHECK(dt.immediateDominator(merge) == entry);
    CHECK(dt.immediateDominator(thenB) == entry);
    CHECK(dt.immediateDominator(elseB) == entry);

    // merge is a join point every branch of the diamond can reach without
    // passing through something merge itself dominates -- both arms should
    // carry it in their dominance frontier.
    auto hasBlock = [](const std::vector<BlockId> &v, BlockId b) {
        for (BlockId x : v)
            if (x == b)
                return true;
        return false;
    };
    CHECK(hasBlock(dt.dominanceFrontier(thenB), merge));
    CHECK(hasBlock(dt.dominanceFrontier(elseB), merge));
    CHECK(dt.dominanceFrontier(entry).empty());
}

TEST_CASE("DominatorTree on a loop CFG: the body does not dominate the header it branches back to") {
    const char *text = R"(module "loop.zz" target = "generic"

fn @f(%v0: i64) -> i64 {
^entry(%v0: i64):
    br ^cond

^cond:
    %c0   = const i64 0
    %cmp  = icmp sgt i64 %v0, %c0
    condbr %cmp, ^body, ^end

^body:
    br ^cond

^end:
    ret i64 %v0
}
)";
    Module m = parseOrFail(text);
    const Function &fn = m.functions()[0];
    DominatorTree dt(fn);

    BlockId entry = blockNamed(fn, "entry");
    BlockId cond = blockNamed(fn, "cond");
    BlockId body = blockNamed(fn, "body");
    BlockId end = blockNamed(fn, "end");

    CHECK(dt.dominates(entry, cond));
    CHECK(dt.dominates(cond, body));
    CHECK(dt.dominates(cond, end));
    // The loop header dominates the body, not the other way around -- the
    // back edge (body -> cond) must not make the analysis think body
    // dominates cond.
    CHECK_FALSE(dt.dominates(body, cond));
    CHECK(dt.immediateDominator(body) == cond);
    CHECK(dt.immediateDominator(end) == cond);

    // cond is its own loop header and sits in body's dominance frontier
    // (body's back edge reaches cond without passing through anything cond
    // itself dominates first).
    auto hasBlock = [](const std::vector<BlockId> &v, BlockId b) {
        for (BlockId x : v)
            if (x == b)
                return true;
        return false;
    };
    CHECK(hasBlock(dt.dominanceFrontier(body), cond));
}

TEST_CASE("DominatorTree marks unreachable blocks as unreachable and dominating nothing") {
    const char *text = R"(module "dead.zz" target = "generic"

fn @f() -> void {
^entry:
    ret void

^dead:
    ret void
}
)";
    Module m = parseOrFail(text);
    const Function &fn = m.functions()[0];
    DominatorTree dt(fn);

    BlockId entry = blockNamed(fn, "entry");
    BlockId dead = blockNamed(fn, "dead");

    CHECK(dt.isReachable(entry));
    CHECK_FALSE(dt.isReachable(dead));
    CHECK_FALSE(dt.dominates(entry, dead));
    CHECK_FALSE(dt.dominates(dead, entry));
    CHECK(dt.dominanceFrontier(dead).empty());
}
