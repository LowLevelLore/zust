#pragma once

#include <cstdint>

namespace zust {

    // A source position, carried by Token and ASTNode so later stages (ZIR
    // lowering, and eventually real diagnostics -- ROADMAP M1) have somewhere
    // to point back into the source without re-deriving it. Deliberately
    // minimal for now: a single line/column point, not a start/end byte
    // range -- the lexer does not buffer source offsets today, and widening
    // this to a real range is M1's job, not this one's. Adding it as its own
    // type now (rather than every caller passing two raw ints) is what keeps
    // that upgrade a one-file change later.
    struct Span {
        std::uint32_t line = 0;
        std::uint32_t column = 0;
    };

}  // namespace zust
