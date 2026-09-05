#pragma once

#include <optional>
#include <string>
#include <string_view>

#include "zir/Module.hpp"

// Reads the textual form Printer writes, back into a Module. This is what
// makes ZIR passes unit-testable from a `.zir` file under tests/zir/<pass>/
// without driving the whole frontend (docs/IR-DESIGN.md "Textual form").
//
// Whitespace, comments, and the printer's fixed-width "=" column alignment
// carry no meaning and are not preserved -- only round-trip *content* is
// guaranteed: Printer::print(*TextParser::parse(Printer::print(m))) equals
// Printer::print(m), not necessarily byte-for-byte with an arbitrary
// hand-written `.zir` file's whitespace.

namespace zust::zir {

    class TextParser {
    public:
        // Returns nullopt and writes a message to `error` on a malformed
        // document. Never throws for malformed input -- this is a frontend,
        // not an internal invariant (CONVENTIONS.md: the compiler never
        // crashes on bad input).
        static std::optional<Module> parse(std::string_view text, std::string &error);
    };

}  // namespace zust::zir
