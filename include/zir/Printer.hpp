#pragma once

#include <ostream>
#include <string>

#include "zir/Module.hpp"

// Textual form per docs/IR-DESIGN.md "Textual form" -- round-trippable
// (Printer -> TextParser -> Printer is a fixed point, tested under
// tests/zir/roundtrip/), which is what makes every later pass unit-testable
// from a .zir file instead of driving the whole frontend.

namespace zust::zir {

    class Printer {
    public:
        static void print(const Module &m, std::ostream &out);
        static std::string print(const Module &m);

        // Exposed individually because the verifier's diagnostics print a
        // single offending function/type, not a whole module.
        static std::string printType(const TypeTable &table, TypeId id);
    };

}  // namespace zust::zir
