#pragma once

#include <cstdint>
#include <string>
#include <vector>

#include "zir/Module.hpp"

// The 8 checks from docs/IR-DESIGN.md "Verifier". Meant to run after
// lowering and after every pass under -DZUST_ASSERTIONS; a verifier failure
// is a compiler bug (the caller decides how loudly to report it -- this
// class only finds and describes failures, it never throws or aborts
// itself).
//
// Two of the spec's eight checks turned out to be structurally unviolable
// given this implementation's data model, and are documented as such at
// their check functions in Verifier.cpp rather than silently reinterpreted:
// a BasicBlock has exactly one Terminator field (not a list), so "exactly
// one terminator, nowhere else" cannot fail by construction; this
// implementation instead checks that whichever terminator is present is
// internally well-formed for its kind, including that every block it
// references is valid. See Verifier.cpp for the reasoning behind each
// check's exact semantics, especially checks 1 and 5.

namespace zust::zir {

    enum class VerifierCheck : std::uint8_t {
        Terminator,       // 1: the block's terminator is well-formed for its kind
        Dominance,        // 2: every use is dominated by its definition
        OperandTypes,     // 3: operand types match the instruction's signature exactly
        BranchArgs,       // 4: branch argument lists match the target block's params
        Predecessors,     // 5: no block is reachable only from outside the entry-rooted graph
        AllocaPlacement,  // 6: alloca appears only in the entry block
        SingleDef,        // 7: every ValueId is defined exactly once
        ReturnType,       // 8: every `ret` matches the function's declared return type
    };

    const char *toString(VerifierCheck check);

    struct VerifierFailure {
        VerifierCheck check;
        std::string function;  // function name, for a readable message
        std::string detail;
    };

    class Verifier {
    public:
        static std::vector<VerifierFailure> verify(const Module &m);
    };

}  // namespace zust::zir
