#pragma once

#include "zir/Types.hpp"

// Numeric type promotion, ported bit-for-bit from
// TypeChecker::promoteType (include/typechecker/TypeChecker.hpp), over
// zir::TypeId instead of the legacy TypeInfo. The legacy function stays in
// place and in use by the legacy backends -- this is new, additive
// infrastructure for Wave 3 (ZIRGen), not a replacement yet. See
// docs/PRD-ZIR.md Wave 2.2.

namespace zust::zir {

    class TypeRules {
    public:
        // Throws if either operand is not Int or Float (matching the legacy
        // function's behavior for isString/isPointer/isUserDefined operands
        // -- there is no ZIR equivalent of "user-defined" yet, so anything
        // that isn't Int or Float is rejected the same way).
        static TypeId promote(const TypeTable &table, TypeId a, TypeId b);
    };

}  // namespace zust::zir
