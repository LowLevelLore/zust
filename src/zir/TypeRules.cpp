#include "zir/TypeRules.hpp"

#include <stdexcept>

namespace zust::zir {

    TypeId TypeRules::promote(const TypeTable &table, TypeId a, TypeId b) {
        const Type &ta = table.get(a);
        const Type &tb = table.get(b);

        auto isNumeric = [](const Type &t) { return t.kind == TypeKind::Int || t.kind == TypeKind::Float; };
        if (!isNumeric(ta) || !isNumeric(tb)) {
            throw std::runtime_error("Invalid type promotion: operands must both be numeric (Int or Float)");
        }

        if (ta.kind == TypeKind::Float || tb.kind == TypeKind::Float) {
            if (ta.kind == TypeKind::Float && tb.kind == TypeKind::Float) {
                return ta.bits > tb.bits ? a : b;
            }
            if (ta.kind == TypeKind::Float) {
                // int wider than float promotes to a float of the *int's*
                // width, not the float's -- e.g. float32 + int64 -> f64.
                if (tb.bits > ta.bits)
                    return table.floatType(tb.bits);
                return a;
            } else {
                if (ta.bits > tb.bits)
                    return table.floatType(ta.bits);
                return b;
            }
        }

        if (ta.bits != tb.bits)
            return ta.bits > tb.bits ? a : b;

        if (ta.isSigned && !tb.isSigned)
            return a;
        if (!ta.isSigned && tb.isSigned)
            return b;

        return a;
    }

}  // namespace zust::zir
