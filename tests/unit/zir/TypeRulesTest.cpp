#include <doctest/doctest.h>

#include <string>
#include <vector>

#include "typechecker/TypeChecker.hpp"
#include "zir/TypeRules.hpp"

using namespace zust;
using namespace zust::zir;

namespace {
    // The 12 numeric legacy types, with the exact bits/align/isSigned values
    // Parser.cpp's constructor registers them with (src/parser/Parser.cpp) --
    // this is the "13x13" (in practice: the numeric_types set has 12 members)
    // grid docs/PRD-ZIR.md Wave 2.2 asks for.
    struct LegacyEntry {
        std::string name;
        TypeInfo info;
    };

    std::vector<LegacyEntry> legacyNumericTypes() {
        auto mk = [](std::string name, std::uint32_t bits, bool isFloat, bool isSigned) {
            TypeInfo t;
            t.bits = bits;
            t.align = bits / 8;
            t.isFloat = isFloat;
            t.isSigned = isSigned;
            t.name = name;
            return LegacyEntry{name, t};
        };
        return {
            mk("integer", 64, false, true),  mk("size_t", 64, false, false),  mk("uint8_t", 8, false, false),
            mk("uint16_t", 16, false, false), mk("uint32_t", 32, false, false), mk("uint64_t", 64, false, false),
            mk("int8_t", 8, false, true),    mk("int16_t", 16, false, true),  mk("int32_t", 32, false, true),
            mk("int64_t", 64, false, true),  mk("float", 32, true, true),     mk("double", 64, true, true),
        };
    }

    TypeId zirEquivalent(TypeTable &table, const TypeInfo &info) {
        if (info.isFloat)
            return table.floatType(info.bits);
        return table.intType(info.bits, info.isSigned);
    }
}  // namespace

TEST_CASE("TypeRules::promote matches TypeChecker::promoteType across every numeric type pair") {
    std::vector<LegacyEntry> legacy = legacyNumericTypes();
    TypeTable table;

    int pairsChecked = 0;
    for (const LegacyEntry &lhs : legacy) {
        for (const LegacyEntry &rhs : legacy) {
            CAPTURE(lhs.name);
            CAPTURE(rhs.name);

            TypeInfo oldResult = TypeChecker::promoteType(lhs.info, rhs.info);

            TypeId newLhs = zirEquivalent(table, lhs.info);
            TypeId newRhs = zirEquivalent(table, rhs.info);
            TypeId newResultId = TypeRules::promote(table, newLhs, newRhs);
            const Type &newResult = table.get(newResultId);

            CHECK(newResult.kind == (oldResult.isFloat ? TypeKind::Float : TypeKind::Int));
            CHECK(newResult.bits == oldResult.bits);
            if (!oldResult.isFloat) {
                CHECK(newResult.isSigned == oldResult.isSigned);
            }
            pairsChecked++;
        }
    }
    CHECK(pairsChecked == 144);  // 12 x 12
}
