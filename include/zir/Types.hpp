#pragma once

#include <cstdint>
#include <vector>

#include "zir/Ids.hpp"

// The ZIR type system (docs/IR-DESIGN.md "Type system"). Interned: identical
// structural types get the same TypeId, so type equality is an integer
// compare. Sizes and alignment are NOT part of Type -- they come from
// TargetLayout, which is target-dependent; the legacy TypeInfo hardcoding
// size_t at 64 bits is exactly the mistake this split exists to avoid.

namespace zust::zir {

    enum class TypeKind : std::uint8_t {
        Void,
        Int,
        Float,
        Ptr,
        Array,
        Fn,
    };

    // A flat struct rather than a tagged union/variant: only the fields for
    // the active `kind` are meaningful, the rest sit at their default. This
    // mirrors the shape docs/IR-DESIGN.md gives in its pseudocode and keeps
    // interning (structural equality + hashing) simple -- one operator==,
    // not one per alternative.
    struct Type {
        TypeKind kind = TypeKind::Void;

        // Int
        std::uint32_t bits = 0;
        bool isSigned = false;

        // Ptr
        TypeId pointee;

        // Array
        TypeId elem;
        std::uint64_t arrayLen = 0;

        // Fn
        std::vector<TypeId> params;
        TypeId ret;
        bool variadic = false;

        bool operator==(const Type &other) const;

        bool operator!=(const Type &other) const { return !(*this == other); }
    };

    // Interning table. Every zir::Module owns exactly one; TypeId is only
    // meaningful relative to the table that produced it.
    //
    // Interning const-qualified: asking for a type is logically a pure
    // lookup (the same request always yields the same TypeId) even though
    // it may need to insert on first use, so callers holding only a `const
    // TypeTable&` -- the Verifier, notably, which only ever observes a
    // Module -- can still ask for "the bool type" or "an i32" without a
    // mutable reference. `types_` is `mutable` to make that legal.
    class TypeTable {
    public:
        TypeTable();

        TypeId voidType() const { return voidType_; }

        TypeId intType(std::uint32_t bits, bool isSigned) const;

        // bool == Int{1, false} in ZIR proper; lowering/backends decide how
        // that 1-bit value is represented in memory (TargetLayout rounds it
        // up to 1 byte -- see sizeOfBytes).
        TypeId boolType() const { return intType(1, false); }

        TypeId floatType(std::uint32_t bits) const;
        TypeId ptrType(TypeId pointee) const;
        TypeId arrayType(TypeId elem, std::uint64_t len) const;
        TypeId fnType(std::vector<TypeId> params, TypeId ret, bool variadic) const;

        const Type &get(TypeId id) const;

        std::size_t size() const { return types_.size(); }

    private:
        TypeId intern(Type t) const;

        mutable std::vector<Type> types_;
        TypeId voidType_;
    };

    // Everything about a type that depends on the target rather than the
    // type itself. A backend/lowering pass asks this, never the Type struct.
    struct TargetLayout {
        std::uint32_t pointerBits = 64;
        std::uint32_t sizeTypeBits = 64;  // width of the `size_t`-equivalent surface type

        std::uint32_t sizeOfBytes(const TypeTable &table, TypeId id) const;
        std::uint32_t alignOfBytes(const TypeTable &table, TypeId id) const;
    };

}  // namespace zust::zir
