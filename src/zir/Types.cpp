#include "zir/Types.hpp"

#include <stdexcept>

namespace zust::zir {

    bool Type::operator==(const Type &other) const {
        if (kind != other.kind)
            return false;
        switch (kind) {
            case TypeKind::Void:
                return true;
            case TypeKind::Int:
                return bits == other.bits && isSigned == other.isSigned;
            case TypeKind::Float:
                return bits == other.bits;
            case TypeKind::Ptr:
                return pointee == other.pointee;
            case TypeKind::Array:
                return elem == other.elem && arrayLen == other.arrayLen;
            case TypeKind::Fn:
                return ret == other.ret && variadic == other.variadic && params == other.params;
        }
        return false;
    }

    TypeTable::TypeTable() {
        Type v;
        v.kind = TypeKind::Void;
        voidType_ = intern(v);
    }

    TypeId TypeTable::intern(Type t) {
        for (std::size_t i = 0; i < types_.size(); ++i) {
            if (types_[i] == t)
                return TypeId(static_cast<TypeId::Value>(i));
        }
        types_.push_back(std::move(t));
        return TypeId(static_cast<TypeId::Value>(types_.size() - 1));
    }

    TypeId TypeTable::intType(std::uint32_t bits, bool isSigned) {
        Type t;
        t.kind = TypeKind::Int;
        t.bits = bits;
        t.isSigned = isSigned;
        return intern(t);
    }

    TypeId TypeTable::floatType(std::uint32_t bits) {
        Type t;
        t.kind = TypeKind::Float;
        t.bits = bits;
        return intern(t);
    }

    TypeId TypeTable::ptrType(TypeId pointee) {
        Type t;
        t.kind = TypeKind::Ptr;
        t.pointee = pointee;
        return intern(t);
    }

    TypeId TypeTable::arrayType(TypeId elem, std::uint64_t len) {
        Type t;
        t.kind = TypeKind::Array;
        t.elem = elem;
        t.arrayLen = len;
        return intern(t);
    }

    TypeId TypeTable::fnType(std::vector<TypeId> params, TypeId ret, bool variadic) {
        Type t;
        t.kind = TypeKind::Fn;
        t.params = std::move(params);
        t.ret = ret;
        t.variadic = variadic;
        return intern(t);
    }

    const Type &TypeTable::get(TypeId id) const {
        if (!id.isValid() || id.value() >= types_.size())
            throw std::runtime_error("TypeTable::get: invalid TypeId");
        return types_[id.value()];
    }

    std::uint32_t TargetLayout::sizeOfBytes(const TypeTable &table, TypeId id) const {
        const Type &t = table.get(id);
        switch (t.kind) {
            case TypeKind::Void:
                return 0;
            case TypeKind::Int:
                // Round up: a 1-bit ZIR bool is still a full byte in memory.
                return (t.bits + 7) / 8;
            case TypeKind::Float:
                return t.bits / 8;
            case TypeKind::Ptr:
                return pointerBits / 8;
            case TypeKind::Array:
                return sizeOfBytes(table, t.elem) * static_cast<std::uint32_t>(t.arrayLen);
            case TypeKind::Fn:
                throw std::runtime_error("TargetLayout::sizeOfBytes: function types have no size");
        }
        throw std::runtime_error("TargetLayout::sizeOfBytes: unknown TypeKind");
    }

    std::uint32_t TargetLayout::alignOfBytes(const TypeTable &table, TypeId id) const {
        const Type &t = table.get(id);
        switch (t.kind) {
            case TypeKind::Void:
                return 1;
            case TypeKind::Int:
                return (t.bits + 7) / 8;
            case TypeKind::Float:
                return t.bits / 8;
            case TypeKind::Ptr:
                return pointerBits / 8;
            case TypeKind::Array:
                return alignOfBytes(table, t.elem);
            case TypeKind::Fn:
                throw std::runtime_error("TargetLayout::alignOfBytes: function types have no alignment");
        }
        throw std::runtime_error("TargetLayout::alignOfBytes: unknown TypeKind");
    }

}  // namespace zust::zir
