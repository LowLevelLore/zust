#pragma once

#include <cstdint>
#include <functional>
#include <limits>

// Strong, arena-index id types for ZIR (docs/IR-DESIGN.md). Each is a plain
// uint32 wrapper -- no pointer chasing, cheap to copy, cheap to hash, stable
// across the owning vector's growth (unlike a pointer/iterator, which a
// reallocation invalidates). kInvalid is a sentinel, not zero, so a
// default-constructed id is detectably unset rather than silently aliasing
// index 0.

namespace zust::zir {

    template <typename Tag>
    class Id {
    public:
        using Value = std::uint32_t;
        static constexpr Value kInvalidValue = std::numeric_limits<Value>::max();

        constexpr Id() noexcept : value_(kInvalidValue) {}

        constexpr explicit Id(Value v) noexcept : value_(v) {}

        constexpr Value value() const noexcept { return value_; }

        constexpr bool isValid() const noexcept { return value_ != kInvalidValue; }

        constexpr bool operator==(const Id &other) const noexcept { return value_ == other.value_; }

        constexpr bool operator!=(const Id &other) const noexcept { return value_ != other.value_; }

        constexpr bool operator<(const Id &other) const noexcept { return value_ < other.value_; }

    private:
        Value value_;
    };

    namespace detail {
        struct TypeIdTag {};

        struct ValueIdTag {};

        struct BlockIdTag {};

        struct InstIdTag {};

        struct FuncIdTag {};

        struct GlobalIdTag {};
    }  // namespace detail

    using TypeId = Id<detail::TypeIdTag>;
    using ValueId = Id<detail::ValueIdTag>;
    using BlockId = Id<detail::BlockIdTag>;
    using InstId = Id<detail::InstIdTag>;
    using FuncId = Id<detail::FuncIdTag>;
    using GlobalId = Id<detail::GlobalIdTag>;

}  // namespace zust::zir

template <typename Tag>
struct std::hash<zust::zir::Id<Tag>> {
    std::size_t operator()(const zust::zir::Id<Tag> &id) const noexcept {
        return std::hash<typename zust::zir::Id<Tag>::Value>{}(id.value());
    }
};
