#pragma once

#include <cstdint>
#include <functional>

// A stable per-declaration identifier, assigned once when a variable or
// function is defined and never reused -- unlike a bare name string, which
// is not unique under shadowing (two different declarations can share a
// name in nested scopes). Exists so later stages (ZIRGen, Wave 3) have a
// name-independent key for "which declaration is this", and so frame-layout
// data (ScopeContext's offset table) can be keyed by declaration identity
// rather than by name.

namespace zust {

    struct SymbolId {
        static constexpr std::uint32_t kInvalid = 0xFFFFFFFFu;
        std::uint32_t value = kInvalid;

        constexpr bool isValid() const noexcept { return value != kInvalid; }
        constexpr bool operator==(const SymbolId &other) const noexcept { return value == other.value; }
        constexpr bool operator!=(const SymbolId &other) const noexcept { return value != other.value; }
    };

}  // namespace zust

template <>
struct std::hash<zust::SymbolId> {
    std::size_t operator()(const zust::SymbolId &id) const noexcept {
        return std::hash<std::uint32_t>{}(id.value);
    }
};
