#pragma once

#include <cstdint>
#include <functional>
#include <memory>
#include <ostream>
#include <string>
#include <string_view>
#include <vector>

#include "ast/ASTNode.hpp"

namespace zust {

    enum class AsmSyntax : std::uint8_t { ATT, Intel, None };

    // Everything the driver and the test harness need to know about a target,
    // without either of them special-casing its name. Adding a target means
    // adding one TargetInfo + one Backend and registering it in
    // registerBuiltinBackends() -- nothing else changes.
    struct TargetInfo {
        std::string name;         // "x86_64-linux" -- also the --format value
        std::string description;  // shown by --formats
        std::string asmExt;       // ".s" / ".asm" / ".ll"
        AsmSyntax syntax = AsmSyntax::ATT;
        bool isNative = true;  // native assembly vs. IR for another toolchain (e.g. LLVM IR)

        // How to turn the emitted file into an object, then an executable.
        // "$IN"/"$OUT" are placeholders the caller substitutes. Consumed by
        // both the driver's future self-hosted build mode and the pytest
        // harness, so the two can never disagree about how to build a target.
        std::vector<std::string> assembleCmd;  // e.g. {"as", "$IN", "-o", "$OUT"}
        std::vector<std::string> linkCmd;      // e.g. {"gcc", "$IN", "-o", "$OUT"}
    };

    // Phase A (docs/BACKENDS.md): still consumes the whole AST, since ZIR does
    // not exist yet. Phase B (docs/PRD-ZIR.md Wave 4+) moves this to
    // `emit(const zir::Module&, ostream&)` once backends are ported off the
    // AST -- taking the module by const reference, rather than consuming it
    // the way this signature does, is what lets one run emit several targets
    // and a backend make more than one pass over the same module.
    class Backend {
    public:
        virtual ~Backend() = default;
        virtual const TargetInfo &info() const = 0;
        // `optLevel` is 0-3 (docs/PRD-ZIR.md Wave 4.3's -O0..-O3). A backend
        // that has no optimization pipeline of its own yet (Linux, Windows
        // -- Wave 5/6 territory) is free to ignore it; llvm-ir's is the
        // first to actually run one (Wave 4.4).
        virtual void emit(std::unique_ptr<ASTNode> program, std::ostream &out, int optLevel) = 0;
    };

    class BackendRegistry {
    public:
        using Factory = std::function<std::unique_ptr<Backend>()>;

        static BackendRegistry &instance();

        // Not copyable/movable -- there is exactly one registry.
        BackendRegistry(const BackendRegistry &) = delete;
        BackendRegistry &operator=(const BackendRegistry &) = delete;

        void registerBackend(TargetInfo info, Factory factory);

        // nullptr when unknown -- the caller turns that into a diagnostic
        // that lists the available names, so a typo is self-correcting.
        std::unique_ptr<Backend> create(std::string_view name) const;

        const TargetInfo *find(std::string_view name) const;
        std::vector<const TargetInfo *> list() const;

        // Resolves what "-f default" (or no -f at all) means on this host.
        // The one place allowed to look at the platform, matching the single
        // #ifdef exception CONVENTIONS.md carves out for target selection.
        static std::string hostDefaultName();

        void printFormats(std::ostream &out) const;
        void printFormatsJson(std::ostream &out) const;

    private:
        BackendRegistry() = default;

        std::vector<TargetInfo> infos_;
        std::vector<Factory> factories_;
    };

    // Explicit registration, not static-initializer self-registration: this
    // is a static library, and the linker drops object files nothing
    // references, so a self-registering backend would silently vanish from a
    // release build. This is the one function a new backend's registration
    // touches outside its own directory.
    void registerBuiltinBackends(BackendRegistry &registry);

}  // namespace zust
