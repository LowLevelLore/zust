// The one file a new backend's registration touches outside its own
// directory (docs/BACKENDS.md §A.4). Each backend here is a thin adapter over
// the existing AST-consuming CodeGen* classes -- Wave 4+ of docs/PRD-ZIR.md
// replaces these bodies with real ZIR-consuming backends one at a time
// without anything outside this file changing.
#include "codegen/Backend.hpp"
#include "codegen/CodeGen.hpp"

namespace zust {
    namespace {

        class LinuxBackend final : public Backend {
        public:
            const TargetInfo &info() const override { return kInfo; }

            void emit(std::unique_ptr<ASTNode> program, std::ostream &out) override {
                CodeGenLinux cg(out);
                cg.generate(std::move(program));
            }

            static const TargetInfo kInfo;
        };

        const TargetInfo LinuxBackend::kInfo{"x86_64-linux",
                                             "Linux x86-64, SysV ABI, GNU assembler (AT&T syntax)",
                                             ".s",
                                             AsmSyntax::ATT,
                                             /*isNative=*/true,
                                             {"as", "$IN", "-o", "$OUT"},
                                             {"gcc", "$IN", "-o", "$OUT"}};

        class WindowsBackend final : public Backend {
        public:
            const TargetInfo &info() const override { return kInfo; }

            void emit(std::unique_ptr<ASTNode> program, std::ostream &out) override {
                CodeGenWindows cg(out);
                cg.generate(std::move(program));
            }

            static const TargetInfo kInfo;
        };

        const TargetInfo WindowsBackend::kInfo{"x86_64-mswin",
                                               "Windows x86-64, Win64 ABI, MASM (Intel syntax)",
                                               ".asm",
                                               AsmSyntax::Intel,
                                               /*isNative=*/true,
                                               {"ml64", "/nologo", "/c", "$IN"},
                                               {"gcc", "$IN", "-o", "$OUT"}};

        class LlvmBackend final : public Backend {
        public:
            const TargetInfo &info() const override { return kInfo; }

            void emit(std::unique_ptr<ASTNode> program, std::ostream &out) override {
                CodeGenLLVM cg(out);
                cg.generate(std::move(program));
            }

            static const TargetInfo kInfo;
        };

        // `clang -c` rather than a standalone `llc -filetype=obj`: both
        // assemble textual IR to an object file identically (clang's codegen
        // path *is* llc's), but `clang` is the one binary guaranteed present
        // everywhere this target is exercised -- it's a listed CI dependency
        // already, and it is literally the only one of the two that the
        // official Windows LLVM distribution ships (no standalone
        // llc.exe/opt.exe at all).
        //
        // On Windows, clang's *implicit* default target is MSVC (its actual
        // "native" ABI there), which emits calls to MSVC's `__chkstk` for
        // any function with a large enough stack frame -- a symbol mingw's
        // libc/ld does not provide, so linking with `gcc`/mingw fails for
        // exactly those functions. `--target=x86_64-w64-mingw32` makes
        // clang target the same GNU/mingw environment `gcc` itself does
        // (GNU-style `___chkstk_ms` instead), which is what the link step
        // below actually needs. This is the one platform `#ifdef`
        // CONVENTIONS.md carves out for target selection (matching
        // `BackendRegistry::hostDefaultName`'s own) -- on Linux, clang's
        // default already matches the host, and there is no chkstk
        // convention to collide with in the first place.
#if defined(_WIN64)
        const std::vector<std::string> kLlvmAssembleCmd{"clang", "--target=x86_64-w64-mingw32", "-c", "$IN", "-o",
                                                         "$OUT"};
#else
        const std::vector<std::string> kLlvmAssembleCmd{"clang", "-c", "$IN", "-o", "$OUT"};
#endif

        const TargetInfo LlvmBackend::kInfo{"llvm-ir",
                                            "Textual LLVM IR",
                                            ".ll",
                                            AsmSyntax::None,
                                            /*isNative=*/false,
                                            kLlvmAssembleCmd,
                                            {"gcc", "$IN", "-o", "$OUT", "-no-pie"}};

    }  // namespace

    void registerBuiltinBackends(BackendRegistry &registry) {
        registry.registerBackend(LinuxBackend::kInfo, [] { return std::make_unique<LinuxBackend>(); });
        registry.registerBackend(WindowsBackend::kInfo, [] { return std::make_unique<WindowsBackend>(); });
        registry.registerBackend(LlvmBackend::kInfo, [] { return std::make_unique<LlvmBackend>(); });
    }

}  // namespace zust
