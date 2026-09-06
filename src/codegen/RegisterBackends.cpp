// The one file a new backend's registration touches outside its own
// directory (docs/BACKENDS.md §A.4). Each backend here is a thin adapter --
// Linux and Windows still wrap the legacy AST-consuming CodeGen* classes;
// llvm-ir was the first to move (docs/PRD-ZIR.md Wave 4.2) to a real
// ZIR-consuming backend, wrapping ZirGen + ZirLlvmBackend instead. Backend
// itself still takes the whole AST either way (that interface migration is
// later Wave 4+/6 work) -- only what each adapter does with it has changed.
#include <sstream>

#include "codegen/Backend.hpp"
#include "codegen/CodeGen.hpp"
#include "codegen/ZirLlvmBackend.hpp"
#include "codegen/machine/NativeBackend.hpp"
#include "codegen/machine/Win64Abi.hpp"
#include "zir/PassManager.hpp"
#include "zir/Verifier.hpp"
#include "zir/passes/Pipeline.hpp"
#include "zirgen/ZirGen.hpp"

namespace zust {
    namespace {

        // Shared by every ZIR-consuming backend below: lower, verify, run
        // the requested optimization pipeline, verify again. Throws (with a
        // message describing every failure) rather than returning a status,
        // matching how a Backend::emit() failure already propagates to
        // main.cpp's catch block.
        zir::Module lowerOptimizeVerify(const ASTNode &program, const std::string &sourceName, int optLevel) {
            ZirGen zirGen;
            zir::Module mod = zirGen.lower(program, sourceName);
            auto verifyOrThrow = [&] {
                std::vector<zir::VerifierFailure> failures = zir::Verifier::verify(mod);
                if (failures.empty())
                    return;
                std::ostringstream msg;
                for (const zir::VerifierFailure &f : failures) {
                    msg << "ZIR verification failed [" << zir::toString(f.check) << "] in @" << f.function << ": "
                        << f.detail << "\n";
                }
                throw std::runtime_error(msg.str());
            };
            verifyOrThrow();  // catches a ZirGen bug before it reaches optimization at all

            zir::AnalysisManager am;
            zir::PassManager pm = zir::buildPipeline(optLevel, mod);
            pm.run(mod, am);
            verifyOrThrow();  // catches an optimizer bug before it reaches codegen
            return mod;
        }

        class LinuxBackend final : public Backend {
        public:
            const TargetInfo &info() const override { return kInfo; }

            void emit(std::unique_ptr<ASTNode> program, std::ostream &out, int /*optLevel*/) override {
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

            // docs/PRD-ZIR.md Wave 6.2: at -O0, the exit criterion this
            // wave is actually stated at, this now runs the new
            // ZIR-consuming pipeline (X86InstSel -> LinearScan ->
            // FrameLayout -> AsmWriterIntel against Win64Abi). -O1 and
            // above still fall back to the legacy AST-consuming
            // CodeGenWindows -- correct (it never ran any ZIR pass either,
            // so "optimized" was already a no-op for every native target),
            // but not yet actually optimized -- until Wave 6.4 gives
            // LinearScan real whole-function liveness. This pipeline's own
            // LiveIntervals is deliberately scoped to block-local live
            // ranges (see LiveIntervals.hpp), which -O0 output always has
            // and -O1's mem2reg-introduced cross-block merges do not.
            void emit(std::unique_ptr<ASTNode> program, std::ostream &out, int optLevel) override {
                if (optLevel == 0) {
                    zir::Module mod = lowerOptimizeVerify(*program, "zust", optLevel);
                    codegen::machine::emitNative(mod, codegen::machine::win64Abi(), /*intelSyntax=*/true, out);
                    return;
                }
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

            // docs/PRD-ZIR.md Wave 4.2 "flip the default": this used to wrap
            // the AST-consuming CodeGenLLVM (deleted this wave, having lived
            // behind --zir-codegen since Wave 4.1 proved ZirLlvmBackend out).
            // The program is already fully type-checked by the time main.cpp
            // reaches here, same precondition CodeGenLLVM relied on.
            void emit(std::unique_ptr<ASTNode> program, std::ostream &out, int optLevel) override {
                zir::Module mod = lowerOptimizeVerify(*program, "zust", optLevel);
                ZirLlvmBackend::emit(mod, out);
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
