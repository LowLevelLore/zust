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

        const TargetInfo LlvmBackend::kInfo{"llvm-ir",
                                            "Textual LLVM IR",
                                            ".ll",
                                            AsmSyntax::None,
                                            /*isNative=*/false,
                                            {"llc", "-filetype=obj", "$IN", "-o", "$OUT"},
                                            {"gcc", "$IN", "-o", "$OUT", "-no-pie"}};

    }  // namespace

    void registerBuiltinBackends(BackendRegistry &registry) {
        registry.registerBackend(LinuxBackend::kInfo, [] { return std::make_unique<LinuxBackend>(); });
        registry.registerBackend(WindowsBackend::kInfo, [] { return std::make_unique<WindowsBackend>(); });
        registry.registerBackend(LlvmBackend::kInfo, [] { return std::make_unique<LlvmBackend>(); });
    }

}  // namespace zust
