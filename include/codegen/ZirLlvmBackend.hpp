#pragma once

#include <ostream>

#include "zir/Module.hpp"

// ZIR -> textual LLVM IR (docs/PRD-ZIR.md Wave 4.1). Wrapped by the
// registered "llvm-ir" `LlvmBackend` adapter in
// src/codegen/RegisterBackends.cpp, which lowers the AST it's handed
// through ZirGen and a Verifier check before calling this -- `Backend`
// itself still takes a whole AST (that interface migration is later Wave
// 4+/6 work); only what the llvm-ir adapter does with it changed in Wave
// 4.2 ("flip the default"), which deleted the AST-consuming `CodeGenLLVM`
// this replaced.
//
// Emits modern opaque-pointer IR ("ptr" everywhere, never a typed "i8*") --
// ZIR pointers are already opaque (zir::Types.hpp), so this is the natural
// translation, and it is what current LLVM (any version still parsing
// typed-pointer IR is long past due for one) actually wants.

namespace zust {

    class ZirLlvmBackend {
    public:
        static void emit(const zir::Module &m, std::ostream &out);
    };

}  // namespace zust
