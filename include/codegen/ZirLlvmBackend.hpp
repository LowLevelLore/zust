#pragma once

#include <ostream>

#include "zir/Module.hpp"

// ZIR -> textual LLVM IR (docs/PRD-ZIR.md Wave 4.1). Distinct from the
// legacy, AST-consuming `LlvmBackend` registered in
// src/codegen/RegisterBackends.cpp -- this one is not registered in the
// BackendRegistry yet (that interface still takes a whole AST; Wave 4.2's
// "flip the default" is what migrates it). For now this is a parallel path
// main.cpp reaches only behind `--zir-codegen`, matching the PRD's "first
// consumer... behind --zir-codegen" framing exactly: it exists to prove ZIR
// out end to end, not to replace the registered backend yet.
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
