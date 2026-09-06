#pragma once

#include "zir/PassManager.hpp"

// docs/PRD-ZIR.md Wave 4.3/4.4/4.5 pipelines, matching docs/IR-DESIGN.md
// "Pass manager" exactly:
//   -O0: lowering + verifier only (empty pipeline -- PassManager::run on it
//        is a correctly-typed no-op).
//   -O1: mem2reg, constfold, simplifycfg, dce.
//   -O2: -O1 + sccp, gvn, instcombine, tailcall.
//   -O3: -O2 + inline, licm, loop unrolling.
// `level` outside [0, 3] is clamped to the nearest end.

namespace zust::zir {

    PassManager buildPipeline(int level, Module &m);

}  // namespace zust::zir
