#include "zir/passes/Pipeline.hpp"

#include "zir/passes/ConstFold.hpp"
#include "zir/passes/DCE.hpp"
#include "zir/passes/GVN.hpp"
#include "zir/passes/Inline.hpp"
#include "zir/passes/InstCombine.hpp"
#include "zir/passes/LICM.hpp"
#include "zir/passes/LoopUnroll.hpp"
#include "zir/passes/Mem2Reg.hpp"
#include "zir/passes/SCCP.hpp"
#include "zir/passes/SimplifyCFG.hpp"
#include "zir/passes/TailCall.hpp"

namespace zust::zir {

    PassManager buildPipeline(int level, Module &m) {
        if (level < 0)
            level = 0;
        if (level > 3)
            level = 3;

        PassManager pm;
        if (level == 0)
            return pm;

        // -O1: mem2reg, constfold, simplifycfg, dce -- iterated together to
        // a fixpoint by PassManager itself. Each round after mem2reg tends
        // to expose more constant folding, which tends to expose more
        // foldable branches, which tends to expose more dead code, which
        // tends to expose more blocks simplifycfg can merge; PassManager's
        // per-function fixpoint loop is what makes that chain actually run
        // to completion instead of needing a fixed number of hand-tuned
        // rounds.
        pm.addFunctionPass(std::make_unique<Mem2RegPass>());
        pm.addFunctionPass(std::make_unique<ConstFoldPass>(m));
        pm.addFunctionPass(std::make_unique<SimplifyCFGPass>());
        pm.addFunctionPass(std::make_unique<DCEPass>());

        if (level == 1)
            return pm;

        // -O2: -O1 plus sccp, gvn, instcombine, tailcall. All in the same
        // PassManager, so they iterate to a fixpoint together with the -O1
        // set rather than as a separate bolted-on "second round" -- each of
        // these can expose more mem2reg/constfold/simplifycfg/dce work
        // (instcombine turning `x*1` into `x` can feed GVN a match it
        // wouldn't otherwise have had, etc.), and the fixpoint loop is what
        // actually chases that down instead of a fixed pass ordering.
        pm.addFunctionPass(std::make_unique<SCCPPass>());
        pm.addFunctionPass(std::make_unique<GVNPass>());
        pm.addFunctionPass(std::make_unique<InstCombinePass>());
        pm.addFunctionPass(std::make_unique<TailCallPass>(m));

        if (level == 2)
            return pm;

        // -O3: -O2 plus inline, licm, loop unrolling. Inlining is the one
        // ModulePass in the whole pipeline (docs/IR-DESIGN.md's own example
        // of something that needs whole-module knowledge); it runs in the
        // same fixpoint as everything else, so a function inlining exposes
        // new constant-folding/mem2reg/LICM opportunities in its caller
        // just like every other pass interaction here.
        pm.addModulePass(std::make_unique<InlinePass>());
        pm.addFunctionPass(std::make_unique<LICMPass>());
        pm.addFunctionPass(std::make_unique<LoopUnrollPass>(m));

        return pm;
    }

}  // namespace zust::zir
