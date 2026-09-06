#include "zir/passes/Pipeline.hpp"

#include "zir/passes/ConstFold.hpp"
#include "zir/passes/DCE.hpp"
#include "zir/passes/Mem2Reg.hpp"
#include "zir/passes/SimplifyCFG.hpp"

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

        return pm;
    }

}  // namespace zust::zir
