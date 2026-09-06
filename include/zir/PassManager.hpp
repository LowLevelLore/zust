#pragma once

#include <memory>
#include <unordered_map>
#include <vector>

#include "zir/DominatorTree.hpp"
#include "zir/Module.hpp"

// docs/IR-DESIGN.md "Pass manager" / docs/PRD-ZIR.md Wave 4.3.
//
//   class Pass       { virtual bool run(Function&, AnalysisManager&) = 0; };
//   class ModulePass { virtual bool run(Module&,   AnalysisManager&) = 0; };
//
// `run` returns whether it changed anything, which is what drives fixpoint
// iteration -- PassManager keeps re-running a function's pass list until a
// full pass over it makes no further change. A pass that always returns
// `true` hangs the compiler, deliberately: that is a bug in the pass, not
// something the manager should paper over by silently capping iterations.

namespace zust::zir {

    class Pass {
    public:
        virtual ~Pass() = default;
        virtual const char *name() const = 0;
        virtual bool run(Function &fn, class AnalysisManager &am) = 0;
    };

    class ModulePass {
    public:
        virtual ~ModulePass() = default;
        virtual const char *name() const = 0;
        virtual bool run(Module &m, class AnalysisManager &am) = 0;
    };

    // Caches per-Function analyses so passes don't each recompute their own
    // DominatorTree. Deliberately coarse invalidation: PassManager
    // invalidates a function's whole entry the moment any pass reports a
    // change to it, rather than each pass declaring which analyses it
    // preserves -- simpler, and recomputing a DominatorTree is cheap next to
    // actually running a pass.
    class AnalysisManager {
    public:
        const DominatorTree &dominatorTree(const Function &fn) {
            auto it = domTrees_.find(&fn);
            if (it != domTrees_.end())
                return *it->second;
            auto [inserted, _] = domTrees_.emplace(&fn, std::make_unique<DominatorTree>(fn));
            return *inserted->second;
        }

        void invalidate(const Function &fn) { domTrees_.erase(&fn); }

        void invalidateAll() { domTrees_.clear(); }

    private:
        std::unordered_map<const Function *, std::unique_ptr<DominatorTree>> domTrees_;
    };

    // One optimization level's pipeline: an ordered list of function passes
    // (run per function, to a per-function fixpoint) plus module passes
    // (need whole-module knowledge -- e.g. a future inliner's call graph;
    // run once per pipeline round, module-wide). PassManager owns its
    // passes.
    class PassManager {
    public:
        void addFunctionPass(std::unique_ptr<Pass> p) { functionPasses_.push_back(std::move(p)); }

        void addModulePass(std::unique_ptr<ModulePass> p) { modulePasses_.push_back(std::move(p)); }

        // Runs every module pass, then every function's function-pass list
        // to a per-function fixpoint, and repeats the whole thing until one
        // full round makes no change anywhere -- so a module pass (e.g.
        // inlining) that exposes new opportunities in a function already
        // "done" gets picked back up.
        void run(Module &m, AnalysisManager &am) {
            bool changedAnything = true;
            while (changedAnything) {
                changedAnything = false;
                for (auto &mp : modulePasses_) {
                    if (mp->run(m, am)) {
                        changedAnything = true;
                        am.invalidateAll();
                    }
                }
                for (std::size_t i = 0; i < m.functions().size(); ++i) {
                    FuncId id(static_cast<FuncId::Value>(i));
                    Function &fn = m.function(id);
                    if (fn.isExtern())
                        continue;
                    bool fnChanged = true;
                    while (fnChanged) {
                        fnChanged = false;
                        for (auto &fp : functionPasses_) {
                            if (fp->run(fn, am)) {
                                fnChanged = true;
                                changedAnything = true;
                                am.invalidate(fn);
                            }
                        }
                    }
                }
            }
        }

    private:
        std::vector<std::unique_ptr<Pass>> functionPasses_;
        std::vector<std::unique_ptr<ModulePass>> modulePasses_;
    };

}  // namespace zust::zir
