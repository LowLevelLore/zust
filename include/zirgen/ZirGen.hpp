#pragma once

#include <string>
#include <unordered_map>
#include <vector>

#include "ast/ASTNode.hpp"
#include "parser/SymbolId.hpp"
#include "zir/Builder.hpp"
#include "zir/Module.hpp"

// AST -> ZIR lowering (docs/PRD-ZIR.md Wave 3). Consumes a fully parsed and
// type-checked Program node (does NOT take ownership -- unlike the legacy
// AST-walking backends, this never mutates or destroys the tree, so it can
// run alongside them without disturbing the existing pipeline) and produces
// a zir::Module.
//
// Locals become alloca/load/store in the entry block, uniformly, regardless
// of nesting depth -- "no cleverness" (docs/IR-DESIGN.md); mem2reg promotes
// the ones that qualify later (Wave 4). Every behavior rule this has to
// reproduce exactly is listed in docs/PRD-ZIR.md's "Behavior inventory".

namespace zust {

    class ZirGen {
    public:
        zir::Module lower(const ASTNode &program, const std::string &sourceName);

    private:
        struct LoopFrame {
            zir::BlockId continueTarget;
            zir::BlockId breakTarget;
        };

        // ---- per-module state ----
        zir::Module *module_ = nullptr;
        std::shared_ptr<ScopeContext> globalScope_;  // for legacy TypeInfo lookups in zirType()
        std::unordered_map<std::string, zir::TypeId> typeCache_;
        std::unordered_map<std::string, zir::FuncId> funcsByName_;
        std::unordered_map<SymbolId, zir::GlobalId> globalVars_;
        std::vector<const ASTNode *> hoistedTopLevel_;  // VariableDeclaration/Reassignment/++|-- at top level, in order

        // ---- per-function state (reset at the start of lowerFunctionBody) ----
        zir::Function *fn_ = nullptr;
        zir::Builder *builder_ = nullptr;
        std::unordered_map<SymbolId, zir::ValueId> localAllocas_;
        std::vector<LoopFrame> loopStack_;
        std::string currentReturnType_;
        // `@main`'s language-level return type is always "none" (the source
        // never writes `-> int32_t` on it), but every backend still emits a
        // real `ret i32 0` for the process exit code -- ZIR's `@main`
        // reflects that ABI-level truth in its signature rather than "none",
        // so this is the type every `ret` in `main` actually has to match,
        // distinct from currentReturnType_'s language-level "none"
        // (docs/PRD-ZIR.md behavior inventory, "Global hoisting" rule 4).
        zir::TypeId currentZirReturnType_;
        bool isMainFunction_ = false;
        int blockCounter_ = 0;
        // Whether the *current* insert block has already been given a real
        // terminator during lowering. Distinct from BasicBlock::term()'s own
        // state, which always holds *something* (defaulting to Unreachable)
        // -- this flag is what lets lowering tell "genuinely unreachable, by
        // this wave's deliberate choice" apart from "nothing's terminated
        // this block yet, keep appending".
        bool blockTerminated_ = false;

        // ---- top-level passes ----
        void registerGlobal(const ASTNode *decl);
        void registerFunctionSignature(const ASTNode *fnOrExtern);
        void lowerFunctionBody(const ASTNode *fnNode);

        // ---- declaration collection ----
        void collectDeclarations(const ASTNode *node, std::vector<const ASTNode *> &out);

        // ---- statements ----
        void lowerStatement(const ASTNode *node);
        void lowerVarDecl(const ASTNode *node);
        void lowerVarReassign(const ASTNode *node);
        void lowerIf(const ASTNode *node);
        void lowerFor(const ASTNode *node);
        void lowerWhile(const ASTNode *node);
        void lowerReturn(const ASTNode *node);
        void lowerBreak();
        void lowerContinue();

        // ---- expressions ----
        zir::ValueId lowerExpression(const ASTNode *node);
        zir::ValueId lowerVariableAccess(const ASTNode *node);
        zir::ValueId lowerBinaryOp(const ASTNode *node);
        zir::ValueId lowerUnaryOp(const ASTNode *node);
        zir::ValueId lowerFunctionCall(const ASTNode *node);
        zir::ValueId lowerStringLiteral(const ASTNode *node);

        // ---- helpers ----
        zir::TypeId zirType(const std::string &legacyName);
        zir::ValueId addressOf(SymbolId sym);
        zir::ValueId castTo(zir::ValueId val, zir::TypeId fromTy, zir::TypeId toTy);
        zir::ValueId toCondition(zir::ValueId val, zir::TypeId ty);
        std::string freshLabel(const char *prefix);

        // Control-flow bookkeeping -- every place lowering moves to a new
        // block, or gives the current one a real terminator, goes through
        // these so blockTerminated_ never drifts out of sync.
        void setInsert(zir::BlockId block);
        void emitBr(zir::BlockId target);
        void emitCondBr(zir::ValueId cond, zir::BlockId thenB, zir::BlockId elseB);
        void emitBrIfNotTerminated(zir::BlockId target);
        void emitRet(zir::ValueId v);
        void emitRetVoid();
        void emitUnreachableIfNotTerminated();
    };

}  // namespace zust
