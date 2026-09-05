#pragma once

#include <cassert>
#include <iostream>
#include <memory>
#include <string>
#include <vector>

#include "common/Span.hpp"
#include "parser/ScopeContext.hpp"

namespace zust {
    enum class NodeType {
        Program,
        VariableDeclaration,   // let x: int; or let x = 10;
        VariableReassignment,  // x = 42;
        VariableAccess,        // just x
        IntegerLiteral,        // 10, 42
        FloatLiteral,
        StringLiteral,
        BooleanLiteral,
        IfStatement,
        ElseStatement,
        ElseIfStatement,
        BinaryOp,
        UnaryOp,
        Symbol,
        Function,
        ExternFunction,
        FunctionParameter,
        FunctionParameterList,
        FunctionReturnType,
        ReturnStatement,
        FunctionCall,
        FunctionCallArgumentList,
        ForLoop,
        WhileLoop,
        BreakStatement,
        ContinueStatement
    };

    class ASTNode {
    public:
        NodeType type;
        std::string value;
        std::vector<std::unique_ptr<ASTNode>> children;
        std::shared_ptr<ScopeContext> scope;
        // Where in the source this node came from. Not wired into diagnostics
        // yet (ROADMAP M1 does that properly, with real start/end ranges);
        // for now this exists so ZIR lowering has somewhere to carry position
        // information forward from, instead of starting from nothing.
        Span span;
        ASTNode() = default;

        ASTNode(NodeType t, const std::string &val = "", std::shared_ptr<ScopeContext> sc = nullptr, Span sp = {})
            : type(t), value(val), scope(sc), span(sp) {}

        static std::unique_ptr<ASTNode> makeProgramNode(const std::shared_ptr<ScopeContext> scope, Span span = {});
        static std::optional<std::unique_ptr<ASTNode>> makeVariableDeclarationNode(
            const std::string &name, std::unique_ptr<ASTNode> typeAnnotation, std::unique_ptr<ASTNode> initializer,
            const std::shared_ptr<ScopeContext> scope, Span span = {});
        static std::unique_ptr<ASTNode> makeVariableReassignmentNode(const std::string &name,
                                                                     std::unique_ptr<ASTNode> expr,
                                                                     const std::shared_ptr<ScopeContext> scope,
                                                                     Span span = {});
        static std::unique_ptr<ASTNode> makeVariableAccessNode(const std::string &name,
                                                               const std::shared_ptr<ScopeContext> scope,
                                                               Span span = {});
        static std::unique_ptr<ASTNode> makeIntegerLiteralNode(const std::string &literal,
                                                               const std::shared_ptr<ScopeContext> scope,
                                                               Span span = {});
        static std::unique_ptr<ASTNode> makeSymbolNode(const std::string &name,
                                                       const std::shared_ptr<ScopeContext> scope, Span span = {});
        static std::unique_ptr<ASTNode> makeFloatLiteralNode(const std::string &literal,
                                                             const std::shared_ptr<ScopeContext> scope, Span span = {});
        static std::unique_ptr<ASTNode> makeStringLiteralNode(const std::string &literal,
                                                              const std::shared_ptr<ScopeContext> scope,
                                                              Span span = {});
        static std::unique_ptr<ASTNode> makeBooleanLiteralNode(const bool value,
                                                               const std::shared_ptr<ScopeContext> scope,
                                                               Span span = {});
        static std::unique_ptr<ASTNode> makeBinaryOp(const std::string &op, std::unique_ptr<ASTNode> lhs,
                                                     std::unique_ptr<ASTNode> rhs,
                                                     const std::shared_ptr<ScopeContext> scope, Span span = {});
        static std::unique_ptr<ASTNode> makeUnaryOp(const std::string &op, std::unique_ptr<ASTNode> operand,
                                                    const std::shared_ptr<ScopeContext> scope, Span span = {});
        static std::unique_ptr<ASTNode> makeIfStatement(std::unique_ptr<ASTNode> condition,
                                                        std::unique_ptr<ASTNode> program,
                                                        const std::shared_ptr<ScopeContext> scope, Span span = {});
        static std::unique_ptr<ASTNode> makeElseIfStatement(std::unique_ptr<ASTNode> condition,
                                                            std::unique_ptr<ASTNode> program,
                                                            const std::shared_ptr<ScopeContext> scope, Span span = {});
        static std::unique_ptr<ASTNode> makeElseStatement(std::unique_ptr<ASTNode> program,
                                                          const std::shared_ptr<ScopeContext> scope, Span span = {});
        static std::unique_ptr<ASTNode> makeExternFunctionDeclaration(std::string name,
                                                                      const std::shared_ptr<ScopeContext> scope,
                                                                      std::vector<ParamInfo> params,
                                                                      std::string returnType, bool isVariadic,
                                                                      Span span = {});
        static std::unique_ptr<ASTNode> makeFunctionDeclaration(std::string name,
                                                                const std::shared_ptr<ScopeContext> scope,
                                                                std::vector<ParamInfo> params, std::string returnType,
                                                                std::unique_ptr<ASTNode> body, bool isVariadic,
                                                                Span span = {});
        static std::unique_ptr<ASTNode> makeFunctionCall(std::string name,
                                                         std::vector<std::unique_ptr<ASTNode>> arguments,
                                                         const std::shared_ptr<ScopeContext> scope, Span span = {});
        static std::unique_ptr<ASTNode> makeFunctionParameterList(const std::vector<ParamInfo> params,
                                                                  const std::shared_ptr<ScopeContext> scope,
                                                                  Span span = {});
        static std::unique_ptr<ASTNode> makeForLoopNode(std::unique_ptr<ASTNode> initializer,
                                                        std::unique_ptr<ASTNode> condition,
                                                        std::unique_ptr<ASTNode> postLoop,
                                                        std::unique_ptr<ASTNode> body,
                                                        const std::shared_ptr<ScopeContext> scope, Span span = {});
        static std::unique_ptr<ASTNode> makeWhileLoopNode(std::unique_ptr<ASTNode> condition,
                                                          std::unique_ptr<ASTNode> body,
                                                          const std::shared_ptr<ScopeContext> scope, Span span = {});
        static std::unique_ptr<ASTNode> makeBreakStatementNode(const std::shared_ptr<ScopeContext> scope,
                                                               Span span = {});
        static std::unique_ptr<ASTNode> makeContinueStatementNode(const std::shared_ptr<ScopeContext> scope,
                                                                  Span span = {});
        void addChild(std::unique_ptr<ASTNode> child);
        void setElseBranch(std::unique_ptr<ASTNode> elseNode);
        ASTNode *getElseBranch() const;
        ASTNode *getFunctionParamList() const;
        ASTNode *getFunctionParamReturnType() const;
        ASTNode *getFunctionBody() const;
        ASTNode *getInitializationForLoop() const;
        ASTNode *getConditionForLoop() const;
        ASTNode *getPostLoopForLoop() const;
        ASTNode *getBodyForLoop() const;
        ASTNode *getConditionWhileLoop() const;
        ASTNode *getBodyWhileLoop() const;
        void print(std::ostream &out, int indent = 0) const;
    };
}  // namespace zust
