#pragma once

#include <string>
#include <vector>
#include <memory>
#include <iostream>
#include "parser/ScopeContext.hpp"

namespace zlang
{
    enum class NodeType
    {
        Program,
        VariableDeclaration,  // let x: int; or let x = 10;
        VariableReassignment, // x = 42;
        VariableAccess,       // just x
        IntegerLiteral,       // 10, 42
        FloatLiteral,
        StringLiteral,
        BooleanLiteral,
        IfStatement,
        ElseStatement,
        ElseIfStatement,
        BinaryOp,
        UnaryOp,
        Symbol,
    };

    class ASTNode
    {
    public:
        NodeType type;
        std::string value;
        std::vector<std::unique_ptr<ASTNode>> children;
        std::shared_ptr<ScopeContext> scope;

        ASTNode() = default;
        ASTNode(NodeType t, const std::string &val = "", std::shared_ptr<ScopeContext> sc = nullptr)
            : type(t), value(val), scope(sc) {}

        static std::unique_ptr<ASTNode> makeProgramNode(const std::shared_ptr<ScopeContext> scope);
        static std::unique_ptr<ASTNode> makeVariableDeclarationNode(
            const std::string &name,
            std::unique_ptr<ASTNode> typeAnnotation,
            std::unique_ptr<ASTNode> initializer, const std::shared_ptr<ScopeContext> scope);

        static std::unique_ptr<ASTNode> makeVariableReassignmentNode(
            const std::string &name,
            std::unique_ptr<ASTNode> expr, const std::shared_ptr<ScopeContext> scope);
        static std::unique_ptr<ASTNode> makeVariableAccessNode(const std::string &name, const std::shared_ptr<ScopeContext> scope);
        static std::unique_ptr<ASTNode> makeIntegerLiteralNode(const std::string &literal, const std::shared_ptr<ScopeContext> scope);
        static std::unique_ptr<ASTNode> makeSymbolNode(const std::string &name, const std::shared_ptr<ScopeContext> scope);
        static std::unique_ptr<ASTNode> makeFloatLiteralNode(const std::string &literal, const std::shared_ptr<ScopeContext> scope);
        static std::unique_ptr<ASTNode> makeStringLiteralNode(const std::string &literal, const std::shared_ptr<ScopeContext> scope);
        static std::unique_ptr<ASTNode> makeBooleanLiteralNode(const bool value, const std::shared_ptr<ScopeContext> scope);
        static std::unique_ptr<ASTNode> makeBinaryOp(const std::string &op, std::unique_ptr<ASTNode> lhs, std::unique_ptr<ASTNode> rhs, const std::shared_ptr<ScopeContext> scope);
        static std::unique_ptr<ASTNode> makeUnaryOp(const std::string &op, std::unique_ptr<ASTNode> operand, const std::shared_ptr<ScopeContext> scope);
        static std::unique_ptr<ASTNode> makeIfStatement(std::unique_ptr<ASTNode> condition, std::unique_ptr<ASTNode> program, const std::shared_ptr<ScopeContext> scope);
        static std::unique_ptr<ASTNode> makeElseIfStatement(std::unique_ptr<ASTNode> condition, std::unique_ptr<ASTNode> program, const std::shared_ptr<ScopeContext> scope);
        static std::unique_ptr<ASTNode> makeElseStatement(std::unique_ptr<ASTNode> program, const std::shared_ptr<ScopeContext> scope);
        void addChild(std::unique_ptr<ASTNode> child);
        void setElseBranch(std::unique_ptr<ASTNode> elseNode);
        ASTNode *getElseBranch() const;
        void print(std::ostream &out, int indent = 0) const;
    };
}
