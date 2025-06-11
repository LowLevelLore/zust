#ifndef ZLANG_AST_ASTNODE_HPP
#define ZLANG_AST_ASTNODE_HPP

#include <string>
#include <vector>
#include <memory>
#include <iostream>

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

        ASTNode() = default;
        ASTNode(NodeType t, const std::string &val = "")
            : type(t), value(val) {}

        static std::unique_ptr<ASTNode> makeProgramNode();
        static std::unique_ptr<ASTNode> makeVariableDeclarationNode(
            const std::string &name,
            std::unique_ptr<ASTNode> initOrType = nullptr);
        static std::unique_ptr<ASTNode> makeVariableDeclarationNode(
            const std::string &name,
            std::unique_ptr<ASTNode> typeAnnotation,
            std::unique_ptr<ASTNode> initializer);

        static std::unique_ptr<ASTNode> makeVariableReassignmentNode(
            const std::string &name,
            std::unique_ptr<ASTNode> expr);
        static std::unique_ptr<ASTNode> makeVariableAccessNode(const std::string &name);
        static std::unique_ptr<ASTNode> makeIntegerLiteralNode(const std::string &literal);
        static std::unique_ptr<ASTNode> makeSymbolNode(const std::string &name);
        static std::unique_ptr<ASTNode> makeFloatLiteralNode(const std::string &literal);
        static std::unique_ptr<ASTNode> makeStringLiteralNode(const std::string &literal);
        static std::unique_ptr<ASTNode> makeBinaryOp(const std::string &op, std::unique_ptr<ASTNode> lhs, std::unique_ptr<ASTNode> rhs);
        static std::unique_ptr<ASTNode> makeUnaryOp(const std::string &op, std::unique_ptr<ASTNode> operand);
        void addChild(std::unique_ptr<ASTNode> child);
        void print(std::ostream &out, int indent = 0) const;
    };
}

#endif // ZLANG_AST_ASTNODE_HPP
