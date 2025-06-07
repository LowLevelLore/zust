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
        Null,
        Program,
        Symbol,
        BinaryOperator,
        DebugPrintInteger,
        VariableDeclaration,
        VariableReassignment,
        VariableAccess,
        IntegerLiteral,
        FloatLiteral,
        StringLiteral,
        FunctionDeclaration,
        FunctionParamsList,
        FunctionParam,
        FunctionReturnType,
        If,
        Condition,
        Else,
        FunctionCall,
        FunctionArgsList,
    };

    /// ASTNode represents a node in the abstract syntax tree
    class ASTNode
    {
    public:
        NodeType type;
        std::string value; // Used for names, literals, operators
        std::vector<std::unique_ptr<ASTNode>> children;

        ASTNode() = default;

        ASTNode(NodeType t, const std::string &val = "")
            : type(t), value(val)
        {
        }

        // Factory methods
        static std::unique_ptr<ASTNode> makeNullNode();
        static std::unique_ptr<ASTNode> makeProgramNode();
        static std::unique_ptr<ASTNode> makeSymbolNode(const std::string &name);
        static std::unique_ptr<ASTNode> makeBinaryOperatorNode(
            const std::string &op,
            std::unique_ptr<ASTNode> left,
            std::unique_ptr<ASTNode> right);
        static std::unique_ptr<ASTNode> makeDebugPrintIntegerNode(
            const std::string &name);
        static std::unique_ptr<ASTNode> makeVariableDeclarationNode(
            const std::string &name,
            std::unique_ptr<ASTNode> initializer);
        static std::unique_ptr<ASTNode> makeVariableReassignmentNode(
            const std::string &name,
            std::unique_ptr<ASTNode> expression);
        static std::unique_ptr<ASTNode> makeVariableAccessNode(
            const std::string &name);
        static std::unique_ptr<ASTNode> makeIntegerLiteralNode(
            const std::string &literal);
        static std::unique_ptr<ASTNode> makeFloatLiteralNode(
            const std::string &literal);
        static std::unique_ptr<ASTNode> makeStringLiteralNode(
            const std::string &literal);
        static std::unique_ptr<ASTNode> makeFunctionDeclarationNode(
            const std::string &name,
            std::unique_ptr<ASTNode> paramsList,
            std::unique_ptr<ASTNode> returnType,
            std::unique_ptr<ASTNode> body);
        static std::unique_ptr<ASTNode> makeFunctionParamsListNode();
        static std::unique_ptr<ASTNode> makeFunctionParamNode(
            const std::string &name,
            const std::string &typeName);
        static std::unique_ptr<ASTNode> makeFunctionReturnTypeNode(
            const std::string &typeName);
        static std::unique_ptr<ASTNode> makeIfNode(
            std::unique_ptr<ASTNode> condition,
            std::unique_ptr<ASTNode> thenBranch,
            std::unique_ptr<ASTNode> elseBranch = nullptr);
        static std::unique_ptr<ASTNode> makeConditionNode(
            std::unique_ptr<ASTNode> expr);
        static std::unique_ptr<ASTNode> makeElseNode(
            std::unique_ptr<ASTNode> branch);
        static std::unique_ptr<ASTNode> makeFunctionCallNode(
            const std::string &name,
            std::unique_ptr<ASTNode> argsList);
        static std::unique_ptr<ASTNode> makeFunctionArgsListNode();

        void addChild(std::unique_ptr<ASTNode> child)
        {
            children.push_back(std::move(child));
        }
        void print(std::ostream &out, int indent = 0) const;
    };

} // namespace zlang

#endif // ZLANG_AST_ASTNODE_HPP