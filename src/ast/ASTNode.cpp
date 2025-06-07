#include "all.hpp"

namespace zlang
{

    std::unique_ptr<ASTNode> ASTNode::makeNullNode()
    {
        return std::make_unique<ASTNode>(NodeType::Null);
    }

    std::unique_ptr<ASTNode> ASTNode::makeProgramNode()
    {
        return std::make_unique<ASTNode>(NodeType::Program);
    }

    std::unique_ptr<ASTNode> ASTNode::makeSymbolNode(const std::string &name)
    {
        return std::make_unique<ASTNode>(NodeType::Symbol, name);
    }

    std::unique_ptr<ASTNode> ASTNode::makeBinaryOperatorNode(
        const std::string &op,
        std::unique_ptr<ASTNode> left,
        std::unique_ptr<ASTNode> right)
    {
        auto node = std::make_unique<ASTNode>(NodeType::BinaryOperator, op);
        node->addChild(std::move(left));
        node->addChild(std::move(right));
        return node;
    }

    std::unique_ptr<ASTNode> ASTNode::makeDebugPrintIntegerNode(
        const std::string &name)
    {
        auto node = std::make_unique<ASTNode>(NodeType::DebugPrintInteger, name);
        return node;
    }

    std::unique_ptr<ASTNode> ASTNode::makeVariableDeclarationNode(
        const std::string &name,
        std::unique_ptr<ASTNode> initializer)
    {
        auto node = std::make_unique<ASTNode>(NodeType::VariableDeclaration, name);
        if (initializer)
            node->addChild(std::move(initializer));
        return node;
    }

    std::unique_ptr<ASTNode> ASTNode::makeVariableReassignmentNode(
        const std::string &name,
        std::unique_ptr<ASTNode> expression)
    {
        auto node = std::make_unique<ASTNode>(NodeType::VariableReassignment, name);
        if (expression)
            node->addChild(std::move(expression));
        return node;
    }

    std::unique_ptr<ASTNode> ASTNode::makeVariableAccessNode(
        const std::string &name)
    {
        return std::make_unique<ASTNode>(NodeType::VariableAccess, name);
    }

    std::unique_ptr<ASTNode> ASTNode::makeIntegerLiteralNode(
        const std::string &literal)
    {
        return std::make_unique<ASTNode>(NodeType::IntegerLiteral, literal);
    }

    std::unique_ptr<ASTNode> ASTNode::makeFloatLiteralNode(
        const std::string &literal)
    {
        return std::make_unique<ASTNode>(NodeType::FloatLiteral, literal);
    }

    std::unique_ptr<ASTNode> ASTNode::makeStringLiteralNode(
        const std::string &literal)
    {
        return std::make_unique<ASTNode>(NodeType::StringLiteral, literal);
    }

    std::unique_ptr<ASTNode> ASTNode::makeFunctionDeclarationNode(
        const std::string &name,
        std::unique_ptr<ASTNode> paramsList,
        std::unique_ptr<ASTNode> returnType,
        std::unique_ptr<ASTNode> body)
    {
        auto node = std::make_unique<ASTNode>(NodeType::FunctionDeclaration, name);
        if (paramsList)
            node->addChild(std::move(paramsList));
        if (returnType)
            node->addChild(std::move(returnType));
        if (body)
            node->addChild(std::move(body));
        return node;
    }

    std::unique_ptr<ASTNode> ASTNode::makeFunctionParamsListNode()
    {
        return std::make_unique<ASTNode>(NodeType::FunctionParamsList);
    }

    std::unique_ptr<ASTNode> ASTNode::makeFunctionParamNode(
        const std::string &name,
        const std::string &typeName)
    {
        auto node = std::make_unique<ASTNode>(NodeType::FunctionParam, name);
        node->addChild(std::make_unique<ASTNode>(NodeType::Symbol, typeName));
        return node;
    }

    std::unique_ptr<ASTNode> ASTNode::makeFunctionReturnTypeNode(
        const std::string &typeName)
    {
        return std::make_unique<ASTNode>(NodeType::FunctionReturnType, typeName);
    }

    std::unique_ptr<ASTNode> ASTNode::makeIfNode(
        std::unique_ptr<ASTNode> condition,
        std::unique_ptr<ASTNode> thenBranch,
        std::unique_ptr<ASTNode> elseBranch)
    {
        auto node = std::make_unique<ASTNode>(NodeType::If);
        if (condition)
            node->addChild(std::move(condition));
        if (thenBranch)
            node->addChild(std::move(thenBranch));
        if (elseBranch)
            node->addChild(std::move(elseBranch));
        return node;
    }

    std::unique_ptr<ASTNode> ASTNode::makeConditionNode(
        std::unique_ptr<ASTNode> expr)
    {
        auto node = std::make_unique<ASTNode>(NodeType::Condition);
        if (expr)
            node->addChild(std::move(expr));
        return node;
    }

    std::unique_ptr<ASTNode> ASTNode::makeElseNode(
        std::unique_ptr<ASTNode> branch)
    {
        auto node = std::make_unique<ASTNode>(NodeType::Else);
        if (branch)
            node->addChild(std::move(branch));
        return node;
    }

    std::unique_ptr<ASTNode> ASTNode::makeFunctionCallNode(
        const std::string &name,
        std::unique_ptr<ASTNode> argsList)
    {
        auto node = std::make_unique<ASTNode>(NodeType::FunctionCall, name);
        if (argsList)
            node->addChild(std::move(argsList));
        return node;
    }

    std::unique_ptr<ASTNode> ASTNode::makeFunctionArgsListNode()
    {
        return std::make_unique<ASTNode>(NodeType::FunctionArgsList);
    }

    void ASTNode::print(std::ostream &out, int indent) const
    {
        for (int i = 0; i < indent; ++i)
            out << ' ';
        switch (type)
        {
        case NodeType::Null:
            out << "Null";
            break;
        case NodeType::Program:
            out << "Program";
            break;
        case NodeType::Symbol:
            out << "Symbol(" << value << ")";
            break;
        case NodeType::BinaryOperator:
            out << "BinaryOp(" << value << ")";
            break;
        case NodeType::DebugPrintInteger:
            out << "PrintInt(" << value << ")";
            break;
        case NodeType::VariableDeclaration:
            out << "VarDecl(" << value << ")";
            break;
        case NodeType::VariableReassignment:
            out << "VarReassign(" << value << ")";
            break;
        case NodeType::VariableAccess:
            out << "VarAccess(" << value << ")";
            break;
        case NodeType::IntegerLiteral:
            out << "Int(" << value << ")";
            break;
        case NodeType::FloatLiteral:
            out << "Float(" << value << ")";
            break;
        case NodeType::StringLiteral:
            out << "String("
                   " << value << "
                   ")";
            break;
        case NodeType::FunctionDeclaration:
            out << "FuncDecl(" << value << ")";
            break;
        case NodeType::FunctionParamsList:
            out << "FuncParamsList";
            break;
        case NodeType::FunctionParam:
            out << "FuncParam(" << value << ")";
            break;
        case NodeType::FunctionReturnType:
            out << "FuncReturnType(" << value << ")";
            break;
        case NodeType::If:
            out << "If";
            break;
        case NodeType::Condition:
            out << "Condition";
            break;
        case NodeType::Else:
            out << "Else";
            break;
        case NodeType::FunctionCall:
            out << "FuncCall(" << value << ")";
            break;
        case NodeType::FunctionArgsList:
            out << "FuncArgsList";
            break;
        default:
            out << "Unknown";
        }
        out << '\n';
        for (const auto &child : children)
        {
            child->print(out, indent + 2);
        }
    }

}