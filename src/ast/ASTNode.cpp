#include "all.hpp"

namespace zlang
{
    std::unique_ptr<ASTNode> ASTNode::makeProgramNode()
    {
        return std::make_unique<ASTNode>(NodeType::Program);
    }

    std::unique_ptr<ASTNode> ASTNode::makeVariableDeclarationNode(
        const std::string &name,
        std::unique_ptr<ASTNode> initOrType)
    {
        auto node = std::make_unique<ASTNode>(NodeType::VariableDeclaration, name);
        if (initOrType)
            node->addChild(std::move(initOrType));
        return node;
    }

    std::unique_ptr<ASTNode> ASTNode::makeVariableReassignmentNode(
        const std::string &name,
        std::unique_ptr<ASTNode> expr)
    {
        auto node = std::make_unique<ASTNode>(NodeType::VariableReassignment, name);
        if (expr)
            node->addChild(std::move(expr));
        return node;
    }

    std::unique_ptr<ASTNode> ASTNode::makeVariableAccessNode(const std::string &name)
    {
        return std::make_unique<ASTNode>(NodeType::VariableAccess, name);
    }

    std::unique_ptr<ASTNode> ASTNode::makeIntegerLiteralNode(const std::string &literal)
    {
        return std::make_unique<ASTNode>(NodeType::IntegerLiteral, literal);
    }

    std::unique_ptr<ASTNode> ASTNode::makeVariableDeclarationNode(
        const std::string &name,
        std::unique_ptr<ASTNode> typeAnnotation,
        std::unique_ptr<ASTNode> initializer)
    {
        auto node = std::make_unique<ASTNode>(NodeType::VariableDeclaration, name);
        if (typeAnnotation)
            node->addChild(std::move(typeAnnotation));
        if (initializer)
            node->addChild(std::move(initializer));
        return node;
    }

    std::unique_ptr<ASTNode> ASTNode::makeSymbolNode(const std::string &name)
    {
        return std::make_unique<ASTNode>(NodeType::Symbol, name);
    }

    std::unique_ptr<ASTNode> ASTNode::makeFloatLiteralNode(const std::string &lit)
    {
        return std::make_unique<ASTNode>(NodeType::FloatLiteral, lit);
    }
    std::unique_ptr<ASTNode> ASTNode::makeStringLiteralNode(const std::string &lit)
    {
        return std::make_unique<ASTNode>(NodeType::StringLiteral, lit);
    }

    std::unique_ptr<ASTNode> ASTNode::makeBinaryOp(
        const std::string &op,
        std::unique_ptr<ASTNode> lhs,
        std::unique_ptr<ASTNode> rhs)
    {
        auto node = std::make_unique<ASTNode>(NodeType::BinaryOp, op);
        node->addChild(std::move(lhs));
        node->addChild(std::move(rhs));
        return node;
    }

    std::unique_ptr<ASTNode> ASTNode::makeUnaryOp(
        const std::string &op,
        std::unique_ptr<ASTNode> operand)
    {
        auto node = std::make_unique<ASTNode>(NodeType::UnaryOp, op);
        node->addChild(std::move(operand));
        return node;
    }

    void ASTNode::addChild(std::unique_ptr<ASTNode> child)
    {
        children.push_back(std::move(child));
    }

    void ASTNode::print(std::ostream &out, int indent) const
    {
        for (int i = 0; i < indent; ++i)
            out << ' ';

        switch (type)
        {
        case NodeType::Program:
            out << "Program";
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
        case NodeType::Symbol:
            out << "Symbol(" << value << ")";
            break;
        case NodeType::FloatLiteral:
            out << "Float(" << value << ")";
            break;
        case NodeType::StringLiteral:
            out << "String(\"" << value << "\")";
            break;
        case NodeType::BinaryOp:
            out << "BinaryOp(" << value << ")";
            break;
        case NodeType::UnaryOp:
            out << "UnaryOp(" << value << ")";
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
