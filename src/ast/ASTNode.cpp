#include "all.hpp"

namespace zlang
{
    std::unique_ptr<ASTNode> ASTNode::makeProgramNode(const std::shared_ptr<ScopeContext> scope)
    {
        return std::make_unique<ASTNode>(NodeType::Program, "", scope);
    }

    std::unique_ptr<ASTNode> ASTNode::makeVariableReassignmentNode(
        const std::string &name,
        std::unique_ptr<ASTNode> expr, const std::shared_ptr<ScopeContext> scope)
    {
        auto node = std::make_unique<ASTNode>(NodeType::VariableReassignment, name, scope);
        if (expr)
            node->addChild(std::move(expr));
        return node;
    }

    std::unique_ptr<ASTNode> ASTNode::makeVariableAccessNode(const std::string &name, const std::shared_ptr<ScopeContext> scope)
    {
        return std::make_unique<ASTNode>(NodeType::VariableAccess, name, scope);
    }

    std::unique_ptr<ASTNode> ASTNode::makeIntegerLiteralNode(const std::string &literal, const std::shared_ptr<ScopeContext> scope)
    {
        return std::make_unique<ASTNode>(NodeType::IntegerLiteral, literal, scope);
    }

    void ASTNode::setElseBranch(std::unique_ptr<ASTNode> elseNode)
    {
        children.push_back(std::move(elseNode));
    }

    ASTNode *ASTNode::getElseBranch() const
    {
        if (children.empty())
            return nullptr;
        return children.back().get();
    }

    std::unique_ptr<ASTNode> ASTNode::makeVariableDeclarationNode(
        const std::string &name,
        std::unique_ptr<ASTNode> typeAnnotation,
        std::unique_ptr<ASTNode> initializer,
        const std::shared_ptr<ScopeContext> scope)
    {
        auto node = std::make_unique<ASTNode>(NodeType::VariableDeclaration, name, scope);
        scope.get()->defineVariable(name, {typeAnnotation.get()->value});
        if (typeAnnotation)
            node->addChild(std::move(typeAnnotation));
        if (initializer)
            node->addChild(std::move(initializer));
        return node;
    }

    std::unique_ptr<ASTNode> ASTNode::makeSymbolNode(const std::string &name, const std::shared_ptr<ScopeContext> scope)
    {
        return std::make_unique<ASTNode>(NodeType::Symbol, name, scope);
    }

    std::unique_ptr<ASTNode> ASTNode::makeFloatLiteralNode(const std::string &lit, const std::shared_ptr<ScopeContext> scope)
    {
        return std::make_unique<ASTNode>(NodeType::FloatLiteral, lit, scope);
    }
    std::unique_ptr<ASTNode> ASTNode::makeStringLiteralNode(const std::string &lit, const std::shared_ptr<ScopeContext> scope)
    {
        return std::make_unique<ASTNode>(NodeType::StringLiteral, lit, scope);
    }
    std::unique_ptr<ASTNode> ASTNode::makeBooleanLiteralNode(const bool value, const std::shared_ptr<ScopeContext> scope)
    {
        if (value)
        {
            return std::make_unique<ASTNode>(NodeType::BooleanLiteral, "true", scope);
        }
        else
        {
            return std::make_unique<ASTNode>(NodeType::BooleanLiteral, "false", scope);
        }
    }

    std::unique_ptr<ASTNode> ASTNode::makeBinaryOp(
        const std::string &op,
        std::unique_ptr<ASTNode> lhs,
        std::unique_ptr<ASTNode> rhs, const std::shared_ptr<ScopeContext> scope)
    {
        auto node = std::make_unique<ASTNode>(NodeType::BinaryOp, op, scope);
        node->addChild(std::move(lhs));
        node->addChild(std::move(rhs));
        return node;
    }

    std::unique_ptr<ASTNode> ASTNode::makeUnaryOp(
        const std::string &op,
        std::unique_ptr<ASTNode> operand, const std::shared_ptr<ScopeContext> scope)
    {
        auto node = std::make_unique<ASTNode>(NodeType::UnaryOp, op, scope);
        node->addChild(std::move(operand));
        return node;
    }

    std::unique_ptr<ASTNode> ASTNode::makeIfStatement(std::unique_ptr<ASTNode> cond,
                                                      std::unique_ptr<ASTNode> thenBlock, const std::shared_ptr<ScopeContext> scope)
    {
        auto node = std::make_unique<ASTNode>(NodeType::IfStatement, "", scope);
        node->addChild(std::move(cond));
        node->addChild(std::move(thenBlock));
        return node;
    }

    std::unique_ptr<ASTNode> ASTNode::makeElseIfStatement(std::unique_ptr<ASTNode> cond,
                                                          std::unique_ptr<ASTNode> thenBlock, const std::shared_ptr<ScopeContext> scope)
    {
        auto node = std::make_unique<ASTNode>(NodeType::ElseIfStatement, "", scope);
        node->addChild(std::move(cond));
        node->addChild(std::move(thenBlock));
        return node;
    }

    std::unique_ptr<ASTNode> ASTNode::makeElseStatement(std::unique_ptr<ASTNode> elseBlock, const std::shared_ptr<ScopeContext> scope)
    {
        auto node = std::make_unique<ASTNode>(NodeType::ElseStatement, "", scope);
        node->addChild(std::move(elseBlock));
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
        case NodeType::BooleanLiteral:
            out << "BooleanLiteral(" << value << ")";
            break;
        case NodeType::IfStatement:
            out << "If(" << value << ")";
            break;
        case NodeType::ElseIfStatement:
            out << "ElseIf(" << value << ")";
            break;
        case NodeType::ElseStatement:
            out << "Else(" << value << ")";
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
