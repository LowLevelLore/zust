#include "all.hpp"

namespace zust
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
        if (children.size() < 2)
        {
            throw std::logic_error("setElseBranch on a non‑conditional node");
        }

        if (children.size() == 2)
        {
            // no else/elif attached yet: attach here
            children.push_back(std::move(elseNode));
        }
        else
        {
            // we already have an elseBranch in children[2]:
            // forward the new elseNode into _that_ node
            ASTNode *existing = children[2].get();
            existing->setElseBranch(std::move(elseNode));
        }
    }

    ASTNode *ASTNode::getElseBranch() const
    {
        if (children.size() < 3 || children[2] == nullptr)
            return nullptr;

        auto *branch = children[2].get();
        if (branch->type != NodeType::ElseIfStatement && branch->type != NodeType::ElseStatement)
            return nullptr;

        return branch;
    }

    ASTNode *ASTNode::getInitializationForLoop() const
    {
        if (!(this->type == NodeType::ForLoop))
        {
            throw std::logic_error("getInitializationForLoop on a non-for-loop node");
        }
        if (children.size() < 1)
        {
            throw std::logic_error("getInitializationForLoop on a non-for-loop node");
        }
        return children[0].get();
    }
    ASTNode *ASTNode::getConditionForLoop() const
    {
        if (!(this->type == NodeType::ForLoop))
        {
            throw std::logic_error("getConditionForLoop on a non-for-loop node");
        }
        if (children.size() < 2)
        {
            throw std::logic_error("getConditionForLoop on a non-for-loop node");
        }
        return children[1].get();
    }
    ASTNode *ASTNode::getPostLoopForLoop() const
    {
        if (!(this->type == NodeType::ForLoop))
        {
            throw std::logic_error("getPostLoopForLoop on a non-for-loop node");
        }
        if (children.size() < 3)
        {
            throw std::logic_error("getPostLoopForLoop on a non-for-loop node");
        }
        return children[2].get();
    }
    ASTNode *ASTNode::getBodyForLoop() const
    {
        if (!(this->type == NodeType::ForLoop))
        {
            throw std::logic_error("getBodyForLoop on a non-for-loop node");
        }
        if (children.size() < 4)
        {
            throw std::logic_error("getBodyForLoop on a non-for-loop node");
        }
        return children[3].get();
    }
    ASTNode *ASTNode::getConditionWhileLoop() const
    {
        if (!(this->type == NodeType::WhileLoop))
        {
            throw std::logic_error("getConditionWhileLoop on a non-while-loop node");
        }
        if (children.size() < 1)
        {
            throw std::logic_error("getConditionWhileLoop on a non-while-loop node");
        }
        return children[0].get();
    }
    ASTNode *ASTNode::getBodyWhileLoop() const
    {
        if (!(this->type == NodeType::WhileLoop))
        {
            throw std::logic_error("getBodyWhileLoop on a non-while-loop node");
        }
        if (children.size() < 2)
        {
            throw std::logic_error("getBodyWhileLoop on a non-while-loop node");
        }
        return children[1].get();
    }

    std::optional<std::unique_ptr<ASTNode>>
    ASTNode::makeVariableDeclarationNode(
        const std::string &name,
        std::unique_ptr<ASTNode> typeAnnotation,
        std::unique_ptr<ASTNode> initializer,
        const std::shared_ptr<ScopeContext> scope)
    {
        auto node = std::make_unique<ASTNode>(NodeType::VariableDeclaration, name, scope);
        bool result = scope.get()->defineVariable(name, {typeAnnotation.get()->value});
        if (result)
        {
            if (typeAnnotation)
                node->addChild(std::move(typeAnnotation));
            if (initializer)
                node->addChild(std::move(initializer));
            return node;
        }
        else
        {
            return std::nullopt;
        }
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

    std::unique_ptr<ASTNode> ASTNode::makeFunctionParameterList(const std::vector<ParamInfo> params, std::shared_ptr<ScopeContext> scope)
    {
        auto node = std::make_unique<ASTNode>(NodeType::FunctionParameterList, "", scope);
        for (ParamInfo pi : params)
        {
            auto child = std::make_unique<ASTNode>(NodeType::FunctionParameter, "", scope);
            auto name = std::make_unique<ASTNode>(NodeType::Symbol, pi.name, scope);
            auto type = std::make_unique<ASTNode>(NodeType::Symbol, pi.type, scope);
            child->children.push_back(std::move(name));
            child->children.push_back(std::move(type));
            node->children.push_back(std::move(child));
        }
        return node;
    }

    std::unique_ptr<ASTNode> ASTNode::makeExternFunctionDeclaration(std::string name, const std::shared_ptr<ScopeContext> scope, std::vector<ParamInfo> params, std::string returnType, bool isVariadic)
    {
        auto node = std::make_unique<ASTNode>(NodeType::ExternFunction, name, scope);
        auto paramsList = ASTNode::makeFunctionParameterList(params, scope);
        auto returnType_ = std::make_unique<ASTNode>(NodeType::FunctionReturnType, returnType, scope);
        node->children.push_back(std::move(paramsList));
        node->children.push_back(std::move(returnType_));
        scope->defineFunction(name, FunctionInfo{.paramTypes = params, .returnType = returnType, .name = name, .label = "", .isExtern = true, .isVariadic = isVariadic});
        return node;
    }

    std::unique_ptr<ASTNode> ASTNode::makeFunctionDeclaration(std::string name, const std::shared_ptr<ScopeContext> scope, std::vector<ParamInfo> params, std::string returnType, std::unique_ptr<ASTNode> body, bool isVariadic)
    {
        auto node = std::make_unique<ASTNode>(NodeType::Function, name, scope);
        auto paramsList = ASTNode::makeFunctionParameterList(params, scope);
        auto returnType_ = std::make_unique<ASTNode>(NodeType::FunctionReturnType, returnType, scope);
        node->children.push_back(std::move(paramsList));
        node->children.push_back(std::move(returnType_));
        body->scope->returnType = returnType;
        for (ParamInfo pi : params)
        {
            body->scope->defineVariable(pi.name, VariableInfo{.type = pi.type});
        }
        node->children.push_back(std::move(body));
        scope->defineFunction(name, FunctionInfo{.paramTypes = params, .returnType = returnType, .name = name, .label = "", .isExtern = false, .isVariadic = isVariadic});
        return node;
    }

    ASTNode *ASTNode::getFunctionParamList() const
    {
        assert(type == NodeType::Function || type == NodeType::ExternFunction);
        return children[0].get();
    }
    ASTNode *ASTNode::getFunctionParamReturnType() const
    {
        assert(type == NodeType::Function || type == NodeType::ExternFunction);
        return children[1].get();
    }
    ASTNode *ASTNode::getFunctionBody() const
    {
        assert(type == NodeType::Function);
        return children[2].get();
    }

    std::unique_ptr<ASTNode> ASTNode::makeFunctionCall(std::string name, std::vector<std::unique_ptr<ASTNode>> arguments, const std::shared_ptr<ScopeContext> scope)
    {
        auto node = std::make_unique<ASTNode>(NodeType::FunctionCall, name, scope);
        auto args = std::make_unique<ASTNode>(NodeType::FunctionCallArgumentList, "", scope);
        for (auto &arg : arguments)
        {
            args->addChild(std::move(arg));
        }
        node->addChild(std::move(args));
        return node;
    }

    std::unique_ptr<ASTNode> ASTNode::makeForLoopNode(std::unique_ptr<ASTNode> initializer, std::unique_ptr<ASTNode> condition, std::unique_ptr<ASTNode> postLoop, std::unique_ptr<ASTNode> body, const std::shared_ptr<ScopeContext> scope)
    {
        auto node = std::make_unique<ASTNode>(NodeType::ForLoop, "", scope);
        initializer.get()->value = "for_init";
        condition.get()->value = "for_cond";
        postLoop.get()->value = "for_post";
        node->addChild(std::move(initializer));
        node->addChild(std::move(condition));
        node->addChild(std::move(postLoop));
        node->addChild(std::move(body));
        return node;
    }

    std::unique_ptr<ASTNode> ASTNode::makeWhileLoopNode(std::unique_ptr<ASTNode> condition, std::unique_ptr<ASTNode> body, const std::shared_ptr<ScopeContext> scope)
    {
        auto node = std::make_unique<ASTNode>(NodeType::WhileLoop, "", scope);
        condition.get()->value = "while_cond";
        node->addChild(std::move(condition));
        node->addChild(std::move(body));
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
        case NodeType::ExternFunction:
            out << "ExternFunction(" << value << ")";
            break;
        case NodeType::Function:
            out << "Functions(" << value << ")";
            break;
        case NodeType::FunctionParameterList:
            out << "ParameterList(" << value << ")";
            break;
        case NodeType::FunctionParameter:
            out << "Parameter(" << value << ")";
            break;
        case NodeType::FunctionReturnType:
            out << "ReturnType(" << value << ")";
            break;
        case NodeType::FunctionCall:
            out << "FunctionCall(" << value << ")";
            break;
        case NodeType::FunctionCallArgumentList:
            out << "Arguments(" << value << ")";
            break;
        case NodeType::ReturnStatement:
            out << "Return(" << value << ")";
            break;
        case NodeType::ForLoop:
            out << "ForLoop";
            break;
        case NodeType::WhileLoop:
            out << "WhileLoop";
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
