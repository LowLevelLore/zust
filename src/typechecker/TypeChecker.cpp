#include "all.hpp"

namespace zlang
{
    void TypeChecker::check(const std::unique_ptr<ASTNode> &program)
    {
        if (program->type != NodeType::Program)
            throw std::runtime_error("TypeChecker error: expected Program node");
        for (const auto &child : program->children)
        {
            checkNode(child.get());
        }
    }

    Types TypeChecker::checkNode(const ASTNode *node)
    {
        switch (node->type)
        {
        case NodeType::VariableDeclaration:
        {
            Types declared = Types::Unknown;
            Types actual = Types::Unknown;

            if (node->children.size() >= 1 && node->children[0]->type == NodeType::Symbol)
            {
                declared = parseType(node->children[0]->value);
            }
            if (node->children.size() == 2 && node->children[1]->type != NodeType::Symbol)
            {
                actual = checkNode(node->children[1].get());
            }

            if (declared == Types::Unknown)
            {
                std::ostringstream oss;
                oss << "Type error: Unknown Type"
                    << " on variable '" << node->value << "'.";
                throw std::runtime_error(oss.str());
            }

            if (declared != Types::Unknown && actual != Types::Unknown && declared != actual)
            {
                std::ostringstream oss;
                oss << "Type error: initializer type '" << typeToString(actual)
                    << "' does not match annotation '" << typeToString(declared)
                    << "' on variable '" << node->value << "'.";
                throw std::runtime_error(oss.str());
            }

            Types finalType = (declared != Types::Unknown ? declared : actual);
            symbolTable[node->value] = finalType;
            return finalType;
        }
        case NodeType::VariableReassignment:
        {
            const std::string &name = node->value;
            auto it = symbolTable.find(name);
            if (it == symbolTable.end())
                throw std::runtime_error("Type error: variable '" + name + "' not declared");

            Types expected = it->second;
            const ASTNode *expr = node->children[0].get();
            Types actual = checkNode(expr);
            if (expected != actual)
            {
                std::ostringstream oss;
                oss << "Type error: reassignment of '" << name
                    << "' expects type " << typeToString(expected)
                    << ", but got " << typeToString(actual);
                throw std::runtime_error(oss.str());
            }
            return expected;
        }
        case NodeType::VariableAccess:
        {
            const std::string &name = node->value;
            auto it = symbolTable.find(name);
            if (it == symbolTable.end())
                throw std::runtime_error("Type error: variable '" + name + "' not declared");
            return it->second;
        }
        case NodeType::IntegerLiteral:
            return Types::Int;
        case NodeType::FloatLiteral:
        {
            auto &t = node->value;
            return (t.back() == 'f' || t.back() == 'F') ? Types::Float : Types::Double;
        }
        case NodeType::StringLiteral:
            return Types::String;
        case NodeType::BinaryOp:
        {
            auto lhs = checkNode(node->children[0].get());
            auto rhs = checkNode(node->children[1].get());
            std::string op = node->value;
            if (op == "+" || op == "-" || op == "*" || op == "/")
            {
                if ((lhs == Types::Int || lhs == Types::Float) && (rhs == lhs))
                    return lhs;
            }
            if (op == "||" || op == "&&")
            {
                if (lhs == Types::Int && rhs == Types::Int)
                    return Types::Int;
            }
            if (op == "|" || op == "&")
            {
                if (lhs == Types::Int && rhs == Types::Int)
                    return Types::Int;
            }
            throw std::runtime_error("Type error: invalid operands for '" + op + "'");
        }
        case NodeType::UnaryOp:
        {
            auto ty = checkNode(node->children[0].get());
            std::string op = node->value;
            if (op == "!")
            {
                if (ty == Types::Int)
                    return Types::Int;
            }
            if (op == "++" || op == "--")
            {
                // ensure l-value & numeric
                return ty;
            }
            throw std::runtime_error("Type error: invalid unary '" + op + "'");
        }
        default:
            return Types::Unknown;
        }
    }

    Types TypeChecker::parseType(const std::string &typeName)
    {
        if (typeName == "float")
            return Types::Float;
        if (typeName == "double")
            return Types::Double;
        if (typeName == "string")
            return Types::String;
        if (typeName == "integer" || typeName == "int")
            return Types::Int;
        return Types::Unknown;
    }

    std::string typeToString(Types type)
    {
        switch (type)
        {
        case Types::Int:
            return "Integer";
        case Types::Float:
            return "Float";
        case Types::Double:
            return "Double";
        case Types::String:
            return "String";
        default:
            return "Unknown";
        }
    }
}