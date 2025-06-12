#include "all.hpp"

namespace zlang
{
    void TypeChecker::check(const std::unique_ptr<ASTNode> &program)
    {
        if (!program || program->type != NodeType::Program)
        {
            logError({ErrorType::Type,
                      "TypeChecker error: root is not a Program node"});
            shouldCodegen_ = false;
            return;
        }
        for (auto &child : program->children)
        {
            checkNode(child.get());
        }
    }

    std::string TypeChecker::checkNode(const ASTNode *node)
    {
        if (!node)
            return "";
        auto scope = node->scope;
        switch (node->type)
        {

        case NodeType::VariableDeclaration:
        {
            std::string annotatedType, initType;

            if (node->children.size() >= 1 &&
                node->children[0]->type == NodeType::Symbol)
            {
                annotatedType = node->children[0]->value;
            }
            if (node->children.size() == 2)
            {
                initType = checkNode(node->children[1].get());
            }

            if (annotatedType.empty() && initType.empty())
            {
                logError({ErrorType::Type,
                          "Declaration of '" + node->value +
                              "' needs a type annotation or initializer"});
                shouldCodegen_ = false;
                return "";
            }

            if (!annotatedType.empty() && !initType.empty() &&
                annotatedType != initType)
            {
                logError({ErrorType::Type,
                          "Initializer type '" + initType +
                              "' does not match annotation '" + annotatedType +
                              "' on variable '" + node->value + "'"});
                shouldCodegen_ = false;
            }

            std::string finalType =
                !annotatedType.empty() ? annotatedType : initType;

            scope->defineVariable(node->value, VariableInfo{finalType});
            return finalType;
        }

        case NodeType::VariableReassignment:
        {
            VariableInfo info = scope->lookupVariable(node->value);
            std::string expected = info.type;
            std::string actual = checkNode(node->children[0].get());
            if (expected != actual)
            {
                logError({ErrorType::Type,
                          "Reassignment of '" + node->value +
                              "' expects '" + expected +
                              "', got '" + actual + "'"});
                shouldCodegen_ = false;
            }
            return expected;
        }

        case NodeType::VariableAccess:
        {
            try
            {
                return scope->lookupVariable(node->value).type;
            }
            catch (std::runtime_error &e)
            {
                logError({ErrorType::Type, e.what()});
                shouldCodegen_ = false;
                return "";
            }
        }

        case NodeType::IntegerLiteral:
            return "integer";

        case NodeType::FloatLiteral:
            return (node->value.back() == 'f' || node->value.back() == 'F')
                       ? "float"
                       : "double";

        case NodeType::StringLiteral:
            return "string";

        case NodeType::BooleanLiteral:
            return "boolean";

        case NodeType::BinaryOp:
        {
            std::string lhs = checkNode(node->children[0].get());
            std::string rhs = checkNode(node->children[1].get());
            const auto &op = node->value;

            if (op == "+" || op == "-" || op == "*" || op == "/")
            {
                if (isNumeric(lhs) && lhs == rhs)
                    return lhs;
                logError({ErrorType::Type,
                          "Arithmetic '" + op +
                              "' only on numeric types, got '" + lhs +
                              "' and '" + rhs + "'"});
                shouldCodegen_ = false;
                return "";
            }
            if (op == "||" || op == "&&")
            {
                if (lhs == "boolean" && rhs == "boolean")
                    return "boolean";
                logError({ErrorType::Type,
                          "Logical '" + op +
                              "' needs booleans, got '" +
                              lhs + "' and '" + rhs + "'"});
                shouldCodegen_ = false;
                return "";
            }
            if (op == "==" || op == "!=" || op == ">=" || op == ">" || op == "<=" || op == "<")
            {
                if (!isComparable(lhs) || lhs != rhs)
                {
                    logError({ErrorType::Type,
                              "Comparison '" + op +
                                  "' requires matching numeric types, got '" +
                                  lhs + "' and '" + rhs + "'"});
                    shouldCodegen_ = false;
                    return "";
                }
                return "boolean";
            }
            // bitwise | & on ints
            if ((op == "|" || op == "&") && lhs == "integer" && rhs == "integer")
                return "integer";

            logError({ErrorType::Type,
                      "Unknown binary operator '" + op + "'"});
            shouldCodegen_ = false;
            return "";
        }

        case NodeType::UnaryOp:
        {
            std::string ty = checkNode(node->children[0].get());
            const auto &op = node->value;
            if (op == "!")
            {
                if (ty != "boolean")
                {
                    logError({ErrorType::Type,
                              "Logical '!' needs boolean, got '" + ty + "'"});
                    shouldCodegen_ = false;
                }
                return "boolean";
            }
            if (op == "++" || op == "--")
            {
                if (!isNumeric(ty))
                {
                    logError(
                        {ErrorType::Type,
                         "Unary '" + op + "' needs numeric, got '" + ty + "'"});
                    shouldCodegen_ = false;
                }
                return ty;
            }
            logError({ErrorType::Type,
                      "Unknown unary operator '" + op + "'"});
            shouldCodegen_ = false;
            return "";
        }

        // handle if / else etc by checking their children
        case NodeType::IfStatement:
        case NodeType::ElseIfStatement:
        case NodeType::ElseStatement:
            for (auto &c : node->children)
                checkNode(c.get());
            return ""; // control node itself has no type

        default:
            // propagate into any other children
            for (auto &c : node->children)
                checkNode(c.get());
            return "";
        }
    }

    bool TypeChecker::isNumeric(const std::string &ty)
    {
        return ty == "integer" || ty == "float" || ty == "double";
    }

    bool TypeChecker::isComparable(const std::string &ty)
    {
        return isNumeric(ty);
    }

} // namespace zlang