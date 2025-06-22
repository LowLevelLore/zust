#include "all.hpp"

namespace zust {
    void TypeChecker::check(const std::unique_ptr<ASTNode> &program) {
        if (!program || program->type != NodeType::Program) {
            logError({ErrorType::Type,
                      "TypeChecker error: root is not a Program node"});
            shouldCodegen_ = false;
            return;
        }
        checkNode(program.get());
    }

    std::string TypeChecker::checkNode(const ASTNode *node) {
        if (!node)
            return "";
        auto scope = node->scope;
        switch (node->type) {
        case NodeType::Program:
            for (auto &child : node->children) {
                checkNode(child.get());
            }
            return "";

        case NodeType::ExternFunction: {
            if (node->value == "main") {
                logError({ErrorType::Generic, "Function named 'main' cannot be extern"});
                shouldCodegen_ = false;
                return "";
            }
            ASTNode *params = node->getFunctionParamList();
            for (const auto &childPtr : params->children) {
                ASTNode *param = childPtr.get();
                try {
                    node->scope->lookupType(param->children[1]->value);
                } catch (...) {
                    logError(Error{ErrorType::Type, "Undefined type '" + (param->children[1]->value) + "' for parameter named '" + param->children[0]->value + "' in declaration of function named '" + node->value + "'."});
                    shouldCodegen_ = false;
                    return "";
                }
            }
            try {
                node->scope->lookupType(node->getFunctionParamReturnType()->value);
            } catch (...) {
                logError(Error{ErrorType::Type, "Undefined type '" + (node->getFunctionParamReturnType()->value) + "' for return type in declaration of function named '" + node->value + "'."});
                shouldCodegen_ = false;
            }
            return "";
        }

        case NodeType::FunctionCall: {
            if (node->value == "main") {
                logError({ErrorType::Generic, "Cannot call 'main' function explicitly."});
                shouldCodegen_ = false;
                return "";
            }
            FunctionInfo functionInfo;
            try {
                functionInfo = node->scope->lookupFunction(node->value);
            } catch (...) {
                logError(Error{ErrorType::Type, "Undefined functions '" + (node->value) + "'."});
                shouldCodegen_ = false;
                return "";
            }
            const std::vector<zust::ParamInfo> &functionParams = functionInfo.paramTypes;
            const std::vector<std::unique_ptr<ASTNode>> &functionArguments = node->children[0]->children;

            if (functionArguments.size() != functionParams.size()) {
                logError(Error{
                    ErrorType::Type,
                    "Function '" + node->value + "' expects " +
                        std::to_string(functionParams.size()) + " arguments, got " +
                        std::to_string(functionArguments.size()) + "."});
                shouldCodegen_ = false;
                return "";
            }

            for (size_t i = 0; i < functionArguments.size(); ++i) {
                std::string argType = checkNode(functionArguments[i].get());
                std::string paramType = functionParams[i].type;

                if (argType != paramType) {
                    if (!(isNumeric(argType) && isNumeric(paramType))) {
                        logError(Error{
                            ErrorType::Type,
                            "Function '" + node->value + "' argument " +
                                std::to_string(i + 1) + " expects '" + paramType +
                                "', got '" + argType + "'."});
                        shouldCodegen_ = false;
                        return "";
                    }
                }
            }
            return functionInfo.returnType;
        }

        case NodeType::Function: {
            ASTNode *params = node->getFunctionParamList();
            for (const auto &childPtr : params->children) {
                ASTNode *param = childPtr.get();
                try {
                    node->scope->lookupType(param->children[1]->value);
                } catch (...) {
                    logError(Error{ErrorType::Type, "Undefined type '" + (param->children[1]->value) + "' for parameter named '" + param->children[0]->value + "' in declaration of function named '" + node->value + "'."});
                    shouldCodegen_ = false;
                    return "";
                }
            }
            try {
                node->scope->lookupType(node->getFunctionParamReturnType()->value);
            } catch (...) {
                logError(Error{ErrorType::Type, "Undefined type '" + (node->getFunctionParamReturnType()->value) + "' for return type in declaration of function named '" + node->value + "'."});
                shouldCodegen_ = false;
            }
            return "";
        }

        case NodeType::ReturnStatement: {
            std::string expectedRet = scope->returnType;
            std::string actualRet = "none";
            if (!node->children.empty()) {
                actualRet = checkNode(node->children[0].get());
            }

            if (actualRet != expectedRet &&
                !(isNumeric(actualRet) && isNumeric(expectedRet))) {
                logError({ErrorType::Type,
                          "Return type mismatch: expected '" + expectedRet +
                              "', got '" + actualRet + "'"});
                shouldCodegen_ = false;
            }
            return actualRet;
        }

        case NodeType::VariableDeclaration: {
            std::string annotatedType, initType;
            if (node->children.size() >= 1 &&
                node->children[0]->type == NodeType::Symbol) {
                annotatedType = node->children[0]->value;
            }
            if (node->children.size() == 2) {
                initType = checkNode(node->children[1].get());
            }

            if (annotatedType.empty() && initType.empty()) {
                logError({ErrorType::Type,
                          "Declaration of '" + node->value +
                              "' needs a type annotation or initializer"});
                shouldCodegen_ = false;
                return "";
            }

            // If both present but mismatch, only error if *non‑numeric*
            if (!annotatedType.empty() && !initType.empty() &&
                annotatedType != initType) {
                if (!(isNumeric(annotatedType) && isNumeric(initType))) {
                    logError({ErrorType::Type,
                              "Initializer type '" + initType +
                                  "' does not match annotation '" + annotatedType +
                                  "' on variable '" + node->value + "'"});
                    shouldCodegen_ = false;
                    return "";
                }
            }

            // final type is the annotation if provided, else the inferred
            std::string finalType =
                !annotatedType.empty() ? annotatedType : initType;
            return finalType;
        }

        case NodeType::VariableReassignment: {
            VariableInfo info = scope->lookupVariable(node->value);
            std::string expected = info.type;
            std::string actual = checkNode(node->children[0].get());
            if (!(expected == actual or isNumeric(expected) == isNumeric(actual))) {
                logError({ErrorType::Type,
                          "Reassignment of '" + node->value +
                              "' expects '" + expected +
                              "', got '" + actual + "'"});
                shouldCodegen_ = false;
            }
            return expected;
        }

        case NodeType::VariableAccess: {
            try {
                return scope->lookupVariable(node->value).type;
            } catch (...) {
                logError({ErrorType::Type, "Unknown variable " + node->value});
                shouldCodegen_ = false;
                return "";
            }
        }

        case NodeType::IntegerLiteral:
            return "integer";

        case NodeType::FloatLiteral:
            if (!node->value.empty() &&
                (node->value.back() == 'f' || node->value.back() == 'F'))
                return "float";
            else
                return "double";

        case NodeType::StringLiteral:
            return "string";
        case NodeType::BooleanLiteral:
            return "boolean";

        case NodeType::BinaryOp: {
            std::string lhs = checkNode(node->children[0].get());
            std::string rhs = checkNode(node->children[1].get());
            const auto &op = node->value;

            // Arithmetic + - * /
            if (op == "+" || op == "-" || op == "*" || op == "/") {
                if (isNumeric(lhs) && isNumeric(rhs)) {
                    const TypeInfo &tL = node->scope->lookupType(lhs);
                    const TypeInfo &tR = node->scope->lookupType(rhs);

                    TypeInfo promoted = promoteType(tL, tR);

                    return typeName(promoted);
                }
                logError({ErrorType::Type,
                          "Arithmetic '" + op +
                              "' only on numeric types, got '" + lhs +
                              "' and '" + rhs + "'"});
                shouldCodegen_ = false;
                return "";
            }

            // Logical || &&
            if (op == "||" || op == "&&") {
                if (lhs == "boolean" && rhs == "boolean")
                    return "boolean";
                logError({ErrorType::Type,
                          "Logical '" + op +
                              "' needs booleans, got '" + lhs +
                              "' and '" + rhs + "'"});
                shouldCodegen_ = false;
                return "";
            }

            // Comparisons
            if (op == "==" || op == "!=" || op == ">=" || op == ">" || op == "<=" || op == "<") {
                // numeric vs numeric → OK
                if (isNumeric(lhs) && isNumeric(rhs))
                    return "boolean";

                // same exact non‑numeric types → OK
                if (lhs == rhs && !isNumeric(lhs))
                    return "boolean";

                logError({ErrorType::Type,
                          "Comparison '" + op +
                              "' requires both operands to be numeric or same type, "
                              "got '" +
                              lhs + "' and '" + rhs + "'"});
                shouldCodegen_ = false;
                return "";
            }

            // Bitwise on plain integers
            if ((op == "|" || op == "&") &&
                lhs == "integer" && rhs == "integer")
                return "integer";

            logError({ErrorType::Type,
                      "Unknown binary operator '" + op + "'"});
            shouldCodegen_ = false;
            return "";
        }
        case NodeType::UnaryOp: {
            std::string ty = checkNode(node->children[0].get());
            const auto &op = node->value;
            if (op == "!") {
                if (ty != "boolean") {
                    logError({ErrorType::Type,
                              "Logical '!' needs boolean, got '" + ty + "'"});
                    shouldCodegen_ = false;
                }
                return "boolean";
            }
            if (op == "++" || op == "--") {
                if (!isInteger(ty)) {
                    logError({ErrorType::Type,
                              "Unary '" + op + "' needs Integral type, got '" + ty + "'"});
                    shouldCodegen_ = false;
                }
                return ty;
            }
            logError({ErrorType::Type,
                      "Unknown unary operator '" + op + "'"});
            shouldCodegen_ = false;
            return "";
        }
        case NodeType::IfStatement:
        case NodeType::ElseIfStatement: {
            if (node->children.size() > 0)
                checkNode(node->children[0].get());

            if (node->children.size() > 1)
                checkNode(node->children[1].get());

            if (node->children.size() > 2 && node->children[2])
                checkNode(node->children[2].get());

            return "";
        }
        case NodeType::ElseStatement: {
            if (!node->children.empty())
                checkNode(node->children[0].get());
            return "";
        }

        default:
            return "";
        }
    }

    bool TypeChecker::isInteger(const std::string &ty) {
        return integral_types.find(ty) != integral_types.end();
    }

    bool TypeChecker::isNumeric(const std::string &ty) {
        return numeric_types.find(ty) != numeric_types.end();
    }

    bool TypeChecker::isComparable(const std::string &ty) {
        return isNumeric(ty);
    }

}  // namespace zust