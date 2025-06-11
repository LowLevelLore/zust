#ifndef ZLANG_TYPECHECKER_HPP
#define ZLANG_TYPECHECKER_HPP

#include <string>
#include <unordered_map>
#include <stdexcept>
#include "../ast/ASTNode.hpp"

namespace zlang
{
    /// Represents variable types in zlang
    enum class Types
    {
        Int,
        Float,
        Double,
        String,
        Unknown
    };

    std::string typeToString(Types type);

    class TypeChecker
    {
    public:
        /// Performs type checking on the given AST (program node)
        /// Throws std::runtime_error on type errors with descriptive messages
        void check(const std::unique_ptr<ASTNode> &program);

    private:
        std::unordered_map<std::string, Types> symbolTable;

        /// Checks a statement or expression node, returns its Types
        Types checkNode(const ASTNode *node);

        /// Helper to parse annotation string to Types
        Types parseType(const std::string &typeName);
    };
}

#endif // ZLANG_TYPECHECKER_HPP