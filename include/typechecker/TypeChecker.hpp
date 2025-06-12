#pragma once

#include <memory>
#include <string>
#include "ast/ASTNode.hpp"
#include "parser/ScopeContext.hpp"
#include "common/Errors.hpp"

namespace zlang
{

    class TypeChecker
    {
    public:
        /// Walks the whole AST, logs any errors found.
        void check(const std::unique_ptr<ASTNode> &program);
        bool shouldCodegen()
        {
            return shouldCodegen_;
        }

    private:
        /// Checks a single AST node, returns its type name (or "" on error).
        std::string checkNode(const ASTNode *node);
        /// Helpers
        bool isNumeric(const std::string &ty);
        bool isComparable(const std::string &ty);

        bool shouldCodegen_ = true;
    };

} // namespace zlang