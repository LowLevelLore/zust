#pragma once

#include <memory>
#include <string>
#include "ast/ASTNode.hpp"
#include "parser/ScopeContext.hpp"
#include "common/Errors.hpp"
#include <set>

namespace zlang
{
    static const std::set<std::string> numeric_types = {"integer", "size_t", "uint8_t", "uint16_t", "uint32_t", "uint64_t", "uint128_t", "int8_t", "int16_t", "int32_t", "int64_t", "int128_t", "float", "double"};

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