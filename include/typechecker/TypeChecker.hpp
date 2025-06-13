#pragma once

#include <memory>
#include <string>
#include "ast/ASTNode.hpp"
#include "parser/ScopeContext.hpp"
#include "common/Errors.hpp"
#include <set>

namespace zlang
{
    static const std::set<std::string> numeric_types = {"integer", "size_t", "uint8_t", "uint16_t", "uint32_t", "uint64_t", "int8_t", "int16_t", "int32_t", "int64_t", "float", "double"};
    static const std::set<std::string> integral_types = {"integer", "size_t", "uint8_t", "uint16_t", "uint32_t", "uint64_t", "int8_t", "int16_t", "int32_t", "int64_t"};

    class TypeChecker
    {
    public:
        /// Walks the whole AST, logs any errors found.
        void check(const std::unique_ptr<ASTNode> &program);
        bool shouldCodegen()
        {
            return shouldCodegen_;
        }

        static inline std::string typeName(const TypeInfo &info)
        {
            if (info.isFloat)
                return info.bits == 32 ? "float" : "double";
            return (info.isSigned ? "int" : "uint") + std::to_string(info.bits) + "_t";
        }
        static inline TypeInfo promoteType(const TypeInfo &a, const TypeInfo &b)
        {
            if (a.isFloat || b.isFloat)
            {
                if (a.isFloat && b.isFloat)
                    return a.bits > b.bits ? a : b;
                if (a.isFloat)
                {
                    if (b.bits > a.bits)
                    {
                        return {b.bits, b.bits / 8, true, true};
                    }
                    else
                    {
                        return a;
                    }
                }
                else
                {
                    if (a.bits > b.bits)
                    {
                        return {a.bits, a.bits / 8, true, true};
                    }
                    else
                    {
                        return b;
                    }
                }
            }
            if (a.bits != b.bits)
                return a.bits > b.bits ? a : b;
            return a;
        }

    private:
        /// Checks a single AST node, returns its type name (or "" on error).
        std::string checkNode(const ASTNode *node);
        /// Helpers
        bool isNumeric(const std::string &ty);
        bool isInteger(const std::string &ty);
        bool isComparable(const std::string &ty);
        bool shouldCodegen_ = true;
    };

} // namespace zlang