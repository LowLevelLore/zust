#pragma once

#include <memory>
#include <set>
#include <string>

#include "ast/ASTNode.hpp"
#include "common/Errors.hpp"
#include "parser/ScopeContext.hpp"

// TODO: Check that all paths inside the function return appropriate value.

namespace zust {
    static const std::set<std::string> numeric_types = {"integer", "size_t", "uint8_t", "uint16_t", "uint32_t", "uint64_t", "int8_t", "int16_t", "int32_t", "int64_t", "float", "double"};
    static const std::set<std::string> integral_types = {"integer", "size_t", "uint8_t", "uint16_t", "uint32_t", "uint64_t", "int8_t", "int16_t", "int32_t", "int64_t"};

    class TypeChecker {
    public:
        void check(const std::unique_ptr<ASTNode> &program);
        bool shouldCodegen() {
            return shouldCodegen_;
        }
        static inline std::string typeName(const TypeInfo &info) {
            return info.name;
        }
        static inline TypeInfo promoteType(const TypeInfo &a, const TypeInfo &b) {
            if (a.isString || b.isString ||
                a.isPointer || b.isPointer ||
                a.isUserDefined || b.isUserDefined) {
                throw std::runtime_error(
                    "Invalid type promotion between: " + typeName(a) + " and " + typeName(b));
            }

            if (a.isFloat || b.isFloat) {
                if (a.isFloat && b.isFloat)
                    return a.bits > b.bits ? a : b;

                if (a.isFloat) {
                    if (b.bits > a.bits)
                        return TypeInfo{
                            .bits = b.bits,
                            .align = b.bits / 8,
                            .isFloat = true,
                            .isSigned = true,
                            .isString = false,
                            .isBoolean = false,
                            .isPointer = false,
                            .isUserDefined = false,
                            .isFunction = false,
                            .name = (b.bits == 64 ? "double" : "float")};
                    else
                        return a;
                } else {
                    if (a.bits > b.bits)
                        return TypeInfo{
                            .bits = a.bits,
                            .align = a.bits / 8,
                            .isFloat = true,
                            .isSigned = true,
                            .isString = false,
                            .isBoolean = false,
                            .isPointer = false,
                            .isUserDefined = false,
                            .isFunction = false,
                            .name = (a.bits == 64 ? "double" : "float")};
                    else
                        return b;
                }
            }

            if (a.bits != b.bits)
                return a.bits > b.bits ? a : b;

            if (a.isSigned && !b.isSigned)
                return a;
            if (!a.isSigned && b.isSigned)
                return b;

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

}  // namespace zust