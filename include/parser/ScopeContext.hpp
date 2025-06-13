#pragma once
#include <string>
#include <unordered_map>
#include <memory>
#include <stdexcept>
#include <vector>

namespace zlang
{

    struct VariableInfo
    {
        std::string type;
    };

    struct FunctionInfo
    {
        std::vector<std::string> paramTypes;
        std::string returnType;
    };

    struct TypeInfo
    {
        // details of user-defined types (e.g., struct/class definitions)
        // TODO
        std::uint32_t bits;
        std::uint32_t align;
        bool isFloat;
        bool isSigned;

        std::string to_string()
        {
            std::string ret = "";
            ret += "TypeInfo( .bits: " + std::to_string(bits) + ", .align: " + std::to_string(align) + ", .isFloat: " + (isFloat ? "true" : "false") + ", .isSigned: " + (isSigned ? "true" : "false") + " )\n";
            return ret;
        }
    };

    class ScopeContext
    {
    public:
        ScopeContext(std::shared_ptr<ScopeContext> parent = nullptr) : stackOffset(0), parent_(parent) {};

        std::int64_t stackOffset;
        std::shared_ptr<ScopeContext> parent_;
        void defineVariable(const std::string &name, const VariableInfo &info);
        void defineFunction(const std::string &name, const FunctionInfo &info);
        void defineType(const std::string &name, const TypeInfo &info);

        VariableInfo lookupVariable(const std::string &name) const;
        FunctionInfo lookupFunction(const std::string &name) const;
        TypeInfo lookupType(const std::string &name) const;
        std::unordered_map<std::string, std::int64_t> offsetTable;
        std::int64_t getVariableOffset(const std::string &name) const;
        bool isGlobalScope() const;
        bool isGlobalVariable(const std::string &name) const;
        void printGlobalContext() const
        {
            const ScopeContext *ctx = this;
            while (ctx->parent_)
                ctx = ctx->parent_.get();

            std::cout << "=== Global Scope ===\n";
            // Variables
            if (!ctx->vars_.empty())
            {
                std::cout << "Variables:\n";
                for (auto &kv : ctx->vars_)
                {
                    std::cout << "  " << kv.first << ": "
                              << kv.second.type << "\n";
                }
            }
            // Functions
            if (!ctx->funcs_.empty())
            {
                std::cout << "Functions:\n";
                for (auto &kv : ctx->funcs_)
                {
                    std::cout << "  " << kv.first << "(\n";
                    for (size_t i = 0; i < kv.second.paramTypes.size(); ++i)
                    {
                        std::cout << kv.second.paramTypes[i];
                        if (i + 1 < kv.second.paramTypes.size())
                            std::cout << ", ";
                    }
                    std::cout << ") -> " << kv.second.returnType << "\n";
                }
            }
            // Types
            if (!ctx->types_.empty())
            {
                std::cout << "Types:\n";
                for (auto &kv : ctx->types_)
                {
                    std::cout << "  " << kv.first
                              << " (" << kv.second.bits << " bits, align=" << kv.second.align
                              << ", " << (kv.second.isFloat ? "float" : "int")
                              << ")\n";
                }
            }
            std::cout << std::endl;
        }

    private:
        std::unordered_map<std::string, VariableInfo> vars_;
        std::unordered_map<std::string, FunctionInfo> funcs_;
        std::unordered_map<std::string, TypeInfo> types_;
    };
}
