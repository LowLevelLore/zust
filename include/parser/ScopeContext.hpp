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
    };

    class ScopeContext
    {
    public:
        ScopeContext(std::shared_ptr<ScopeContext> parent = nullptr);

        void defineVariable(const std::string &name, const VariableInfo &info);
        void defineFunction(const std::string &name, const FunctionInfo &info);
        void defineType(const std::string &name, const TypeInfo &info);

        VariableInfo lookupVariable(const std::string &name) const;
        FunctionInfo lookupFunction(const std::string &name) const;
        TypeInfo lookupType(const std::string &name) const;
        std::shared_ptr<ScopeContext> parent_;

    private:
        std::unordered_map<std::string, VariableInfo> vars_;
        std::unordered_map<std::string, FunctionInfo> funcs_;
        std::unordered_map<std::string, TypeInfo> types_;
    };
}
