#include <all.hpp>

namespace zlang
{
    ScopeContext::ScopeContext(std::shared_ptr<ScopeContext> parent)
        : parent_(parent) {}
    void ScopeContext::defineVariable(const std::string &name, const VariableInfo &info)
    {
        vars_[name] = info;
    }
    void ScopeContext::defineFunction(const std::string &name, const FunctionInfo &info)
    {
        funcs_[name] = info;
    }
    void ScopeContext::defineType(const std::string &name, const TypeInfo &info)
    {
        types_[name] = info;
    }
    VariableInfo ScopeContext::lookupVariable(const std::string &name) const
    {
        auto it = vars_.find(name);
        if (it != vars_.end())
            return it->second;
        if (parent_)
            return parent_->lookupVariable(name);
        throw std::runtime_error("Undefined variable '" + name + "'");
    }
    FunctionInfo ScopeContext::lookupFunction(const std::string &name) const
    {
        auto it = funcs_.find(name);
        if (it != funcs_.end())
            return it->second;
        if (parent_)
            return parent_->lookupFunction(name);
        throw std::runtime_error("Undefined function '" + name + "'");
    }
    TypeInfo ScopeContext::lookupType(const std::string &name) const
    {
        auto it = types_.find(name);
        if (it != types_.end())
            return it->second;
        if (parent_)
            return parent_->lookupType(name);
        throw std::runtime_error("Undefined type '" + name + "'");
    }
}
