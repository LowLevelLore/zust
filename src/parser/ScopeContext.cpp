#include <all.hpp>

namespace zlang
{
    void ScopeContext::defineVariable(const std::string &name, const VariableInfo &info)
    {

        TypeInfo ti = lookupType(info.type);
        int size = ti.bits / 8;
        int padding = (ti.align - (stackOffset % ti.align)) % ti.align;
        stackOffset += padding;
        stackOffset += size;
        offsetTable[name] = -stackOffset;
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

    std::uint64_t ScopeContext::getVariableOffset(const std::string &name) const
    {
        auto it = offsetTable.find(name);
        if (it != offsetTable.end())
            return it->second;
        if (parent_)
            return parent_->getVariableOffset(name);
        throw std::runtime_error("Unknown variable " + name);
    }

    bool ScopeContext::isGlobalScope()
    {
        return !this->parent_.get();
    }
}
