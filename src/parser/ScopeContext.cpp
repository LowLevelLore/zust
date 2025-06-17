#include <all.hpp>

namespace zlang
{
    bool ScopeContext::defineVariable(const std::string &varName, const VariableInfo &info)
    {
        if (lookupVariableInCurrentContext(varName).has_value())
        {
            return false;
        }
        else
        {
            TypeInfo ti = lookupType(info.type);
            int size = ti.bits / 8;
            name_mappings[varName] = varName + "___" + this->name + "___" + std::to_string(variableNumber++);
            int padding = (ti.align - (stackOffset % ti.align)) % ti.align;
            stackOffset += padding;
            stackOffset += size;
            offsetTable[varName] = -stackOffset;
            vars_[varName] = info;
            return true;
        }
    }

    void ScopeContext::defineFunction(const std::string &functionName, const FunctionInfo &info)
    {
        funcs_[functionName] = info;
    }

    void ScopeContext::defineType(const std::string &typeName, const TypeInfo &info)
    {
        types_[typeName] = info;
    }

    VariableInfo ScopeContext::lookupVariable(const std::string &varName) const
    {
        auto it = vars_.find(varName);
        if (it != vars_.end())
            return it->second;
        if (parent_)
            return parent_->lookupVariable(varName);
        throw std::runtime_error("Undefined variable '" + varName + "'");
    }

    std::string ScopeContext::getMapping(std::string varName)
    {
        try
        {
            lookupVariable(varName);
            auto it = vars_.find(varName);
            if (it != vars_.end())
            {
                if (name_mappings.find(varName) != name_mappings.end())
                {
                    return name_mappings[varName];
                }
                else
                {
                    throw std::runtime_error("Mapping not found for variable '" + varName + "', in scope named '" + name + "'.");
                }
            }
            if (parent_)
                return parent_->getMapping(varName);
        }
        catch (...)
        {
            throw std::runtime_error("Undefined variable '" + varName + "'");
        }
        return "";
    }

    std::optional<VariableInfo> ScopeContext::lookupVariableInCurrentContext(const std::string &varName) const
    {
        auto it = vars_.find(varName);
        if (it != vars_.end())
            return it->second;
        return std::nullopt;
    }

    FunctionInfo ScopeContext::lookupFunction(const std::string &functionName) const
    {
        auto it = funcs_.find(functionName);
        if (it != funcs_.end())
            return it->second;
        if (parent_)
            return parent_->lookupFunction(functionName);
        throw std::runtime_error("Undefined function '" + functionName + "'");
    }

    TypeInfo ScopeContext::lookupType(const std::string &typeName) const
    {
        auto it = types_.find(typeName);
        if (it != types_.end())
            return it->second;
        if (parent_)
            return parent_->lookupType(typeName);
        throw std::runtime_error("Undefined type '" + typeName + "'");
    }

    std::int64_t ScopeContext::getVariableOffset(const std::string &varName) const
    {
        auto it = offsetTable.find(varName);
        if (it != offsetTable.end())
            return it->second;
        if (parent_)
            return parent_->getVariableOffset(varName);
        throw std::runtime_error("Unknown variable " + varName);
    }

    bool ScopeContext::isGlobalVariable(const std::string &varName) const
    {

        const ScopeContext *ctx = this;
        while (ctx)
        {
            auto it = ctx->vars_.find(varName);
            if (it != ctx->vars_.end())
            {
                return (ctx->parent_ == nullptr);
            }
            ctx = ctx->parent_.get();
        }
        throw std::runtime_error("Unknown variable " + varName);
    }

    bool ScopeContext::isGlobalScope() const
    {
        return !this->parent_.get();
    }
}
