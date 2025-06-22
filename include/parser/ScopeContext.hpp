#pragma once
#include <map>
#include <memory>
#include <optional>
#include <stdexcept>
#include <string>
#include <unordered_map>
#include <vector>

#include "support/CommandLine.hpp"

namespace zust {
    struct VariableInfo {
        std::string type;
    };

    struct TypeInfo {
        std::uint32_t bits = 0;
        std::uint32_t align = 0;
        bool isFloat = false;
        bool isSigned = false;
        bool isString = false;
        bool isBoolean = false;
        bool isPointer = false;
        bool isUserDefined = false;
        bool isFunction = false;

        std::string name;

        std::string to_string() const;
    };

    struct ParamInfo {
        std::string name;
        std::string type;
        std::string to_string() const;
    };

    inline std::string ParamInfo::to_string() const {
        std::string ret = "ParamInfo( .name: " + name + ", .type: " + type + ")";
        return ret;
    }

    struct FunctionInfo {
        std::vector<ParamInfo> paramTypes;
        std::string returnType;
        std::string name;
        std::string label;
        bool isExtern;
        bool isVariadic;
        std::string to_string() const;
    };

    inline std::string FunctionInfo::to_string() const {
        std::string ret = "FunctionInfo( params: (";
        for (size_t i = 0; i < paramTypes.size(); ++i) {
            ret += paramTypes[i].to_string();
            if (i != paramTypes.size() - 1)
                ret += ", ";
        }
        ret += "), return: " + returnType + " )";
        return ret;
    }

    inline std::string TypeInfo::to_string() const {
        std::string ret =
            "TypeInfo( .name: " + name + ", .bits: " + std::to_string(bits) +
            ", .align: " + std::to_string(align) +
            ", .isFloat: " + (isFloat ? "true" : "false") +
            ", .isSigned: " + (isSigned ? "true" : "false") +
            ", .isBoolean: " + (isBoolean ? "true" : "false") +
            ", .isString: " + (isString ? "true" : "false") +
            ", .isPointer: " + (isPointer ? "true" : "false") +
            ", .isUserDefined: " + (isUserDefined ? "true" : "false") +
            ", .isFunction: " + (isFunction ? "true" : "false") + " )";
        return ret;
    }

    class FunctionScope;

    class ScopeContext : public std::enable_shared_from_this<ScopeContext> {
    public:
        ScopeContext(std::string name,
                     std::shared_ptr<ScopeContext> parent = nullptr)
            : name_(std::move(name)), parent_(std::move(parent)) {}
        virtual ~ScopeContext() = default;
        virtual std::string kind() const = 0;
        bool defineVariable(const std::string &name, const VariableInfo &info);
        void defineFunction(const std::string &name, FunctionInfo info);
        void defineType(const std::string &name, const TypeInfo &info);
        VariableInfo lookupVariable(const std::string &name) const;
        FunctionInfo lookupFunction(const std::string &name) const;
        TypeInfo lookupType(const std::string &name) const;
        std::optional<VariableInfo>
        lookupVariableInCurrentContext(const std::string &name) const;
        virtual std::int64_t allocateStack(const std::string &varName,
                                           const TypeInfo &type);
        std::int64_t getVariableOffset(const std::string &name) const;
        bool isGlobalScope() const;
        bool isGlobalVariable(const std::string &name) const;
        std::shared_ptr<ScopeContext> parent() const { return parent_; }
        const std::string &name() const { return name_; }
        std::string getMapping(std::string name);
        void setMapping(const std::string &name, const std::string &llvmName);
        virtual void printScope(std::ostream &out, int indent = 0) const;
        std::shared_ptr<FunctionScope> findEnclosingFunctionScope();
        std::shared_ptr<ScopeContext> getGlobal();
        std::string returnType = "none";

    protected:
        std::string name_;
        std::shared_ptr<ScopeContext> parent_;
        std::unordered_map<std::string, VariableInfo> vars_;
        std::unordered_map<std::string, FunctionInfo> funcs_;
        std::unordered_map<std::string, TypeInfo> types_;
        std::unordered_map<std::string, std::int64_t> offsetTable_;
        std::unordered_map<std::string, std::string> variable_name_mappings;
    };

    class FunctionScope : public ScopeContext {
    public:
        FunctionScope(std::string name,
                      std::shared_ptr<ScopeContext> parent = nullptr);
        ~FunctionScope() override;
        void printScope(std::ostream &out, int indent = 0) const override;
        inline std::string kind() const override { return "Function"; }
        std::int64_t allocateStack(const std::string &varName,
                                   const TypeInfo &type) override;
        std::int64_t getStackOffset() const;
        void setCanary(std::uint64_t canary_) {
            this->canary = canary_;
        }
        std::uint64_t getCanary() {
            return this->canary;
        }
        std::string allocateSpillSlot(std::int64_t size, CodegenOutputFormat format);
        std::int64_t getSpillSize() const;
        void freeSpillSlot(const std::string &slot, std::int64_t size);

    private:
        std::int64_t stackOffset_;
        std::uint64_t canary;
        std::int64_t nextSpillOffset_ = 0;
        std::vector<std::pair<std::int64_t, std::int64_t>> freeSpillSlots_;
        inline static std::int64_t alignSize(const TypeInfo &type) {
            return std::max<std::uint32_t>(type.align, 8);
        }
    };
    class BlockScope : public ScopeContext {
    public:
        BlockScope(std::string name, std::shared_ptr<FunctionScope> funcScope,
                   std::shared_ptr<ScopeContext> parent);
        ~BlockScope() override;
        void printScope(std::ostream &out, int indent = 0) const override;
        inline std::string kind() const override { return "Block"; }
        std::int64_t allocateStack(const std::string &varName,
                                   const TypeInfo &type) override;

    private:
        std::shared_ptr<FunctionScope> funcScope_;
    };

    class NamespaceScope : public ScopeContext {
    public:
        NamespaceScope(std::string name, std::shared_ptr<ScopeContext> parent = nullptr)
            : ScopeContext(std::move(name), std::move(parent)) {}

        std::string kind() const override { return "Namespace"; }

        std::int64_t allocateStack(const std::string &, const TypeInfo &) override {
            // namespaces don’t allocate stack space
            throw std::runtime_error("Namespaces cannot define stack variables");
        }
    };

}  // namespace zust
