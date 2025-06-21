#include <all.hpp>

namespace zlang {
    bool ScopeContext::defineVariable(const std::string &name,
                                      const VariableInfo &info) {
        if (!parent_ || (parent_->kind() == "Namespace" && kind() != "Function")) {
            if (lookupVariableInCurrentContext(name).has_value()) {
                return false;
            }
            TypeInfo ti = lookupType(info.type);
            vars_[name] = info;
            variable_name_mappings[name] = GLOBAL_NAME_MAPPER.mapVariable(name, name_);
            return true;
        } else {
            if (lookupVariableInCurrentContext(name).has_value()) {
                return false;
            }
            TypeInfo ti = lookupType(info.type);
            std::int64_t offset = allocateStack(name, ti);
            vars_[name] = info;
            offsetTable_[name] = offset;
            variable_name_mappings[name] = GLOBAL_NAME_MAPPER.mapVariable(name, name_);
            return true;
        }
    }
    void ScopeContext::defineFunction(const std::string &name, FunctionInfo info) {
        if (!info.isExtern) {
            info.label = GLOBAL_NAME_MAPPER.mapFunction(name, name_);
        }
        funcs_[name] = info;
    }
    void ScopeContext::defineType(const std::string &name, const TypeInfo &info) {
        types_[name] = info;
    }
    VariableInfo ScopeContext::lookupVariable(const std::string &name) const {
        auto it = vars_.find(name);
        if (it != vars_.end()) {
            return it->second;
        }
        if (parent_) {
            // Skip outer function locals if crossing function boundary
            if (this->kind() == "Function" && parent_->kind() == "Function" &&
                parent_->parent_) {
                return parent_->parent_->lookupVariable(name);
            }
            return parent_->lookupVariable(name);
        }
        throw std::runtime_error("Undefined variable: " + name);
    }
    FunctionInfo ScopeContext::lookupFunction(const std::string &name) const {
        auto it = funcs_.find(name);
        if (it != funcs_.end()) {
            return it->second;
        }
        if (parent_) {
            return parent_->lookupFunction(name);
        }
        throw std::runtime_error("Undefined function: " + name);
    }
    TypeInfo ScopeContext::lookupType(const std::string &name) const {
        auto it = types_.find(name);
        if (it != types_.end()) {
            return it->second;
        }
        if (parent_) {
            return parent_->lookupType(name);
        }
        throw std::runtime_error("Undefined type: " + name);
    }
    std::optional<VariableInfo>
    ScopeContext::lookupVariableInCurrentContext(const std::string &name) const {
        auto it = vars_.find(name);
        if (it != vars_.end()) {
            return it->second;
        }
        return std::nullopt;
    }
    std::int64_t ScopeContext::allocateStack(const std::string & /*varName*/,
                                             const TypeInfo & /*type*/) {
        throw std::runtime_error("allocateStack not implemented for scope: " +
                                 kind());
    }
    std::int64_t ScopeContext::getVariableOffset(const std::string &name) const {
        auto it = offsetTable_.find(name);
        if (it != offsetTable_.end()) {
            return it->second;
        }
        if (parent_) {
            if (this->kind() == "Function" && parent_->kind() == "Function" &&
                parent_->parent_) {
                return parent_->parent_->getVariableOffset(name);
            }
            return parent_->getVariableOffset(name);
        }
        throw std::runtime_error("Unknown variable: " + name);
    }
    bool ScopeContext::isGlobalVariable(const std::string &name) const {
        const ScopeContext *ctx = this;
        while (ctx) {
            auto it = ctx->vars_.find(name);
            if (it != ctx->vars_.end()) {
                return ctx->isGlobalScope();
            }
            ctx = ctx->parent_.get();
        }
        throw std::runtime_error("Unknown variable: " + name);
    }
    bool ScopeContext::isGlobalScope() const { return parent_ == nullptr; }
    std::string ScopeContext::getMapping(std::string name) {
        auto it = variable_name_mappings.find(name);
        if (it != variable_name_mappings.end()) {
            return it->second;
        }
        if (parent_) {
            if (this->kind() == "Function" && parent_->kind() == "Function" &&
                parent_->parent_) {
                return parent_->parent_->getMapping(name);
            }
            return parent_->getMapping(name);
        }
        throw std::runtime_error("Mapping not found for variable: " + name);
    }
    void ScopeContext::setMapping(const std::string &name,
                                  const std::string &llvmName) {
        variable_name_mappings[name] = llvmName;
    }
    void ScopeContext::printScope(std::ostream &out, int indent) const {
        std::string pad(indent, ' ');
        out << pad << kind() << " Scope: " << name_ << "\n";

        if (!vars_.empty()) {
            out << pad << "  Variables:\n";
            for (const auto &kv : vars_) {
                out << pad << "    " << kv.first << ": " << kv.second.type << "\n";
            }
        }

        if (!funcs_.empty()) {
            out << pad << "  Functions:\n";
            for (const auto &kv : funcs_) {
                out << pad << "    " << kv.first << " -> " << kv.second.returnType
                    << "\n";
            }
        }

        if (!types_.empty()) {
            out << pad << "  Types:\n";
            for (const auto &kv : types_) {
                out << pad << "    " << kv.first << "\n";
            }
        }
    }
    std::shared_ptr<FunctionScope> ScopeContext::findEnclosingFunctionScope() {
        std::shared_ptr<ScopeContext> current = shared_from_this();
        while (current) {
            if (current->kind() == "Function") {
                return std::static_pointer_cast<FunctionScope>(current);
            }
            current = current->parent_;
        }
        return nullptr;
    }
    std::shared_ptr<ScopeContext> ScopeContext::getGlobal() {
        if (!parent_) {
            return shared_from_this();
        } else {
            return parent_->getGlobal();
        }
    }

    FunctionScope::FunctionScope(std::string name,
                                 std::shared_ptr<ScopeContext> parent)
        : ScopeContext(std::move(name), std::move(parent)), stackOffset_(-8) {}
    FunctionScope::~FunctionScope() = default;
    std::int64_t FunctionScope::allocateStack(const std::string &varName,
                                              const TypeInfo &type) {
        std::int64_t size = alignSize(type);
        stackOffset_ -= size;
        offsetTable_[varName] = stackOffset_;
        return stackOffset_;
    }
    void FunctionScope::printScope(std::ostream &out, int indent) const {
        // Use base implementation for printing variables, functions, and types
        ScopeContext::printScope(out, indent);
    }
    std::int64_t FunctionScope::getStackOffset() const {
        return stackOffset_;
    }
    std::string FunctionScope::allocateSpillSlot(std::int64_t size, CodegenOutputFormat format) {
        for (auto it = freeSpillSlots_.begin(); it != freeSpillSlots_.end(); ++it) {
            if (it->second == size) {
                std::int64_t offset = it->first;
                freeSpillSlots_.erase(it);
                switch (format) {
                case CodegenOutputFormat::X86_64_LINUX:
                    return "-" + std::to_string(offset) + "(%rbp)";
                case CodegenOutputFormat::X86_64_MSWIN:
                    return "[rbp - " + std::to_string(stackOffset_ + offset) + "]";
                default:
                    return "";
                }
            }
        }
        nextSpillOffset_ -= size;
        std::int64_t offset = nextSpillOffset_;
        switch (format) {
        case CodegenOutputFormat::X86_64_LINUX:
            return std::to_string(offset) + "(%rbp)";
        case CodegenOutputFormat::X86_64_MSWIN:
            return "[rbp - " + std::to_string(stackOffset_ + offset) + "]";
        default:
            return "";
        }
    }
    std::int64_t FunctionScope::getSpillSize() const {
        return nextSpillOffset_;
    }
    void FunctionScope::freeSpillSlot(const std::string &slot, std::int64_t size) {
        auto pos = slot.find('(');
        if (pos == std::string::npos)
            throw std::runtime_error("Invalid spill slot format: " + slot);
        std::string offsetStr = slot.substr(1, pos - 1);  // skip '-'
        std::int64_t offset = std::stoll(offsetStr);
        freeSpillSlots_.emplace_back(offset, size);
    }

    BlockScope::BlockScope(std::string name,
                           std::shared_ptr<FunctionScope> funcScope,
                           std::shared_ptr<ScopeContext> parent)
        : ScopeContext(std::move(name), std::move(parent)),
          funcScope_(std::move(funcScope)) {}
    BlockScope::~BlockScope() = default;
    std::int64_t BlockScope::allocateStack(const std::string &varName,
                                           const TypeInfo &type) {
        return funcScope_->allocateStack(varName, type);
    }
    void BlockScope::printScope(std::ostream &out, int indent) const {
        std::string pad(indent, ' ');
        out << pad << kind() << " Scope: " << name_ << "\n";
        ScopeContext::printScope(out, indent + 2);
    }
}  // namespace zlang
