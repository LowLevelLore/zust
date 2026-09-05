#include "parser/ScopeContext.hpp"

#include <ostream>

#include "parser/NameMapper.hpp"

namespace zust {
    namespace {
        // The name mapper mangles declaration names into globally-unique
        // labels/SSA names; every scope in one compiler invocation must share
        // the same counters, so this is the single instance for the process,
        // not a per-TU static (this used to live in include/all.hpp as
        // `static zust::NameMapper GLOBAL_NAME_MAPPER;` -- a namespace-scope
        // `static` in a header gives every translation unit its own copy;
        // harmless here only because this was the one .cpp file that ever
        // called into it, but still a footgun worth removing).
        NameMapper GLOBAL_NAME_MAPPER;

        // One counter for the whole compilation, same reasoning as the name
        // mapper above: every SymbolId must be unique across the entire
        // program, not just within one scope.
        std::uint32_t nextSymbolIdValue = 0;
        SymbolId allocateSymbolId() { return SymbolId{nextSymbolIdValue++}; }
    }  // namespace

    bool ScopeContext::defineVariable(const std::string &name, const VariableInfo &info) {
        if (!parent_ || (parent_->kind() == "Namespace" && kind() != "Function")) {
            if (lookupVariableInCurrentContext(name).has_value()) {
                return false;
            }
            TypeInfo ti = lookupType(info.type);
            VariableInfo recorded = info;
            recorded.symbolId = allocateSymbolId();
            vars_[name] = recorded;
            variable_name_mappings[name] = GLOBAL_NAME_MAPPER.mapVariable(name, name_);
            return true;
        } else {
            if (lookupVariableInCurrentContext(name).has_value()) {
                return false;
            }
            TypeInfo ti = lookupType(info.type);
            // Frame slots are no longer assigned here -- allocateStack runs
            // lazily, on first call to getVariableOffset (see that method).
            // Only the symbol identity is assigned at definition time.
            VariableInfo recorded = info;
            recorded.symbolId = allocateSymbolId();
            vars_[name] = recorded;
            variable_name_mappings[name] = GLOBAL_NAME_MAPPER.mapVariable(name, name_);
            return true;
        }
    }

    void ScopeContext::defineFunction(const std::string &name, FunctionInfo info) {
        if (!info.isExtern) {
            info.label = GLOBAL_NAME_MAPPER.mapFunction(name, name_);
        }
        info.symbolId = allocateSymbolId();
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
            if (this->kind() == "Function" && parent_->kind() == "Function" && parent_->parent_) {
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

    std::optional<VariableInfo> ScopeContext::lookupVariableInCurrentContext(const std::string &name) const {
        auto it = vars_.find(name);
        if (it != vars_.end()) {
            return it->second;
        }
        return std::nullopt;
    }

    std::int64_t ScopeContext::allocateStack(const std::string & /*varName*/, const TypeInfo & /*type*/) const {
        throw std::runtime_error("allocateStack not implemented for scope: " + kind());
    }

    std::int64_t ScopeContext::getVariableOffset(const std::string &name) const {
        // Is this variable actually defined in *this* scope? If so, this is
        // the scope responsible for its offset -- allocate lazily on first
        // request and cache it, exactly mirroring what defineVariable used
        // to do eagerly (see the M0-1 shadowing-fix comment on
        // FunctionScope::allocateStack: recording must happen in the
        // defining scope, never in the enclosing function scope, or a
        // shadowed outer variable's slot gets clobbered).
        auto varIt = vars_.find(name);
        if (varIt != vars_.end()) {
            SymbolId id = varIt->second.symbolId;
            auto offIt = offsetTable_.find(id);
            if (offIt != offsetTable_.end()) {
                return offIt->second;
            }
            TypeInfo ti = lookupType(varIt->second.type);
            std::int64_t offset = allocateStack(name, ti);
            offsetTable_[id] = offset;
            return offset;
        }
        if (parent_) {
            if (this->kind() == "Function" && parent_->kind() == "Function" && parent_->parent_) {
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

    bool ScopeContext::isGlobalScope() const {
        return parent_ == nullptr;
    }

    std::string ScopeContext::getMapping(std::string name) {
        auto it = variable_name_mappings.find(name);
        if (it != variable_name_mappings.end()) {
            return it->second;
        }
        if (parent_) {
            if (this->kind() == "Function" && parent_->kind() == "Function" && parent_->parent_) {
                return parent_->parent_->getMapping(name);
            }
            return parent_->getMapping(name);
        }
        throw std::runtime_error("Mapping not found for variable: " + name);
    }

    void ScopeContext::setMapping(const std::string &name, const std::string &llvmName) {
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
                out << pad << "    " << kv.first << " -> " << kv.second.returnType << "\n";
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

    FunctionScope::FunctionScope(std::string name, std::shared_ptr<ScopeContext> parent)
        : ScopeContext(std::move(name), std::move(parent)), stackOffset_(-16) {}

    FunctionScope::~FunctionScope() = default;

    std::int64_t FunctionScope::allocateStack(const std::string & /*varName*/, const TypeInfo &type) const {
        // Only hand out the slot; recording it belongs to the scope that is
        // *defining* the variable (see ScopeContext::defineVariable). A block
        // scope allocates out of its enclosing function's frame, so recording
        // the offset here under the bare name would clobber the entry for an
        // outer variable of the same name and make the shadowed one resolve to
        // the inner slot for the rest of the function.
        std::int64_t size = alignSize(type);
        stackOffset_ -= size;
        return stackOffset_;
    }

    void FunctionScope::printScope(std::ostream &out, int indent) const {
        // Use base implementation for printing variables, functions, and types
        ScopeContext::printScope(out, indent);
    }

    std::int64_t FunctionScope::getStackOffset() const {
        return stackOffset_;
    }

    std::int64_t FunctionScope::allocateSpillSlot(std::int64_t size) {
        for (auto it = freeSpillSlots_.begin(); it != freeSpillSlots_.end(); ++it) {
            if (it->second == size) {
                std::int64_t offset = it->first;
                freeSpillSlots_.erase(it);
                return offset;
            }
        }
        nextSpillOffset_ -= size;
        return nextSpillOffset_;
    }

    std::int64_t FunctionScope::getSpillSize() const {
        return nextSpillOffset_;
    }

    void FunctionScope::freeSpillSlot(std::int64_t offset, std::int64_t size) {
        freeSpillSlots_.emplace_back(offset, size);
    }

    BlockScope::BlockScope(std::string name, std::shared_ptr<FunctionScope> funcScope,
                           std::shared_ptr<ScopeContext> parent)
        : ScopeContext(std::move(name), std::move(parent)), funcScope_(std::move(funcScope)) {}

    BlockScope::~BlockScope() = default;

    std::int64_t BlockScope::allocateStack(const std::string &varName, const TypeInfo &type) const {
        return funcScope_->allocateStack(varName, type);
    }

    void BlockScope::printScope(std::ostream &out, int indent) const {
        std::string pad(indent, ' ');
        out << pad << kind() << " Scope: " << name_ << "\n";
        ScopeContext::printScope(out, indent + 2);
    }
}  // namespace zust
