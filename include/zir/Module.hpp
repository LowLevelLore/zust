#pragma once

#include <stdexcept>
#include <string>
#include <unordered_map>
#include <vector>

#include "zir/Ids.hpp"
#include "zir/Instruction.hpp"
#include "zir/Types.hpp"

// Module -> Function -> BasicBlock -> Instruction, per docs/IR-DESIGN.md
// "Shape". Everything is arena-allocated and index-addressed: a BasicBlock
// holds InstIds into its owning Function's instruction arena, not
// Instructions themselves, so nothing here holds a pointer/iterator that a
// vector growth could invalidate.

namespace zust::zir {

    class BasicBlock {
    public:
        explicit BasicBlock(std::string label) : label_(std::move(label)) {}

        const std::string &label() const { return label_; }

        std::vector<ValueId> &params() { return params_; }

        const std::vector<ValueId> &params() const { return params_; }

        std::vector<InstId> &insts() { return insts_; }

        const std::vector<InstId> &insts() const { return insts_; }

        Terminator &term() { return term_; }

        const Terminator &term() const { return term_; }

    private:
        std::string label_;
        std::vector<ValueId> params_;
        std::vector<InstId> insts_;
        Terminator term_;
    };

    // A function AND an extern declaration are the same object here: the
    // only difference is whether `blocks_` is empty. This is deliberate --
    // both are addressed by the same FuncId space and called the same way
    // (`call <ret> @name(...)`), and keeping one array avoids two arrays
    // that could each independently produce e.g. FuncId(0).
    class Function {
    public:
        Function(std::string name, TypeId signature, bool isExtern, bool isVariadic)
            : name_(std::move(name)), signature_(signature), isExtern_(isExtern), isVariadic_(isVariadic) {}

        const std::string &name() const { return name_; }

        TypeId signature() const { return signature_; }

        bool isExtern() const { return isExtern_; }

        bool isVariadic() const { return isVariadic_; }

        BlockId entry() const { return entry_; }

        void setEntry(BlockId b) { entry_ = b; }

        BlockId addBlock(std::string label) {
            blocks_.emplace_back(std::move(label));
            return BlockId(static_cast<BlockId::Value>(blocks_.size() - 1));
        }

        std::size_t blockCount() const { return blocks_.size(); }

        BasicBlock &block(BlockId id) {
            if (!id.isValid() || id.value() >= blocks_.size())
                throw std::runtime_error("Function::block: invalid BlockId");
            return blocks_[id.value()];
        }

        const BasicBlock &block(BlockId id) const {
            if (!id.isValid() || id.value() >= blocks_.size())
                throw std::runtime_error("Function::block: invalid BlockId");
            return blocks_[id.value()];
        }

        // Every ValueId a function ever produces (block param or instruction
        // result) gets its type recorded here at creation time -- this is
        // what lets a verifier or a pass ask "what type is %v" without
        // walking back to find its definition first.
        ValueId newValue(TypeId type) {
            valueTypes_.push_back(type);
            return ValueId(static_cast<ValueId::Value>(valueTypes_.size() - 1));
        }

        TypeId typeOf(ValueId v) const {
            if (!v.isValid() || v.value() >= valueTypes_.size())
                throw std::runtime_error("Function::typeOf: invalid ValueId");
            return valueTypes_[v.value()];
        }

        std::size_t valueCount() const { return valueTypes_.size(); }

        void setValueName(ValueId v, std::string name) { valueNames_[v] = std::move(name); }

        // The printer's name for a value: the user-assigned name if one was
        // set (e.g. "n" for source variable `n`, printed as "%n"), else a
        // synthesized "v<N>" (printed as "%v3").
        std::string nameOf(ValueId v) const {
            auto it = valueNames_.find(v);
            if (it != valueNames_.end())
                return it->second;
            return "v" + std::to_string(v.value());
        }

        InstId addInst(BlockId block, Instruction inst) {
            insts_.push_back(std::move(inst));
            InstId id(static_cast<InstId::Value>(insts_.size() - 1));
            this->block(block).insts().push_back(id);
            return id;
        }

        Instruction &inst(InstId id) {
            if (!id.isValid() || id.value() >= insts_.size())
                throw std::runtime_error("Function::inst: invalid InstId");
            return insts_[id.value()];
        }

        const Instruction &inst(InstId id) const {
            if (!id.isValid() || id.value() >= insts_.size())
                throw std::runtime_error("Function::inst: invalid InstId");
            return insts_[id.value()];
        }

        std::size_t instCount() const { return insts_.size(); }

    private:
        std::string name_;
        TypeId signature_;
        bool isExtern_;
        bool isVariadic_;
        BlockId entry_;
        std::vector<BasicBlock> blocks_;
        std::vector<Instruction> insts_;
        std::vector<TypeId> valueTypes_;
        std::unordered_map<ValueId, std::string> valueNames_;
    };

    // A module-level variable. `type` is the variable's own type (e.g. the
    // array type for a string constant), addressed via `@name`, not a
    // pointer-to-that-type -- matching the textual form's
    // `@.str0 = private constant [4 x i8] c"..."`.
    struct GlobalVar {
        std::string name;
        TypeId type;
        bool isPrivate = false;
        bool isConstant = false;
        bool hasInit = false;
        // Only string-byte initializers are needed today (the one case the
        // textual form spec shows); a real constant-value system (arrays,
        // structs, nested constants) is M6+ scope.
        std::string initBytes;
    };

    class Module {
    public:
        explicit Module(std::string sourceName, std::string targetName = "generic")
            : sourceName_(std::move(sourceName)), targetName_(std::move(targetName)) {}

        const std::string &sourceName() const { return sourceName_; }

        const std::string &targetName() const { return targetName_; }

        TypeTable &types() { return types_; }

        const TypeTable &types() const { return types_; }

        TargetLayout &layout() { return layout_; }

        const TargetLayout &layout() const { return layout_; }

        GlobalId addGlobal(GlobalVar g) {
            globals_.push_back(std::move(g));
            return GlobalId(static_cast<GlobalId::Value>(globals_.size() - 1));
        }

        const GlobalVar &global(GlobalId id) const {
            if (!id.isValid() || id.value() >= globals_.size())
                throw std::runtime_error("Module::global: invalid GlobalId");
            return globals_[id.value()];
        }

        const std::vector<GlobalVar> &globals() const { return globals_; }

        FuncId addFunction(Function fn) {
            functions_.push_back(std::move(fn));
            return FuncId(static_cast<FuncId::Value>(functions_.size() - 1));
        }

        Function &function(FuncId id) {
            if (!id.isValid() || id.value() >= functions_.size())
                throw std::runtime_error("Module::function: invalid FuncId");
            return functions_[id.value()];
        }

        const Function &function(FuncId id) const {
            if (!id.isValid() || id.value() >= functions_.size())
                throw std::runtime_error("Module::function: invalid FuncId");
            return functions_[id.value()];
        }

        const std::vector<Function> &functions() const { return functions_; }

    private:
        std::string sourceName_;
        std::string targetName_;
        TypeTable types_;
        TargetLayout layout_;
        std::vector<GlobalVar> globals_;
        std::vector<Function> functions_;
    };

}  // namespace zust::zir
