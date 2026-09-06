#include "zirgen/ZirGen.hpp"

#include <cstring>
#include <stdexcept>

#include "zir/TypeRules.hpp"

namespace zust {

    using namespace zust::zir;

    namespace {
        std::string unescapeStringLiteral(const std::string &raw) {
            // The lexer keeps escapes raw (a literal backslash followed by
            // the next character, uninterpreted) -- this is the one place
            // that actually decodes them, matching the Windows/LLVM legacy
            // pipelines' escape set (docs/PRD-ZIR.md behavior inventory).
            std::string out;
            for (std::size_t i = 0; i < raw.size(); ++i) {
                if (raw[i] == '\\' && i + 1 < raw.size()) {
                    char c = raw[++i];
                    switch (c) {
                        case 'n':
                            out += '\n';
                            break;
                        case 't':
                            out += '\t';
                            break;
                        case 'r':
                            out += '\r';
                            break;
                        case '\\':
                            out += '\\';
                            break;
                        case '"':
                            out += '"';
                            break;
                        case '0':
                            out += '\0';
                            break;
                        default:
                            out += '\\';
                            out += c;
                            break;
                    }
                } else {
                    out += raw[i];
                }
            }
            return out;
        }
    }  // namespace

    zir::Module ZirGen::lower(const ASTNode &program, const std::string &sourceName) {
        Module m(sourceName);
        module_ = &m;
        globalScope_ = program.scope;
        typeCache_.clear();
        funcsByName_.clear();
        globalVars_.clear();
        hoistedTopLevel_.clear();

        std::vector<const ASTNode *> functionNodes;
        for (const auto &childPtr : program.children) {
            const ASTNode *child = childPtr.get();
            if (child->type == NodeType::VariableDeclaration) {
                registerGlobal(child);
                hoistedTopLevel_.push_back(child);
            } else if (child->type == NodeType::VariableReassignment ||
                       (child->type == NodeType::UnaryOp && (child->value == "++" || child->value == "--"))) {
                hoistedTopLevel_.push_back(child);
            } else if (child->type == NodeType::Function || child->type == NodeType::ExternFunction) {
                registerFunctionSignature(child);
                if (child->type == NodeType::Function)
                    functionNodes.push_back(child);
            } else {
                throw std::runtime_error("ZirGen: unsupported top-level construct");
            }
        }

        for (const ASTNode *fnNode : functionNodes) {
            lowerFunctionBody(fnNode);
        }

        module_ = nullptr;
        return m;
    }

    // ---------------------------------------------------------------------
    // Type mapping
    // ---------------------------------------------------------------------

    TypeId ZirGen::zirType(const std::string &legacyName) {
        auto it = typeCache_.find(legacyName);
        if (it != typeCache_.end())
            return it->second;

        TypeId result;
        if (legacyName == "none") {
            result = module_->types().voidType();
        } else if (legacyName == "boolean") {
            // Memory/declared representation is a full byte; SSA "logical"
            // boolean values are i1 (see toCondition/lowerBinaryOp) --
            // docs/PRD-ZIR.md's decided boolean representation.
            result = module_->types().intType(8, false);
        } else if (legacyName == "string") {
            result = module_->types().ptrType(module_->types().intType(8, true));
        } else {
            TypeInfo ti = globalScope_->lookupType(legacyName);
            result = ti.isFloat ? module_->types().floatType(ti.bits) : module_->types().intType(ti.bits, ti.isSigned);
        }
        typeCache_[legacyName] = result;
        return result;
    }

    // ---------------------------------------------------------------------
    // Top-level registration
    // ---------------------------------------------------------------------

    void ZirGen::registerGlobal(const ASTNode *decl) {
        VariableInfo info = decl->scope->lookupVariable(decl->value);
        GlobalVar g;
        g.name = decl->value;
        g.type = zirType(info.type);
        g.isPrivate = false;
        g.isConstant = false;
        g.hasInit = false;  // zero-initialized; the real initializer runs as a store inside main (rule 3)
        GlobalId gid = module_->addGlobal(g);
        globalVars_[info.symbolId] = gid;
    }

    void ZirGen::registerFunctionSignature(const ASTNode *fnOrExtern) {
        bool isExtern = fnOrExtern->type == NodeType::ExternFunction;
        FunctionInfo info = fnOrExtern->scope->lookupFunction(fnOrExtern->value);

        std::vector<TypeId> paramTypes;
        for (const ParamInfo &p : info.paramTypes) paramTypes.push_back(zirType(p.type));
        // `main` is always `-> none` at the language level, but every legacy
        // backend still emits `define i32 @main()` / `ret i32 0` for the
        // process exit code (CodeGenLLVM.cpp:734) -- ZIR's signature matches
        // that ABI reality, not the language-level "none".
        TypeId retTy = fnOrExtern->value == "main" ? module_->types().intType(32, true) : zirType(info.returnType);
        TypeId sig = module_->types().fnType(paramTypes, retTy, info.isVariadic);

        FuncId id = module_->addFunction(Function(fnOrExtern->value, sig, isExtern, info.isVariadic));
        funcsByName_[fnOrExtern->value] = id;
    }

    // ---------------------------------------------------------------------
    // Function body lowering
    // ---------------------------------------------------------------------

    void ZirGen::collectDeclarations(const ASTNode *node, std::vector<const ASTNode *> &out) {
        if (!node)
            return;
        if (node->type == NodeType::VariableDeclaration) {
            out.push_back(node);
            return;
        }
        if (node->type == NodeType::Function)
            return;  // nested functions get their own entry block; not supported this wave (unused by any test)
        for (const auto &child : node->children) collectDeclarations(child.get(), out);
    }

    void ZirGen::lowerFunctionBody(const ASTNode *fnNode) {
        FuncId id = funcsByName_.at(fnNode->value);
        fn_ = &module_->function(id);
        localAllocas_.clear();
        loopStack_.clear();
        blockCounter_ = 0;

        Builder builder(*module_, *fn_);
        builder_ = &builder;

        BlockId entry = builder_->createBlock("entry");
        fn_->setEntry(entry);
        setInsert(entry);

        const ASTNode *body = fnNode->getFunctionBody();
        const ASTNode *paramList = fnNode->getFunctionParamList();
        FunctionInfo info = fnNode->scope->lookupFunction(fnNode->value);
        currentReturnType_ = info.returnType;
        isMainFunction_ = fnNode->value == "main";
        currentZirReturnType_ = isMainFunction_ ? module_->types().intType(32, true) : zirType(currentReturnType_);

        // Parameter allocas.
        std::vector<TypeId> paramTypes;
        std::vector<SymbolId> paramSyms;
        std::vector<std::string> paramNames;
        for (const auto &paramChild : paramList->children) {
            const std::string &pname = paramChild->children[0]->value;
            VariableInfo pinfo = body->scope->lookupVariable(pname);
            TypeId pty = zirType(pinfo.type);
            ValueId ptr = builder_->alloca_(pty);
            localAllocas_[pinfo.symbolId] = ptr;
            paramTypes.push_back(pty);
            paramSyms.push_back(pinfo.symbolId);
            paramNames.push_back(pname);
        }

        // Every other local, at any nesting depth, gets its alloca up front
        // too -- alloca is only ever legal in the entry block (Verifier
        // check 6), so there is no other place to put it.
        std::vector<const ASTNode *> decls;
        collectDeclarations(body, decls);
        for (const ASTNode *d : decls) {
            VariableInfo dinfo = d->scope->lookupVariable(d->value);
            if (localAllocas_.count(dinfo.symbolId))
                continue;  // a parameter redeclared as a body-visible symbol shouldn't happen, but be defensive
            TypeId dty = zirType(dinfo.type);
            ValueId ptr = builder_->alloca_(dty);
            localAllocas_[dinfo.symbolId] = ptr;
        }

        // Entry block parameters are the incoming SSA values; spill each
        // into its alloca immediately -- "no cleverness" applies to
        // parameters exactly as it does to every other local.
        for (std::size_t i = 0; i < paramTypes.size(); ++i) {
            ValueId incoming = builder_->addBlockParam(entry, paramTypes[i]);
            fn_->setValueName(incoming, paramNames[i]);
            builder_->store(incoming, localAllocas_.at(paramSyms[i]));
        }

        if (fnNode->value == "main") {
            for (const ASTNode *h : hoistedTopLevel_) lowerStatement(h);
        }
        lowerStatement(body);

        if (!blockTerminated_) {
            if (isMainFunction_)
                emitRet(builder_->constInt(currentZirReturnType_, 0));  // rule 4: unconditional trailing `ret 0`
            else if (currentReturnType_ == "none")
                emitRetVoid();
            else
                emitUnreachableIfNotTerminated();  // missing return: matches today's undefined behavior, deliberately
        }

        builder_ = nullptr;
    }

    // ---------------------------------------------------------------------
    // Statements
    // ---------------------------------------------------------------------

    void ZirGen::lowerStatement(const ASTNode *node) {
        if (!node)
            return;
        switch (node->type) {
            case NodeType::Program:
                for (const auto &child : node->children) lowerStatement(child.get());
                return;
            case NodeType::VariableDeclaration:
                lowerVarDecl(node);
                return;
            case NodeType::VariableReassignment:
                lowerVarReassign(node);
                return;
            case NodeType::IfStatement:
                lowerIf(node);
                return;
            case NodeType::ForLoop:
                lowerFor(node);
                return;
            case NodeType::WhileLoop:
                lowerWhile(node);
                return;
            case NodeType::ReturnStatement:
                lowerReturn(node);
                return;
            case NodeType::BreakStatement:
                lowerBreak();
                return;
            case NodeType::ContinueStatement:
                lowerContinue();
                return;
            case NodeType::UnaryOp:
            case NodeType::FunctionCall:
            case NodeType::BinaryOp:
                lowerExpression(node);  // bare expression statement; discard the value
                return;
            default:
                throw std::runtime_error("ZirGen: unsupported statement kind");
        }
    }

    void ZirGen::lowerVarDecl(const ASTNode *node) {
        VariableInfo info = node->scope->lookupVariable(node->value);
        TypeId declType = zirType(info.type);
        ValueId ptr = addressOf(info.symbolId);

        // children[0] is the type-annotation Symbol (if present), children[1]
        // the initializer (if present) -- matches TypeChecker.cpp's own
        // indexing exactly. The legacy parser always synthesizes a default
        // initializer, so in practice both are always present.
        if (node->children.size() == 2) {
            ValueId val = lowerExpression(node->children[1].get());
            val = castTo(val, fn_->typeOf(val), declType);
            builder_->store(val, ptr);
        }
    }

    void ZirGen::lowerVarReassign(const ASTNode *node) {
        VariableInfo info = node->scope->lookupVariable(node->value);
        TypeId declType = zirType(info.type);
        ValueId ptr = addressOf(info.symbolId);
        ValueId val = lowerExpression(node->children[0].get());
        val = castTo(val, fn_->typeOf(val), declType);
        builder_->store(val, ptr);
    }

    void ZirGen::lowerIf(const ASTNode *node) {
        // Flatten if/elseif*/else? into a branch list sharing one
        // continuation block, rather than recursing (a naive recursive
        // lowering would give each elseif its own continuation, which is
        // wrong -- they must all funnel into the same one).
        std::vector<std::pair<const ASTNode *, const ASTNode *>> branches;
        const ASTNode *finalElse = nullptr;
        const ASTNode *cur = node;
        while (cur) {
            branches.emplace_back(cur->children[0].get(), cur->children[1].get());
            const ASTNode *eb = cur->getElseBranch();
            if (!eb) {
                cur = nullptr;
            } else if (eb->type == NodeType::ElseIfStatement) {
                cur = eb;
            } else {
                finalElse = eb->children.empty() ? nullptr : eb->children[0].get();
                cur = nullptr;
            }
        }

        BlockId contBlock = builder_->createBlock(freshLabel("endif"));
        for (std::size_t i = 0; i < branches.size(); ++i) {
            const ASTNode *condNode = branches[i].first;
            const ASTNode *bodyNode = branches[i].second;

            ValueId condVal = lowerExpression(condNode);
            ValueId boolCond = toCondition(condVal, fn_->typeOf(condVal));

            BlockId thenB = builder_->createBlock(freshLabel("then"));
            bool hasNext = (i + 1 < branches.size()) || finalElse != nullptr;
            BlockId elseB = hasNext ? builder_->createBlock(freshLabel("else")) : contBlock;
            emitCondBr(boolCond, thenB, elseB);

            setInsert(thenB);
            lowerStatement(bodyNode);
            emitBrIfNotTerminated(contBlock);

            setInsert(elseB);
        }
        if (finalElse) {
            lowerStatement(finalElse);
            emitBrIfNotTerminated(contBlock);
            setInsert(contBlock);
        }
        // If there was no finalElse, the loop's last iteration already left
        // the insertion point at elseB == contBlock.
    }

    void ZirGen::lowerFor(const ASTNode *node) {
        lowerStatement(node->children[0].get());  // init, runs in the current block

        BlockId condBlock = builder_->createBlock(freshLabel("for.cond"));
        BlockId bodyBlock = builder_->createBlock(freshLabel("for.body"));
        BlockId postBlock = builder_->createBlock(freshLabel("for.post"));
        BlockId endBlock = builder_->createBlock(freshLabel("for.end"));

        emitBr(condBlock);
        setInsert(condBlock);
        ValueId condVal = lowerExpression(node->children[1].get());
        ValueId boolCond = toCondition(condVal, fn_->typeOf(condVal));
        emitCondBr(boolCond, bodyBlock, endBlock);

        // continue -> post (still runs the increment); break -> end.
        loopStack_.push_back({postBlock, endBlock});
        setInsert(bodyBlock);
        lowerStatement(node->children[3].get());
        emitBrIfNotTerminated(postBlock);
        loopStack_.pop_back();

        setInsert(postBlock);
        lowerStatement(node->children[2].get());
        emitBrIfNotTerminated(condBlock);

        setInsert(endBlock);
    }

    void ZirGen::lowerWhile(const ASTNode *node) {
        BlockId condBlock = builder_->createBlock(freshLabel("while.cond"));
        BlockId bodyBlock = builder_->createBlock(freshLabel("while.body"));
        BlockId endBlock = builder_->createBlock(freshLabel("while.end"));

        emitBr(condBlock);
        setInsert(condBlock);
        ValueId condVal = lowerExpression(node->children[0].get());
        ValueId boolCond = toCondition(condVal, fn_->typeOf(condVal));
        emitCondBr(boolCond, bodyBlock, endBlock);

        // continue -> cond; break -> end.
        loopStack_.push_back({condBlock, endBlock});
        setInsert(bodyBlock);
        lowerStatement(node->children[1].get());
        emitBrIfNotTerminated(condBlock);
        loopStack_.pop_back();

        setInsert(endBlock);
    }

    void ZirGen::lowerReturn(const ASTNode *node) {
        bool isVoidReturn =
            node->children.empty() || (node->children[0]->type == NodeType::Symbol && node->children[0]->value == "none");
        if (isVoidReturn) {
            // A bare `return;` inside `main` still has to produce `ret i32 0`
            // -- main's ZIR signature is int32 regardless of its "none"
            // language-level type (see currentZirReturnType_).
            if (isMainFunction_)
                emitRet(builder_->constInt(currentZirReturnType_, 0));
            else
                emitRetVoid();
            return;
        }
        ValueId val = lowerExpression(node->children[0].get());
        val = castTo(val, fn_->typeOf(val), currentZirReturnType_);
        emitRet(val);
    }

    void ZirGen::lowerBreak() {
        if (loopStack_.empty())
            throw std::runtime_error("ZirGen: 'break' outside any loop");
        emitBr(loopStack_.back().breakTarget);
    }

    void ZirGen::lowerContinue() {
        if (loopStack_.empty())
            throw std::runtime_error("ZirGen: 'continue' outside any loop");
        emitBr(loopStack_.back().continueTarget);
    }

    // ---------------------------------------------------------------------
    // Expressions
    // ---------------------------------------------------------------------

    ValueId ZirGen::lowerExpression(const ASTNode *node) {
        switch (node->type) {
            case NodeType::IntegerLiteral: {
                TypeId ty = zirType("integer");
                return builder_->constInt(ty, static_cast<std::uint64_t>(std::stoll(node->value)));
            }
            case NodeType::FloatLiteral: {
                bool isF32 = !node->value.empty() && (node->value.back() == 'f' || node->value.back() == 'F');
                TypeId ty = zirType(isF32 ? "float" : "double");
                double d = std::stod(node->value);  // stod tolerates a trailing f/F suffix being absent; strip it first
                std::string numeric = node->value;
                if (isF32)
                    numeric.pop_back();
                d = std::stod(numeric);
                std::uint64_t bits;
                if (isF32) {
                    float f = static_cast<float>(d);
                    std::uint32_t b32;
                    std::memcpy(&b32, &f, sizeof(f));
                    bits = b32;
                } else {
                    std::memcpy(&bits, &d, sizeof(d));
                }
                return builder_->constFloatBits(ty, bits);
            }
            case NodeType::BooleanLiteral:
                return builder_->constInt(module_->types().boolType(), node->value == "true" ? 1 : 0);
            case NodeType::StringLiteral:
                return lowerStringLiteral(node);
            case NodeType::VariableAccess:
                return lowerVariableAccess(node);
            case NodeType::BinaryOp:
                return lowerBinaryOp(node);
            case NodeType::UnaryOp:
                return lowerUnaryOp(node);
            case NodeType::FunctionCall:
                return lowerFunctionCall(node);
            default:
                throw std::runtime_error("ZirGen: cannot lower node as an expression");
        }
    }

    ValueId ZirGen::lowerVariableAccess(const ASTNode *node) {
        VariableInfo info = node->scope->lookupVariable(node->value);
        TypeId declType = zirType(info.type);
        ValueId ptr = addressOf(info.symbolId);
        ValueId loaded = builder_->load(declType, ptr);
        if (info.type == "boolean") {
            loaded = builder_->cast(Opcode::Trunc, loaded, module_->types().boolType());
        }
        return loaded;
    }

    ValueId ZirGen::lowerStringLiteral(const ASTNode *node) {
        std::string bytes = unescapeStringLiteral(node->value);
        bytes.push_back('\0');

        GlobalVar g;
        g.name = ".str" + std::to_string(module_->globals().size());
        g.type = module_->types().arrayType(module_->types().intType(8, true), bytes.size());
        g.isPrivate = true;
        g.isConstant = true;
        g.hasInit = true;
        g.initBytes = bytes;
        GlobalId gid = module_->addGlobal(g);
        return builder_->globalAddr(gid);
    }

    ValueId ZirGen::lowerBinaryOp(const ASTNode *node) {
        const std::string &op = node->value;
        ValueId lhs = lowerExpression(node->children[0].get());
        ValueId rhs = lowerExpression(node->children[1].get());
        TypeId lty = fn_->typeOf(lhs);
        TypeId rty = fn_->typeOf(rhs);

        if (op == "&&" || op == "||") {
            // Both operands are "boolean"-typed per the type checker, but
            // not necessarily i1 yet -- a variable access truncs to i1
            // itself, but e.g. a boolean-returning function call yields i8
            // (its declared memory representation), so normalize through
            // toCondition first. Deliberately a plain and/or on the result,
            // never short-circuiting (docs/PRD-ZIR.md behavior inventory).
            ValueId lc = toCondition(lhs, lty);
            ValueId rc = toCondition(rhs, rty);
            return builder_->binop(op == "&&" ? Opcode::And : Opcode::Or, module_->types().boolType(), lc, rc);
        }

        TypeId common = TypeRules::promote(module_->types(), lty, rty);
        ValueId l2 = castTo(lhs, lty, common);
        ValueId r2 = castTo(rhs, rty, common);
        const Type &ct = module_->types().get(common);
        bool isFloat = ct.kind == TypeKind::Float;

        if (op == "==" || op == "!=" || op == ">=" || op == ">" || op == "<=" || op == "<") {
            if (isFloat) {
                CmpPred p = op == "==" ? CmpPred::Oeq
                            : op == "!=" ? CmpPred::One
                            : op == ">=" ? CmpPred::Oge
                            : op == ">"  ? CmpPred::Ogt
                            : op == "<=" ? CmpPred::Ole
                                         : CmpPred::Olt;
                return builder_->fcmp(p, module_->types().boolType(), l2, r2);
            }
            bool s = ct.isSigned;
            CmpPred p = op == "==" ? CmpPred::Eq
                        : op == "!=" ? CmpPred::Ne
                        : op == ">=" ? (s ? CmpPred::Sge : CmpPred::Uge)
                        : op == ">"  ? (s ? CmpPred::Sgt : CmpPred::Ugt)
                        : op == "<=" ? (s ? CmpPred::Sle : CmpPred::Ule)
                                     : (s ? CmpPred::Slt : CmpPred::Ult);
            return builder_->icmp(p, module_->types().boolType(), l2, r2);
        }

        if (op == "+" || op == "-" || op == "*" || op == "/") {
            if (isFloat) {
                Opcode fo = op == "+" ? Opcode::FAdd : op == "-" ? Opcode::FSub : op == "*" ? Opcode::FMul : Opcode::FDiv;
                return builder_->binop(fo, common, l2, r2);
            }
            Opcode io;
            if (op == "+")
                io = Opcode::Add;
            else if (op == "-")
                io = Opcode::Sub;
            else if (op == "*")
                io = Opcode::Mul;
            else
                io = ct.isSigned ? Opcode::SDiv : Opcode::UDiv;
            return builder_->binop(io, common, l2, r2);
        }

        throw std::runtime_error("ZirGen: unsupported binary operator '" + op + "'");
    }

    ValueId ZirGen::lowerUnaryOp(const ASTNode *node) {
        const std::string &op = node->value;
        if (op == "!") {
            ValueId v = lowerExpression(node->children[0].get());
            ValueId cond = toCondition(v, fn_->typeOf(v));
            return builder_->unop(Opcode::Not, module_->types().boolType(), cond);
        }
        if (op == "++" || op == "--") {
            const ASTNode *target = node->children[0].get();
            VariableInfo info = target->scope->lookupVariable(target->value);
            TypeId ty = zirType(info.type);
            ValueId ptr = addressOf(info.symbolId);
            ValueId oldVal = builder_->load(ty, ptr);
            ValueId one = builder_->constInt(ty, 1);
            ValueId newVal = builder_->binop(op == "++" ? Opcode::Add : Opcode::Sub, ty, oldVal, one);
            builder_->store(newVal, ptr);
            return newVal;  // ++x/x++ both yield the *new* value in this language
        }
        throw std::runtime_error("ZirGen: unsupported unary operator '" + op + "'");
    }

    ValueId ZirGen::lowerFunctionCall(const ASTNode *node) {
        FunctionInfo info = node->scope->lookupFunction(node->value);
        FuncId callee = funcsByName_.at(node->value);
        const ASTNode *argList = node->children[0].get();

        std::vector<ValueId> args;
        for (std::size_t i = 0; i < argList->children.size(); ++i) {
            ValueId val = lowerExpression(argList->children[i].get());
            TypeId fromTy = fn_->typeOf(val);
            if (i < info.paramTypes.size()) {
                TypeId toTy = zirType(info.paramTypes[i].type);
                val = castTo(val, fromTy, toTy);
            } else {
                // Variadic tail: promote per the legacy default-promotion
                // rule (float -> double, everything else -> int64_t).
                const Type &t = module_->types().get(fromTy);
                TypeId toTy = t.kind == TypeKind::Float ? module_->types().floatType(64) : module_->types().intType(64, true);
                val = castTo(val, fromTy, toTy);
            }
            args.push_back(val);
        }

        if (info.returnType == "none") {
            builder_->callVoid(callee, args);
            return ValueId{};
        }
        TypeId retTy = zirType(info.returnType);
        return builder_->call(callee, retTy, args);
    }

    // ---------------------------------------------------------------------
    // Helpers
    // ---------------------------------------------------------------------

    ValueId ZirGen::addressOf(SymbolId sym) {
        auto git = globalVars_.find(sym);
        if (git != globalVars_.end())
            return builder_->globalAddr(git->second);
        auto lit = localAllocas_.find(sym);
        if (lit != localAllocas_.end())
            return lit->second;
        throw std::runtime_error("ZirGen: reference to a symbol with no known address");
    }

    ValueId ZirGen::castTo(ValueId val, TypeId fromTy, TypeId toTy) {
        if (fromTy == toTy)
            return val;
        const Type &from = module_->types().get(fromTy);
        const Type &to = module_->types().get(toTy);

        if (from.kind == TypeKind::Int && to.kind == TypeKind::Int) {
            if (to.bits > from.bits)
                return builder_->cast(from.isSigned ? Opcode::SExt : Opcode::ZExt, val, toTy);
            if (to.bits < from.bits)
                return builder_->cast(Opcode::Trunc, val, toTy);
            return builder_->cast(Opcode::Bitcast, val, toTy);  // same width, different signedness: just retag
        }
        if (from.kind == TypeKind::Float && to.kind == TypeKind::Float) {
            if (to.bits > from.bits)
                return builder_->cast(Opcode::FPExt, val, toTy);
            if (to.bits < from.bits)
                return builder_->cast(Opcode::FPTrunc, val, toTy);
            return builder_->cast(Opcode::Bitcast, val, toTy);
        }
        if (from.kind == TypeKind::Int && to.kind == TypeKind::Float) {
            return builder_->cast(from.isSigned ? Opcode::SIToFP : Opcode::UIToFP, val, toTy);
        }
        if (from.kind == TypeKind::Float && to.kind == TypeKind::Int) {
            return builder_->cast(to.isSigned ? Opcode::FPToSI : Opcode::FPToUI, val, toTy);
        }
        if (from.kind == TypeKind::Ptr && to.kind == TypeKind::Ptr) {
            // ZIR pointers print as an opaque "ptr" regardless of pointee
            // (Printer::printType), so a pointer-to-array (a string
            // literal's GlobalAddr) decaying to the declared pointer-to-byte
            // parameter type is just a retag, exactly like C's array-to-
            // pointer decay -- no real conversion happens at any backend.
            return builder_->cast(Opcode::Bitcast, val, toTy);
        }
        throw std::runtime_error("ZirGen: unsupported implicit cast");
    }

    ValueId ZirGen::toCondition(ValueId val, TypeId ty) {
        if (ty == module_->types().boolType())
            return val;  // already i1 -- avoid a redundant icmp
        const Type &t = module_->types().get(ty);
        if (t.kind == TypeKind::Float) {
            ValueId zero = builder_->constFloatBits(ty, 0);
            return builder_->fcmp(CmpPred::One, module_->types().boolType(), val, zero);
        }
        ValueId zero = builder_->constInt(ty, 0);
        return builder_->icmp(CmpPred::Ne, module_->types().boolType(), val, zero);
    }

    std::string ZirGen::freshLabel(const char *prefix) {
        return std::string(prefix) + std::to_string(blockCounter_++);
    }

    void ZirGen::setInsert(BlockId block) {
        builder_->setInsertBlock(block);
        blockTerminated_ = false;
    }

    void ZirGen::emitBr(BlockId target) {
        if (blockTerminated_)
            return;
        builder_->br(target);
        blockTerminated_ = true;
    }

    void ZirGen::emitCondBr(ValueId cond, BlockId thenB, BlockId elseB) {
        if (blockTerminated_)
            return;
        builder_->condBr(cond, thenB, {}, elseB, {});
        blockTerminated_ = true;
    }

    void ZirGen::emitBrIfNotTerminated(BlockId target) { emitBr(target); }

    void ZirGen::emitRet(ValueId v) {
        if (blockTerminated_)
            return;
        builder_->ret(v);
        blockTerminated_ = true;
    }

    void ZirGen::emitRetVoid() {
        if (blockTerminated_)
            return;
        builder_->retVoid();
        blockTerminated_ = true;
    }

    void ZirGen::emitUnreachableIfNotTerminated() {
        if (blockTerminated_)
            return;
        builder_->unreachable();
        blockTerminated_ = true;
    }

}  // namespace zust
