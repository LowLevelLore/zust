#include "all.hpp"

namespace zust
{

    std::string llvmTypeName(const std::string &typeName)
    {
        if (typeName == "boolean")
            return "i8"; // Boolean as 1-bit integer
        if (typeName == "none")
            return "void"; // No type
        if (typeName == "string")
            return "i8*"; // Pointer to characters
        if (typeName == "size_t")
            return "i64"; // Platform word size
        if (typeName == "integer")
            return "i64"; // Default signed integer

        // Unsigned integers
        if (typeName == "uint8_t")
            return "i8";
        if (typeName == "uint16_t")
            return "i16";
        if (typeName == "uint32_t")
            return "i32";
        if (typeName == "uint64_t")
            return "i64";

        // Signed integers
        if (typeName == "int8_t")
            return "i8";
        if (typeName == "int16_t")
            return "i16";
        if (typeName == "int32_t")
            return "i32";
        if (typeName == "int64_t")
            return "i64";

        // Floating-point types
        if (typeName == "float")
            return "float";
        if (typeName == "double")
            return "double";

        // Fallback for user-defined or pointer types
        return typeName;
    }

    std::string unescapeString(const std::string &input)
    {
        std::string result;
        result.reserve(input.size());

        for (size_t i = 0; i < input.size(); ++i)
        {
            if (input[i] == '\\' && i + 1 < input.size())
            {
                switch (input[++i])
                {
                case 'n':
                    result += '\n';
                    break;
                case 't':
                    result += '\t';
                    break;
                case 'r':
                    result += '\r';
                    break;
                case '\\':
                    result += '\\';
                    break;
                case '"':
                    result += '"';
                    break;
                case '0':
                    result += '\0';
                    break; // Null byte
                default:   // Keep unrecognized escapes as-is
                    result += '\\';
                    result += input[i];
                    break;
                }
            }
            else
            {
                result += input[i];
            }
        }
        return result;
    }

    static std::string fresh()
    {
        static int cnt = 0;
        return "%tmp" + std::to_string(cnt++);
    }
    std::string toHexFloatFromStr(const std::string &valStr, bool isF32, std::ostringstream &out)
    {
        std::ostringstream oss;
        oss << std::scientific << std::setprecision(16);
        if (isF32)
        {
            float v = std::stof(valStr);
            oss << v;
        }
        else
        {
            double v = std::stod(valStr);
            oss << v;
        }
        return oss.str();
    }
    std::string CodeGenLLVM::castValue(const std::string &val, const TypeInfo &fromType, const TypeInfo &toType, std::ostringstream &out)
    {
        if (fromType.isFloat == toType.isFloat && fromType.bits == toType.bits)
        {
            return val;
        }

        std::string tmp = fresh();

        if (!fromType.isFloat && toType.isFloat)
        {
            std::string intTy = "i" + std::to_string(fromType.bits);
            std::string floatTy = (toType.bits == 64 ? "double" : "float");
            std::string instr = fromType.isSigned ? "sitofp" : "uitofp";
            out << "  " << tmp << " = " << instr << " " << intTy << " " << val << " to " << floatTy << "\n";
        }
        else if (fromType.isFloat && !toType.isFloat)
        {
            std::string floatTy = (fromType.bits == 64 ? "double" : "float");
            std::string intTy = "i" + std::to_string(toType.bits);
            std::string instr = toType.isSigned ? "fptosi" : "fptoui";
            out << "  " << tmp << " = " << instr << " " << floatTy << " " << val << " to " << intTy << "\n";
        }
        else if (!fromType.isFloat && !toType.isFloat)
        {
            std::string fromTy = "i" + std::to_string(fromType.bits);
            std::string toTy = "i" + std::to_string(toType.bits);
            if (fromType.bits < toType.bits)
            {
                std::string instr = fromType.isSigned ? "sext" : "zext";
                out << "  " << tmp << " = " << instr << " " << fromTy << " " << val << " to " << toTy << "\n";
            }
            else if (fromType.bits > toType.bits)
            {
                out << "  " << tmp << " = trunc " << fromTy << " " << val << " to " << toTy << "\n";
            }
            else if (fromType.isSigned != toType.isSigned)
            {
                // Retag value if signedness differs but bit-width is the same
                out << "  " << tmp << " = add " << fromTy << " " << val << ", 0\n";
            }
            else
            {
                return val;
            }
        }
        else if (fromType.isFloat && toType.isFloat)
        {
            std::string fromTy = (fromType.bits == 64 ? "double" : "float");
            std::string toTy = (toType.bits == 64 ? "double" : "float");
            if (fromType.bits < toType.bits)
            {
                out << "  " << tmp << " = fpext " << fromTy << " " << val << " to " << toTy << "\n";
            }
            else if (fromType.bits > toType.bits)
            {
                out << "  " << tmp << " = fptrunc " << fromTy << " " << val << " to " << toTy << "\n";
            }
            else
            {
                return val;
            }
        }
        else
        {
            throw std::runtime_error("Unsupported cast from type to type");
        }

        noteType(tmp, toType);
        return tmp;
    }
    std::string CodeGenLLVM::generateIntegerLiteral(std::unique_ptr<ASTNode> node, std::ostringstream &out)
    {
        std::string name = fresh();
        out << "  " << name << " = add i64 0, " << node->value << "\n";
        noteType(name, node->scope->lookupType("int64_t"));
        return name;
    }
    std::string CodeGenLLVM::generateFloatLiteral(std::unique_ptr<ASTNode> node, std::ostringstream &out)
    {
        std::string tmp = fresh();
        std::string val = node->value;
        bool isF32 = (!val.empty() && (val.back() == 'f' || val.back() == 'F'));
        if (isF32)
            val.pop_back();

        std::string hexVal = toHexFloatFromStr(val, isF32, out);

        if (isF32)
        {
            out << "  " << tmp << " = fadd float 0.0, " << hexVal << "\n";
            noteType(tmp, node->scope->lookupType("float"));
        }
        else
        {
            out << "  " << tmp << " = fadd double 0.0, " << hexVal << "\n";
            noteType(tmp, node->scope->lookupType("double"));
        }
        return tmp;
    }
    std::string CodeGenLLVM::generateStringLiteral(std::unique_ptr<ASTNode> node, std::ostringstream &out)
    {
        std::string value = node->value;

        std::string name = "@.str" + std::to_string(stringLabelCount++);
        std::string unescaped = unescapeString(value);
        size_t total_bytes = unescaped.size() + 1; // +1 for null terminator

        std::string llvmEscaped;
        for (unsigned char c : unescaped)
        {
            if (isprint(c) && c != '"' && c != '\\')
            {
                llvmEscaped += c;
            }
            else
            {
                char buf[5];
                snprintf(buf, sizeof(buf), "\\%02x", c);
                llvmEscaped += buf;
            }
        }
        llvmEscaped += "\\00";

        outGlobalStream << name << " = private unnamed_addr constant ["
                        << total_bytes << " x i8] c\"" << llvmEscaped << "\"\n";

        std::string ptr = fresh();
        out << "  " << ptr << " = getelementptr inbounds ["
            << total_bytes << " x i8], [" << total_bytes << " x i8]* "
            << name << ", i64 0, i64 0\n";

        // Cast pointer to i64 if storing in i64* variable
        std::string casted = fresh();
        out << "  " << casted << " = ptrtoint i8* " << ptr << " to i64\n";

        noteType(casted, node->scope->lookupType("int64_t")); // casted is now i64
        stringLiterals[value] = casted;
        return casted;
    }
    std::string CodeGenLLVM::generateBooleanLiteral(std::unique_ptr<ASTNode> node, std::ostringstream &out)
    {
        std::string name = fresh();
        out << "  " << name << " = add i8 0, "
            << (node->value == "true" ? "1" : "0") << "\n";
        noteType(name, node->scope->lookupType("boolean"));
        return name;
    }
    std::string CodeGenLLVM::generateVariableAccess(std::unique_ptr<ASTNode> node, std::ostringstream &out)
    {
        auto &scope = *node->scope;
        auto name = node->value;

        // Lookup type info
        auto ti = scope.lookupType(scope.lookupVariable(name).type);
        std::string ty = ti.isFloat
                             ? (ti.bits == 32 ? "float" : "double")
                             : "i" + std::to_string(ti.bits);

        // Determine if it's a global or local variable
        bool isGlobal = scope.isGlobalVariable(name);
        std::string ptr;
        if (!isGlobal)
        {
            ptr = "%" + node->scope->getMapping(name);
        }
        else
        {
            ptr = "@" + name;
        }

        // Generate load instruction
        std::string loaded = fresh();
        out << "  " << loaded << " = load " << ty << ", " << ty << "* " << ptr << "\n";

        noteType(loaded, ti);
        return loaded;
    }
    std::string CodeGenLLVM::generateBinaryOperation(std::unique_ptr<ASTNode> node, std::ostringstream &out)
    {
        auto lhs = emitExpression(std::move(node->children[0]), out);
        auto rhs = emitExpression(std::move(node->children[1]), out);
        TypeInfo t1 = regType[lhs];
        TypeInfo t2 = regType[rhs];
        TypeInfo tr = TypeChecker::promoteType(t1, t2);
        std::string L = castValue(lhs, t1, tr, out);
        std::string R = castValue(rhs, t2, tr, out);
        std::string res = fresh();

        if (tr.isFloat)
        {
            std::string target = (tr.bits == 64 ? "double" : "float");
            static const std::unordered_map<std::string, std::string> fp_ops = {{"+", "fadd"}, {"-", "fsub"}, {"*", "fmul"}, {"/", "fdiv"}};
            static const std::unordered_map<std::string, std::string> fcmp_ops = {{"==", "oeq"}, {"!=", "one"}, {"<", "olt"}, {"<=", "ole"}, {">", "ogt"}, {">=", "oge"}};

            if (fp_ops.count(node->value))
            {
                out << "  " << res << " = " << fp_ops.at(node->value) << " " << target << " " << L << ", " << R << "\n";
                noteType(res, tr);
                return res;
            }
            else
            {
                std::string cmpop = fcmp_ops.at(node->value);
                out << "  " << res << " = fcmp " << cmpop << " " << target << " " << L << ", " << R << "\n";
                std::string zext = fresh();
                out << "  " << zext << " = zext i1 " << res << " to i8\n";
                noteType(zext, node->scope->lookupType("boolean"));
                return zext;
            }
        }
        else
        {
            std::string intTy = "i" + std::to_string(tr.bits);
            bool isSigned = tr.isSigned;

            if (node->value == "+" || node->value == "-" || node->value == "*" || node->value == "/")
            {
                std::string op;
                if (node->value == "+")
                    op = "add";
                else if (node->value == "-")
                    op = "sub";
                else if (node->value == "*")
                    op = "mul";
                else /* "/" */
                    op = (isSigned ? "sdiv" : "udiv");

                out << "  " << res << " = " << op << " " << intTy << " " << L << ", " << R << "\n";
                noteType(res, tr);
                return res;
            }

            static const std::unordered_map<std::string, std::pair<std::string, std::string>> cmp_map = {
                {"==", {"eq", "eq"}}, {"!=", {"ne", "ne"}}, {"<", {"slt", "ult"}}, {"<=", {"sle", "ule"}}, {">", {"sgt", "ugt"}}, {">=", {"sge", "uge"}}};

            auto it = cmp_map.find(node->value);
            if (it != cmp_map.end())
            {
                std::string signedOp = it->second.first;
                std::string unsignedOp = it->second.second;
                std::string cmpop = isSigned ? signedOp : unsignedOp;

                out << "  " << res << " = icmp " << cmpop << " " << intTy << " " << L << ", " << R << "\n";
                std::string zext = fresh();
                out << "  " << zext << " = zext i1 " << res << " to i8\n";
                noteType(zext, node->scope->lookupType("boolean"));
                return zext;
            }

            throw std::runtime_error("Unsupported integer op " + node->value);
        }
    }
    std::string CodeGenLLVM::generateUnaryOperation(std::unique_ptr<ASTNode> node, std::ostringstream &out)
    {
        auto &scope = *node->children[0]->scope;
        auto varName = node->children[0]->value;
        TypeInfo ti = scope.lookupType(scope.lookupVariable(varName).type);
        NodeType child_type = node->children[0]->type;
        auto val = emitExpression(std::move(node->children[0]), out);

        std::string llvmType;
        if (ti.isFloat)
            llvmType = (ti.bits == 32) ? "float" : "double";
        else
            llvmType = "i" + std::to_string(ti.bits);

        if (node->value == "!")
        {
            TypeInfo boolType = node->scope->lookupType("boolean");
            val = castValue(val, regType.at(val), boolType, out);
            std::string res = fresh();
            out << "  " << res << " = icmp eq " << llvmType << " " << val << ", 0\n";
            std::string zero = fresh();
            out << "  " << zero << " = zext i1 " << res << " to i8\n";
            noteType(zero, node->scope->lookupType("boolean"));
            return zero;
        }
        else if (node->value == "++" || node->value == "--")
        {
            if (child_type != NodeType::VariableAccess)
                throw std::runtime_error(node->value + " can only be applied to variables");

            if (ti.isFloat)
                throw std::runtime_error("Increment/Decrement not supported on float");

            bool isGlobal = scope.isGlobalVariable(varName);
            std::string ptr = (isGlobal ? "@" + varName : "%" + node->scope->getMapping(varName));

            std::string cur = fresh();
            out << "  " << cur << " = load " << llvmType << ", " << llvmType << "* " << ptr << "\n";

            std::string updated = fresh();
            out << "  " << updated << " = "
                << (node->value == "++" ? "add" : "sub")
                << " " << llvmType << " " << cur << ", 1\n";

            out << "  store " << llvmType << " " << updated << ", " << llvmType << "* " << ptr << "\n";

            noteType(updated, ti);
            return updated;
        }
        else
        {
            throw std::runtime_error("Unsupported unary: " + node->value);
        }
    }
    std::string CodeGenLLVM::emitExpression(std::unique_ptr<ASTNode> node, std::ostringstream &out)
    {
        switch (node->type)
        {
        case NodeType::IntegerLiteral:
            return generateIntegerLiteral(std::move(node), out);
        case NodeType::FloatLiteral:
            return generateFloatLiteral(std::move(node), out);
        case NodeType::StringLiteral:
            return generateStringLiteral(std::move(node), out);
        case NodeType::BooleanLiteral:
            return generateBooleanLiteral(std::move(node), out);
        case NodeType::VariableAccess:
            return generateVariableAccess(std::move(node), out);
        case NodeType::BinaryOp:
            return generateBinaryOperation(std::move(node), out);
        case NodeType::UnaryOp:
            return generateUnaryOperation(std::move(node), out);
        case NodeType::FunctionCall:
            return generateFunctionCall(std::move(node), out);
        default:
            throw std::runtime_error("Unknown statement encountered.");
        }
    }
    void CodeGenLLVM::emitEpilogue(std::shared_ptr<ScopeContext> scope, std::ostringstream &out, bool clearRax)
    {
        out << "    ; Block ends\n";
    }
    void CodeGenLLVM::emitPrologue(std::shared_ptr<ScopeContext> scope, std::ostringstream &out)
    {
    }
    void CodeGenLLVM::generateStatement(std::unique_ptr<ASTNode> statement, std::ostringstream &out)
    {
        switch (statement->type)
        {
        case NodeType::VariableReassignment:
        {
            generateVariableReassignment(std::move(statement), out);
            out << "\n";
            break;
        }
        case NodeType::VariableDeclaration:
        {
            generateVariableDeclaration(std::move(statement), out);
            out << "\n";
            break;
        }
        case NodeType::IfStatement:
        {
            generateIfStatement(std::move(statement), out);
            out << "\n";
            break;
        }
        case NodeType::UnaryOp:
        {
            std::string op = statement->value;
            std::string reg = emitExpression(std::move(statement), out);
            out << "\n";
            break;
        }
        case NodeType::BinaryOp:
        {
            std::string reg = emitExpression(std::move(statement), out); // I am doing this just so the increments/decrements work in x + y-- -> this itself must not have any result, but y-- should still be effective.
            out << "\n";
            break;
        }
        case NodeType::FunctionCall:
        {
            std::string reg = generateFunctionCall(std::move(statement), out);
            out << "\n";
            break;
        }
        case NodeType::Function:
        {
            generateFunctionDeclaration(std::move(statement), out);
            out << "\n";
            break;
        }
        case NodeType::ExternFunction:
        {
            generateExternFunctionDeclaration(std::move(statement), out);
            out << "\n";
            break;
        }
        case NodeType::ReturnStatement:
        {
            generateReturnstatement(std::move(statement), out);
            out << "\n";
            break;
        }
        default:
            statement->print(std::cout, 0);
            throw std::runtime_error("Unknown statement encountered.");
        }
    }
    void CodeGenLLVM::generateVariableReassignment(std::unique_ptr<ASTNode> node, std::ostringstream &out)
    {
        auto &scope = *node->scope;
        auto name = node->value;
        TypeInfo ti = scope.lookupType(scope.lookupVariable(name).type);
        bool isGlobal = scope.isGlobalVariable(name);

        // 1) Compute the RHS expression
        std::string val = emitExpression(std::move(node->children.back()), out);
        TypeInfo tr = regType[val];

        // 2) Convert value to target type (ti) using castValue
        std::string castedVal = castValue(val, tr, ti, out);

        // 3) Emit the store to the correct location
        std::string ty = ti.isFloat
                             ? (ti.bits == 32 ? "float" : "double")
                             : "i" + std::to_string(ti.bits);

        if (isGlobal)
        {
            out << "  store " << ty << " " << castedVal << ", " << ty << "* @" << name << "\n";
        }
        else
        {
            out << "  store " << ty << " " << castedVal << ", " << ty << "* %" << node->scope->getMapping(name) << "\n";
        }
    }
    void CodeGenLLVM::generateVariableDeclaration(std::unique_ptr<ASTNode> node, std::ostringstream &out)
    {
        bool isGlobal = node->scope->isGlobalVariable(node->value);
        TypeInfo ti = node->scope->lookupType(node->scope->lookupVariable(node->value).type);
        std::string ty = ti.isFloat
                             ? (ti.bits == 32 ? "float" : "double")
                             : "i" + std::to_string(ti.bits);

        if (!isGlobal)
        {
            out << "  %" << node->scope->getMapping(node->value) << " = alloca " << ty << "\n";
        }

        if (node->children.size() >= 2)
        {
            auto val = emitExpression(std::move(node->children.back()), out);
            TypeInfo tr = regType[val];

            std::string castedVal = castValue(val, tr, ti, out);

            if (isGlobal)
            {
                out << "  store " << ty << " " << castedVal << ", " << ty << "* @" << node->value << "\n";
            }
            else
            {
                out << "  store " << ty << " " << castedVal << ", " << ty << "* %" << node->scope->getMapping(node->value) << "\n";
            }
        }
    }
    void CodeGenLLVM::generateIfStatement(std::unique_ptr<ASTNode> statement, std::ostringstream &out)
    {
        int id = blockLabelCount++;
        std::string thenLbl = "if.then" + std::to_string(id);
        std::string elseLbl = "if.else" + std::to_string(id);
        std::string endLbl = "if.end" + std::to_string(id);

        auto condVal = emitExpression(std::move(statement->children[0]), out);
        TypeInfo condTi = regType[condVal];

        std::string condBool = fresh();
        out << "    " << condBool
            << " = trunc i" << condTi.bits
            << " " << condVal << " to i1\n";

        out << "    br i1 " << condBool
            << ", label %" << thenLbl
            << ", label %"
            << (statement->getElseBranch() ? elseLbl : endLbl)
            << "\n\n";

        out << thenLbl << ":\n";
        {
            auto ifBlock = std::move(statement->children[1]);
            auto children = std::move(ifBlock->children);
            emitPrologue(ifBlock->scope, out);
            for (auto &stmt : children)
                generateStatement(std::move(stmt), out);
            emitEpilogue(ifBlock->scope, out);
        }
        out << "    br label %" << endLbl << "\n\n";

        ASTNode *branch = statement->getElseBranch();
        if (branch)
        {
            out << elseLbl << ":\n";

            while (branch)
            {
                if (branch->type == NodeType::ElseIfStatement)
                {
                    int elifId = blockLabelCount++;
                    std::string elifThen = "elif.then" + std::to_string(elifId);
                    std::string elifNext = "elif.next" + std::to_string(elifId);

                    auto elifCond = emitExpression(std::move(branch->children[0]), out);
                    TypeInfo elifTi = regType[elifCond];
                    std::string elifBool = fresh();
                    out << "    " << elifBool
                        << " = trunc i" << elifTi.bits
                        << " " << elifCond << " to i1\n";
                    out << "    br i1 " << elifBool
                        << ", label %" << elifThen
                        << ", label %" << elifNext
                        << "\n\n";

                    out << elifThen << ":\n";
                    {
                        auto elifBlock = std::move(branch->children[1]);
                        auto elifChildren = std::move(elifBlock->children);
                        emitPrologue(elifBlock->scope, out);
                        for (auto &stmt : elifChildren)
                            generateStatement(std::move(stmt), out);
                        emitEpilogue(elifBlock->scope, out);
                    }
                    out << "    br label %" << endLbl << "\n\n";
                    out << elifNext << ":\n";
                    branch = branch->getElseBranch();
                    if (!branch)
                    {
                        out << "    br label %" << endLbl << "\n\n";
                    }
                }
                else if (branch->type == NodeType::ElseStatement)
                {
                    auto elseBlock = std::move(branch->children[0]);
                    auto elseChildren = std::move(elseBlock->children);
                    emitPrologue(elseBlock->scope, out);
                    for (auto &stmt : elseChildren)
                        generateStatement(std::move(stmt), out);
                    emitEpilogue(elseBlock->scope, out);
                    out << "    br label %" << endLbl << "\n\n";
                    branch = nullptr;
                }
                else
                {
                    throw std::runtime_error("Unexpected node type in else chain");
                }
            }
        }

        // 5) end label
        out << endLbl << ":\n";
    }
    void CodeGenLLVM::generateForLoop(std::unique_ptr<ASTNode> node, std::ostringstream &out)
    {
        // TODO: implement loop codegen
    }

    void CodeGenLLVM::generateWhileLoop(std::unique_ptr<ASTNode> node, std::ostringstream &out)
    {
        // TODO: implement loop codegen
    }
    void CodeGenLLVM::generate(std::unique_ptr<ASTNode> program)
    {
        outGlobalStream << "; ModuleID = 'zust'\n"
                        << "source_filename = \"zust\"\n\n";

        // Globals
        for (auto &stmt : program->children)
        {
            if (stmt->type == NodeType::VariableDeclaration)
            {
                const auto &name = stmt->value;
                auto vi = stmt->scope->lookupVariable(name);
                TypeInfo ti = stmt->scope->lookupType(vi.type);
                std::string ty = llvmTypeName(ti.name);
                outGlobalStream << "@" << name << " = global " << ty
                                << (ti.isFloat ? " 0.0" : (ti.name == "string" ? " null" : " 0"))
                                << "\n";
            }
        }
        outGlobalStream << "\n";

        // Split main and decls
        std::unique_ptr<ASTNode> mainFunction;
        std::vector<std::unique_ptr<ASTNode>> declarationsAndReassignments;
        for (auto &stmt : program->children)
        {
            if (stmt->type == NodeType::Function && stmt->value == "main")
            {
                mainFunction = std::move(stmt);
            }
            else if (stmt->type == NodeType::VariableDeclaration || stmt->type == NodeType::VariableReassignment || (stmt->type == NodeType::UnaryOp and (stmt->value == "++" || stmt->value == "--")))
            {
                declarationsAndReassignments.push_back(std::move(stmt));
            }
            else
            {
                generateStatement(std::move(stmt), outStream);
            }
        }

        // Emit main
        outStream << "define i32 @main() {\n";
        // Decls in main
        for (auto &s : declarationsAndReassignments)
        {
            generateStatement(std::move(s), outStream);
        }

        for (auto &c : mainFunction->getFunctionBody()->children)
            generateStatement(std::move(c), outStream);
        outStream << "  ret i32 0\n";
        outStream << "}\n";

        outfinal << outGlobalStream.str() << "\n"
                 << outStream.str() << "\n";
    }

    std::string CodeGenLLVM::generateFunctionCall(std::unique_ptr<ASTNode> node, std::ostringstream &out)
    {
        // Lookup function metadata
        auto fnInfo = node->scope->lookupFunction(node->value);
        const std::string &funcName = fnInfo.isExtern ? node->value : fnInfo.label;
        auto &args = node->children[0]->children;
        const auto &paramTypes = fnInfo.paramTypes;

        // Emit each argument, casting to expected type
        std::vector<std::pair<std::string, std::string>> argList;
        for (size_t i = 0; i < args.size(); ++i)
        {
            // Evaluate expression
            std::string src = emitExpression(std::move(args[i]), out);

            out << "; Source: " << src << " \n";

            // Determine types
            TypeInfo passed = regType.at(src);
            bool passedIsFloat = passed.isFloat;
            TypeInfo expect;
            if (i < paramTypes.size())
            {
                expect = node->scope->lookupType(paramTypes[i].type);
            }
            else if (fnInfo.isVariadic)
            {
                expect = node->scope->lookupType(passedIsFloat ? "double" : "int64_t");
            }
            else
            {
                throw std::runtime_error("Too many arguments for function '" + node->value + "'");
            }

            // Cast value
            std::string cvt = castValue(src, passed, expect, out);

            // Determine LLVM type
            std::string ty = llvmTypeName(expect.name);
            if (expect.isPointer)
            {
                std::string tmpPtr = fresh();
                out << "  " << tmpPtr << " = inttoptr i64 " << cvt << " to " << llvmTypeName(expect.name) << "\n";
                cvt = tmpPtr;
            }

            argList.emplace_back(ty, cvt);
        }

        // Prepare call result
        TypeInfo rti = node->scope->lookupType(fnInfo.returnType);
        std::string retTy = llvmTypeName(rti.name);
        bool isVariadic = fnInfo.isVariadic;

        std::string callRes;
        if (retTy != "void")
        {
            callRes = fresh();
            out << "  " << callRes << " = ";
        }
        else
        {
            out << "  ";
        }

        // Emit call
        out << "call " << retTy;
        if (isVariadic && !argList.empty())
        {
            out << " (" << argList[0].first << ", ...)";
        }
        out << " @" << funcName << "(";
        for (size_t i = 0; i < argList.size(); ++i)
        {
            if (i)
                out << ", ";
            out << argList[i].first << " " << argList[i].second;
        }
        out << ")\n";

        noteType(callRes, node->scope->lookupType(fnInfo.returnType));

        return callRes;
    }
    void CodeGenLLVM::generateFunctionDeclaration(std::unique_ptr<ASTNode> node, std::ostringstream &out, bool force)
    {
        const std::string &name = node->value;
        bool isMain = (name == "main");
        if (isMain && !force)
            return; // skip main if not forced
        node->print(std::cout, 0);
        auto fnInfo = node->scope->lookupFunction(name);
        auto bodyNode = node->getFunctionBody();
        auto funcScope = std::static_pointer_cast<FunctionScope>(bodyNode->scope->findEnclosingFunctionScope());

        // Function signature
        TypeInfo rti = node->scope->lookupType(fnInfo.returnType);
        std::string retTy = llvmTypeName(rti.name);
        out << "define " << retTy << " @" << fnInfo.label << "(";
        // Parameters
        for (size_t i = 0; i < fnInfo.paramTypes.size(); ++i)
        {
            const auto &p = fnInfo.paramTypes[i];
            TypeInfo pti = node->scope->lookupType(p.type);
            out << llvmTypeName(pti.name) << " %" << bodyNode->scope->getMapping(p.name);
            if (i + 1 < fnInfo.paramTypes.size())
                out << ", ";
        }
        out << ") {\n";

        // Emit prologue
        emitPrologue(funcScope, out);

        // TODO: We were here last night, now we take one day break.

        // Map arguments into LLVM locals
        for (const auto &p : fnInfo.paramTypes)
        {
            TypeInfo pti = node->scope->lookupType(p.type);
            std::string allocaReg = fresh() + "___arg___";
            out << "  " << allocaReg << " = alloca "
                << llvmTypeName(pti.name) << "\n";
            out << "  store " << llvmTypeName(pti.name)
                << " %" << bodyNode->scope->getMapping(p.name)
                << ", " << llvmTypeName(pti.name)
                << "* " << allocaReg << "\n";
            funcScope->setMapping(p.name, allocaReg.substr(1)); // or pass without '%' if your getMapping adds it
        }

        // Function body
        std::ostringstream body;
        std::vector<std::unique_ptr<ASTNode>> nested;
        for (auto &stmt : bodyNode->children)
        {
            if (stmt->type == NodeType::Function)
            {
                nested.push_back(std::move(stmt));
            }
            else
            {
                generateStatement(std::move(stmt), body);
            }
        }
        out << body.str();

        // Return or void
        if (retTy != "void")
        {
        }
        else
        {
            out << "  ret void\n";
        }
        out << "}\n\n";

        // Nested functions
        for (auto &nf : nested)
        {
            generateFunctionDeclaration(std::move(nf), out, false);
        }
    }
    void CodeGenLLVM::generateExternFunctionDeclaration(std::unique_ptr<ASTNode> node, std::ostringstream &out)
    {
        auto fnInfo = node->scope->lookupFunction(node->value);
        // Build a `declare` line:
        //   declare <retTy> @<label>(<argTys>)
        TypeInfo rti = node->scope->lookupType(fnInfo.returnType);
        std::string retTy = llvmTypeName(rti.name);

        outGlobalStream << "declare " << retTy
                        << " @" << node->value << "(";
        for (size_t i = 0; i < fnInfo.paramTypes.size(); ++i)
        {
            const auto &p = fnInfo.paramTypes[i];
            TypeInfo pti = node->scope->lookupType(p.type);
            outGlobalStream << llvmTypeName(pti.name);
            if (i + 1 < fnInfo.paramTypes.size())
                outGlobalStream << ", ";
        }
        outGlobalStream << ")\n";
    }
    void CodeGenLLVM::generateReturnstatement(std::unique_ptr<ASTNode> node, std::ostringstream &out)
    {
        // If there's no child, it's a void return
        if (node->children.empty())
        {
            out << "  ret void\n";
            return;
        }

        // 1) Evaluate the expression
        std::string result = emitExpression(std::move(node->children[0]), out);

        // 2) Cast to the function’s return type if needed
        auto funcScope = node->scope->findEnclosingFunctionScope();
        TypeInfo expected = funcScope->lookupType(funcScope->returnType);
        TypeInfo actual = regType.at(result);
        std::string casted = castValue(result, actual, expected, out);

        // 3) Emit the LLVM `ret` instruction
        std::string llvmTy = llvmTypeName(expected.name);
        out << "  ret " << llvmTy << " " << casted << "\n";
    }
} // namespace zust