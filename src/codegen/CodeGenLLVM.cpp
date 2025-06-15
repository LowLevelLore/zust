#include "all.hpp"

namespace zlang
{
    static std::string fresh()
    {
        static int cnt = 0;
        return "%tmp" + std::to_string(cnt++);
    }

    std::string CodeGenLLVM::intToXmm(const std::string &register_int,
                                      uint32_t bits)
    {
        return "";
    }
    std::string CodeGenLLVM::generateIntegerLiteral(std::unique_ptr<ASTNode> node)
    {
        std::string name = fresh();
        out << "  " << name << " = add i64 0, " << node->value << "\n";
        noteType(name, node->scope->lookupType("int64_t"));
        return name;
    }
    std::string CodeGenLLVM::generateFloatLiteral(std::unique_ptr<ASTNode> node)
    {
        std::string tmpDouble = fresh();
        std::string val = node->value;
        bool isF32 = (!val.empty() && (val.back() == 'f' || val.back() == 'F'));
        if (isF32)
            val.pop_back();
        out << "  " << tmpDouble << " = fadd double 0.0, " << val << "\n";
        if (isF32)
        {
            std::string tmpFloat = fresh();
            out << "  " << tmpFloat << " = fptrunc double " << tmpDouble << " to float\n";
            noteType(tmpFloat, node->scope->lookupType("float"));
            return tmpFloat;
        }
        else
        {
            noteType(tmpDouble, node->scope->lookupType("double"));
            return tmpDouble;
        }
    }
    std::string CodeGenLLVM::generateStringLiteral(std::unique_ptr<ASTNode> node)
    {
        std::string value = node->value;

        auto it = stringLiterals.find(value);
        if (it != stringLiterals.end())
            return it->second;

        std::string name = "@.str" + std::to_string(stringLabelCount++);
        size_t len = value.size() + 1; // Include null terminator
        std::string llvmEscaped;

        for (unsigned char c : value)
        {
            if (isprint(c) && c != '"' && c != '\\')
            {
                llvmEscaped += c;
            }
            else
            {
                char buf[5];
                snprintf(buf, sizeof(buf), "\\%02X", c); // Use uppercase hex
                llvmEscaped += buf;
            }
        }
        llvmEscaped += "\\00";
        outGlobal << name << " = private unnamed_addr constant ["
                  << len << " x i8] c\"" << llvmEscaped << "\"\n";

        std::string ptr = fresh();
        out << "  " << ptr << " = getelementptr inbounds ["
            << len << " x i8], [" << len << " x i8]* "
            << name << ", i64 0, i64 0\n";

        // Cast pointer to i64 if storing in i64* variable
        std::string casted = fresh();
        out << "  " << casted << " = ptrtoint i8* " << ptr << " to i64\n";

        noteType(casted, node->scope->lookupType("int64_t")); // casted is now i64
        stringLiterals[value] = casted;
        return casted;
    }
    std::string CodeGenLLVM::generateBooleanLiteral(std::unique_ptr<ASTNode> node)
    {
        std::string name = fresh();
        out << "  " << name << " = add i1 0, "
            << (node->value == "true" ? "1" : "0") << "\n";
        noteType(name, node->scope->lookupType("boolean"));
        return name;
    }
    std::string CodeGenLLVM::generateVariableAccess(std::unique_ptr<ASTNode> node)
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
        std::string ptr = (isGlobal ? "@" : "%") + name;

        // Generate load instruction
        std::string loaded = fresh();
        out << "  " << loaded << " = load " << ty << ", " << ty << "* " << ptr << "\n";

        noteType(loaded, ti);
        return loaded;
    }
    std::string CodeGenLLVM::generateBinaryOperation(std::unique_ptr<ASTNode> node)
    {
        auto lhs = emitExpression(std::move(node->children[0]));
        auto rhs = emitExpression(std::move(node->children[1]));
        TypeInfo t1 = regType[lhs];
        TypeInfo t2 = regType[rhs];
        TypeInfo tr = TypeChecker::promoteType(t1, t2);
        std::string res = fresh();

        if (tr.isFloat)
        {
            auto cast = [&](const std::string &val, const TypeInfo &ti)
            {
                if (ti.isFloat && ti.bits == 64)
                    return val; // already double
                std::string target = (tr.bits == 64 ? "double" : "float");
                std::string tmp = fresh();
                if (!ti.isFloat)
                {
                    // integer -> double
                    std::string intTy = "i" + std::to_string(ti.bits);
                    out << "  " << tmp << " = sitofp " << intTy << " " << val << " to " << target << "\n";
                }
                else
                {
                    // float32 -> double
                    out << "  " << tmp << " = fpext float " << val << " to " << target << "\n";
                }
                noteType(tmp, tr);
                return tmp;
            };
            std::string L = cast(lhs, t1);
            std::string R = cast(rhs, t2);
            std::string target = (tr.bits == 64 ? "double" : "float");
            // Floating-point operations
            if (node->value == "+" || node->value == "-" || node->value == "*" || node->value == "/")
            {
                static const std::unordered_map<std::string, std::string> fp_ops = {{"+", "fadd"}, {"-", "fsub"}, {"*", "fmul"}, {"/", "fdiv"}};
                auto op = fp_ops.at(node->value);
                out << "  " << res << " = " << op << " " << target << " " << L << ", " << R << "\n";
            }
            else
            {
                static const std::unordered_map<std::string, std::string> fcmp_ops = {{"==", "oeq"}, {"!=", "one"}, {"<", "olt"}, {"<=", "ole"}, {">", "ogt"}, {">=", "oge"}};
                auto cmpop = fcmp_ops.at(node->value);
                out << "  " << res << " = fcmp " << cmpop << " " << target << " " << L << ", " << R << "\n";
                std::string zero = fresh();
                out << "  " << zero << " = zext i1 " << res << " to i8\n";
                noteType(zero, node->scope->lookupType("boolean"));
                return zero;
            }
            noteType(res, tr);
        }
        else
        {
            // Integer operations on various widths
            std::string intTy = "i" + std::to_string(tr.bits);

            if (node->value == "+" || node->value == "-" || node->value == "*" || node->value == "/")
            {
                static const std::unordered_map<std::string, std::string> int_ops = {{"+", "add"}, {"-", "sub"}, {"*", "mul"}, {"/", "sdiv"}};
                auto op = int_ops.at(node->value);
                out << "  " << res << " = " << op << " " << intTy << " " << lhs << ", " << rhs << "\n";
                noteType(res, tr);
            }
            else
            {
                static const std::unordered_map<std::string, std::string> icmp_ops = {{"==", "eq"}, {"!=", "ne"}, {"<", "slt"}, {"<=", "sle"}, {">", "sgt"}, {">=", "sge"}};
                auto cmpop = icmp_ops.at(node->value);
                out << "  " << res << " = icmp " << cmpop << " " << intTy << " " << lhs << ", " << rhs << "\n";
                std::string zero = fresh();
                out << "  " << zero << " = zext i1 " << res << " to i8\n";
                noteType(zero, node->scope->lookupType("boolean"));
                return zero;
            }
        }

        return res;
    }
    std::string CodeGenLLVM::generateUnaryOperation(std::unique_ptr<ASTNode> node)
    {
        auto &scope = *node->children[0]->scope;
        auto varName = node->children[0]->value;
        TypeInfo ti = scope.lookupType(scope.lookupVariable(varName).type);

        // Use correct LLVM type string
        std::string llvmType;
        if (ti.isFloat)
            llvmType = (ti.bits == 32) ? "float" : "double";
        else
            llvmType = "i" + std::to_string(ti.bits);

        if (node->value == "!")
        {
            auto val = emitExpression(std::move(node->children[0]));
            std::string res = fresh();
            out << "  " << res << " = icmp eq " << llvmType << " " << val << ", 0\n";
            std::string zero = fresh();
            out << "  " << zero << " = zext i1 " << res << " to i8\n";
            noteType(zero, node->scope->lookupType("boolean"));
            return zero;
        }
        else if (node->value == "-")
        {
            auto val = emitExpression(std::move(node->children[0]));
            std::string res = fresh();
            out << "  " << res << " = sub " << llvmType << " 0, " << val << "\n";
            return res;
        }
        else if (node->value == "++" || node->value == "--")
        {
            if (ti.isFloat)
                throw std::runtime_error("Increment/Decrement not supported on float");

            bool isGlobal = scope.isGlobalVariable(varName);
            std::string ptr = (isGlobal ? "@" : "%") + varName;

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
    std::string CodeGenLLVM::emitExpression(std::unique_ptr<ASTNode> node)
    {
        switch (node->type)
        {
        case NodeType::IntegerLiteral:
            return generateIntegerLiteral(std::move(node));
        case NodeType::FloatLiteral:
            return generateFloatLiteral(std::move(node));
        case NodeType::StringLiteral:
            return generateStringLiteral(std::move(node));
        case NodeType::BooleanLiteral:
            return generateBooleanLiteral(std::move(node));
        case NodeType::VariableAccess:
            return generateVariableAccess(std::move(node));
        case NodeType::BinaryOp:
            return generateBinaryOperation(std::move(node));
        case NodeType::UnaryOp:
            return generateUnaryOperation(std::move(node));
        default:
            node->print(std::cout, 0);
            throw std::runtime_error("Unknown expression encountered.");
        }
    }
    void CodeGenLLVM::emitEpilogue()
    {
        out << "    ; Block ends\n";
    }
    void CodeGenLLVM::emitPrologue(std::unique_ptr<ASTNode> blockNode)
    {
    }
    void CodeGenLLVM::generateStatement(std::unique_ptr<ASTNode> statement)
    {
        switch (statement->type)
        {
        case NodeType::VariableReassignment:
        {
            generateVariableReassignment(std::move(statement));
            break;
        }
        case NodeType::VariableDeclaration:
        {
            generateVariableDeclaration(std::move(statement));
            break;
        }
        case NodeType::IfStatement:
        {
            generateIfStatement(std::move(statement));
            break;
        }
        default:
            statement->print(std::cout, 0);
            throw std::runtime_error("Unknown statement encountered.");
        }
    }
    void CodeGenLLVM::generateVariableReassignment(std::unique_ptr<ASTNode> node)
    {
        auto &scope = *node->scope;
        auto name = node->value;
        TypeInfo ti = scope.lookupType(scope.lookupVariable(name).type);
        bool isGlobal = scope.isGlobalVariable(name);

        // 1) Compute the RHS expression
        std::string val = emitExpression(std::move(node->children.back()));
        TypeInfo tr = regType[val]; // actual computed type

        // 2) Convert value to target type if needed
        if (!ti.isFloat && !tr.isFloat && ti.bits != tr.bits)
        {
            if (tr.bits > ti.bits)
            {
                // Truncate larger integer to smaller
                std::string narrow = fresh();
                out << "  " << narrow << " = trunc i" << tr.bits
                    << " " << val << " to i" << ti.bits << "\n";
                val = narrow;
            }
            else
            {
                // Extend smaller integer to larger
                std::string extend = fresh();
                std::string op = tr.isSigned ? "sext" : "zext";
                out << "  " << extend << " = " << op << " i" << tr.bits
                    << " " << val << " to i" << ti.bits << "\n";
                val = extend;
            }
        }

        // 3) Emit the store to the correct location
        std::string ty = ti.isFloat
                             ? (ti.bits == 32 ? "float" : "double")
                             : "i" + std::to_string(ti.bits);

        if (isGlobal)
        {
            out << "  store " << ty << " " << val << ", " << ty << "* @" << name << "\n";
        }
        else
        {
            out << "  store " << ty << " " << val << ", " << ty << "* %" << name << "\n";
        }
    }
    void CodeGenLLVM::generateVariableDeclaration(
        std::unique_ptr<ASTNode> node)
    {
        bool isGlobal = node->scope->isGlobalVariable(node->value);
        TypeInfo ti = node->scope->lookupType(node->scope->lookupVariable(node->value).type);
        std::string ty = ti.isFloat ? (ti.bits == 32 ? "float" : "double") : "i" + std::to_string(ti.bits);

        if (node->children.size() >= 2)
        {
            auto val = emitExpression(std::move(node->children.back()));
            TypeInfo tr = regType[val];

            if (!ti.isFloat && !tr.isFloat && ti.bits != tr.bits)
            {
                if (tr.bits > ti.bits)
                {
                    std::string narrow = fresh();
                    out << "  " << narrow << " = trunc i" << tr.bits << " " << val << " to i" << ti.bits << "\n";
                    val = narrow;
                }
                else
                {
                    std::string extend = fresh();
                    std::string op = tr.isSigned ? "sext" : "zext";
                    out << "  " << extend << " = " << op << " i" << tr.bits << " " << val << " to i" << ti.bits << "\n";
                    val = extend;
                }
            }

            if (isGlobal)
            {
                out << "  store " << ty << " " << val << ", " << ty << "* @" << node->value << "\n";
            }
            else
            {
                out << "  store " << ty << " " << val << ", " << ty << "* %" << node->value << "\n";
            }
        }
    }
    void CodeGenLLVM::generateIfStatement(std::unique_ptr<ASTNode> statement)
    {
        int id = blockLabelCount++;
        std::string thenLbl = "if.then" + std::to_string(id);
        std::string elseLbl = "if.else" + std::to_string(id);
        std::string endLbl = "if.end" + std::to_string(id);

        // Emit the condition
        auto condVal = emitExpression(std::move(statement->children[0])); // e.g., %1
        TypeInfo condTi = regType[condVal];
        std::string condBool = fresh();
        out << "    " << condBool
            << " = trunc i" << condTi.bits
            << " " << condVal << " to i1";
        std::string temp = fresh();
        out << "    " << temp << " = icmp ne i1 " << condBool << ", 0\n";
        out << "    br i1 " << temp << ", label %" << thenLbl << ", label %" << elseLbl << "\n";

        // Then block
        out << thenLbl << ":\n";
        auto ifBlock = std::move(statement->children[1]);
        auto children = std::move(ifBlock->children);
        emitPrologue(std::move(ifBlock));
        for (auto &stmt : children)
            generateStatement(std::move(stmt));
        emitEpilogue();
        out << "    br label %" << endLbl << "\n";

        // Else or ElseIf
        out << elseLbl << ":\n";
        ASTNode *branch = statement->getElseBranch();
        while (branch)
        {
            if (branch->type == NodeType::ElseIfStatement)
            {
                auto elifCond = emitExpression(std::move(branch->children[0]));
                TypeInfo condTi = regType[elifCond];
                std::string condBool = fresh();
                out << "    " << condBool
                    << " = trunc i" << condTi.bits
                    << " " << condVal << " to i1";
                std::string temp = fresh();
                out << "    " << temp << " = icmp ne i1 " << condBool << ", 0\n";

                std::string elifThen = "elif.then" + std::to_string(blockLabelCount);
                std::string elifNext = "elif.next" + std::to_string(blockLabelCount);
                blockLabelCount++;

                out << "    br i1 " << temp << ", label %" << elifThen << ", label %" << elifNext << "\n";

                // Elif then block
                out << elifThen << ":\n";
                auto elifBlock = std::move(branch->children[1]);
                auto elifChildren = std::move(elifBlock->children);
                emitPrologue(std::move(elifBlock));
                for (auto &stmt : elifChildren)
                    generateStatement(std::move(stmt));
                emitEpilogue();
                out << "    br label %" << endLbl << "\n";

                // Prepare for next else
                out << elifNext << ":\n";
                branch = branch->getElseBranch();
            }
            else if (branch->type == NodeType::ElseStatement)
            {
                auto elseBlock = std::move(branch->children[0]);
                auto elseChildren = std::move(elseBlock->children);
                emitPrologue(std::move(elseBlock));
                for (auto &stmt : elseChildren)
                    generateStatement(std::move(stmt));
                emitEpilogue();
                out << "    br label %" << endLbl << "\n";
                break;
            }
            else
            {
                break;
            }
        }
        out << "    br label %" << endLbl << "\n";
        // End block
        out << endLbl << ":\n";
    }
    void CodeGenLLVM::generate(std::unique_ptr<ASTNode> program)
    {
        outGlobal << "; ModuleID = 'zlang'\n";
        outGlobal << "source_filename = \"zlang\"\n";
        // Emit global variable definitions
        for (auto &statement : program->children)
        {
            if (statement->type == NodeType::VariableDeclaration)
            {
                auto &name = statement->value;
                auto ti = statement->scope->lookupType(
                    statement->scope->lookupVariable(name).type);
                std::string ty = ti.isFloat
                                     ? (ti.bits == 32 ? "float" : "double")
                                     : "i" + std::to_string(ti.bits);
                outGlobal << "@" << name << " = global " << ty << (ti.isFloat ? " 0.0" : " 0") << "\n";
            }
        }
        outGlobal << "\n";

        // Define main function
        out
            << "define i32 @main() {\n";
        emitPrologue(nullptr);
        // Generate each top-level statement
        for (auto &stmt : program->children)
        {
            generateStatement(std::move(stmt));
        }
        out << "  ret i32 0\n";
        out
            << "}\n";

        outfinal << outGlobal.str() + "\n\n"
                 << out.str() << "\n\n";
    }
} // namespace zlang