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
        return name;
    }
    std::string CodeGenLLVM::generateFloatLiteral(std::unique_ptr<ASTNode> node)
    {
        std::string name = fresh();
        double val = std::stod(node->value);
        out << "  " << name << " = fadd double 0.0, " << val << "\n";
        return name;
    }
    std::string CodeGenLLVM::generateStringLiteral(std::unique_ptr<ASTNode> node)
    {
        std::string lbl = "@.str" + std::to_string(stringLabelCount++);
        out << lbl << " = private unnamed_addr constant [" << node->value.size() + 1 << " x i8] c\""
            << node->value << "\00\", align 1\n";
        std::string ptr = fresh();
        out << "  " << ptr << " = getelementptr inbounds [" << node->value.size() + 1 << " x i8], ["
            << node->value.size() + 1 << " x i8]* " << lbl << ", i64 0, i64 0\n";
        return ptr;
    }
    std::string
    CodeGenLLVM::generateBooleanLiteral(std::unique_ptr<ASTNode> node)
    {
        std::string name = fresh();
        out << "  " << name << " = add i1 0, "
            << (node->value == "true" ? "1" : "0") << "\n";
        return name;
    }
    std::string
    CodeGenLLVM::generateVariableAccess(std::unique_ptr<ASTNode> node)
    {
        auto &scope = *node->scope;
        auto name = node->value;
        // Assume all locals are allocas at entry; global variables are @globals
        std::string ptr = "%" + name;
        std::string loaded = fresh();
        auto ti = scope.lookupType(scope.lookupVariable(name).type);
        std::string ty = ti.isFloat ? (ti.bits == 32 ? "float" : "double") : (ti.bits == 64 ? "i64" : "i32");
        out << "  " << loaded << " = load " << ty << ", " << ty << "* " << ptr << "\n";
        return loaded;
    }
    std::string
    CodeGenLLVM::generateBinaryOperation(std::unique_ptr<ASTNode> node)
    {
        auto lhs = emitExpression(std::move(node->children[0]));
        auto rhs = emitExpression(std::move(node->children[1]));
        TypeInfo t1 = regType[lhs], t2 = regType[rhs];
        TypeInfo tr = TypeChecker::promoteType(t1, t2);
        std::string res = fresh();

        if (tr.isFloat)
        {
            // floating-point arithmetic and comparison
            if (node->value == "+" || node->value == "-" || node->value == "*" || node->value == "/")
            {
                std::string op = (node->value == "+" ? "fadd" : node->value == "-" ? "fsub"
                                                            : node->value == "*"   ? "fmul"
                                                                                   : "fdiv");
                out << "  " << res << " = " << op << " double " << lhs << ", " << rhs << "\n";
            }
            else
            {
                // comparisons: ==, !=, <, <=, >, >=
                static const std::unordered_map<std::string, std::string> fcmp_ops = {
                    {"==", "oeq"}, {"!=", "one"}, {"<", "olt"}, {"<=", "ole"}, {">", "ogt"}, {">=", "oge"}};
                auto it = fcmp_ops.find(node->value);
                if (it == fcmp_ops.end())
                    throw std::runtime_error("Unsupported FP cmp: " + node->value);
                out << "  " << res << " = fcmp " << it->second << " double " << lhs << ", " << rhs << "\n";
            }
        }
        else
        {
            // integer arithmetic and comparison
            if (node->value == "+" || node->value == "-" || node->value == "*" || node->value == "/")
            {
                std::string op = (node->value == "+" ? "add" : node->value == "-" ? "sub"
                                                           : node->value == "*"   ? "mul"
                                                                                  : "sdiv");
                out << "  " << res << " = " << op << " i64 " << lhs << ", " << rhs << "\n";
            }
            else
            {
                // comparisons: ==, !=, <, <=, >, >=
                static const std::unordered_map<std::string, std::string> icmp_ops = {
                    {"==", "eq"}, {"!=", "ne"}, {"<", "slt"}, {"<=", "sle"}, {">", "sgt"}, {">=", "sge"}};
                auto it = icmp_ops.find(node->value);
                if (it == icmp_ops.end())
                    throw std::runtime_error("Unsupported Int cmp: " + node->value);
                out << "  " << res << " = icmp " << it->second << " i64 " << lhs << ", " << rhs << "\n";
            }
        }
        return res;
    }
    std::string
    CodeGenLLVM::generateUnaryOperation(std::unique_ptr<ASTNode> node)
    {
        auto &scope = *node->children[0]->scope;
        auto varName = node->children[0]->value;
        TypeInfo ti = scope.lookupType(scope.lookupVariable(varName).type);
        std::string ty = ti.isFloat ? (ti.bits == 32 ? "float" : "double") : "i64";

        if (node->value == "!")
        {
            auto val = emitExpression(std::move(node->children[0]));
            std::string res = fresh();
            out << "  " << res << " = icmp eq " << ty << " " << val << ", 0\n";
            return res;
        }
        else if (node->value == "-")
        {
            auto val = emitExpression(std::move(node->children[0]));
            std::string res = fresh();
            out << "  " << res << " = sub " << ty << " 0, " << val << "\n";
            return res;
        }
        else if (node->value == "++" || node->value == "--")
        {
            // only integer supported
            if (ti.isFloat)
                throw std::runtime_error("Increment/Decrement not supported on float");
            // generate load ptr and current value
            std::string ptr = "%" + varName;
            std::string cur = fresh();
            out << "  " << cur << " = load " << ty << ", " << ty << "* " << ptr << "\n";
            // increment or decrement
            std::string one = "";
            std::string updated = fresh();
            out << "  " << updated << " = "
                << (node->value == "++" ? "add" : "sub")
                << " " << ty << " " << cur << ", 1\n";
            // store back
            out
                << "  store " << ty << " " << updated << ", " << ty << "* " << ptr << "\n";
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
            throw std::runtime_error("Unknown statement encountered.");
        }
    }

    void CodeGenLLVM::emitEpilogue()
    {
    }
    void CodeGenLLVM::emitPrologue(std::unique_ptr<ASTNode> blockNode)
    {
        out << fresh() << ":\n";
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
            throw std::runtime_error("Unknown statement encountered.");
        }
    }

    void CodeGenLLVM::generateVariableReassignment(
        std::unique_ptr<ASTNode> node)
    {
        auto &scope = *node->scope;
        auto name = node->value;
        TypeInfo ti = scope.lookupType(scope.lookupVariable(name).type);
        std::string ty = ti.isFloat ? (ti.bits == 32 ? "float" : "double") : "i64";
        auto val = emitExpression(std::move(node->children.back()));
        out << "  store " << ty << " " << val << ", " << ty << "* %" << name << "\n";
    }
    void CodeGenLLVM::generateVariableDeclaration(
        std::unique_ptr<ASTNode> node)
    {
        auto &scope = *node->scope;
        auto name = node->value;
        TypeInfo ti = scope.lookupType(scope.lookupVariable(name).type);
        std::string ty = ti.isFloat ? (ti.bits == 32 ? "float" : "double") : "i64";
        // At function entry, emit: %name = alloca ty
        out << "  %" << name << " = alloca " << ty << "\n";
        if (node->children.size() >= 1)
        {
            auto val = emitExpression(std::move(node->children.back()));
            out << "  store " << ty << " " << val << ", " << ty << "* %" << name << "\n";
        }
    }
    void CodeGenLLVM::generateIfStatement(std::unique_ptr<ASTNode> statement)
    {
        int id = blockLabelCount++;
        std::string elseLbl = ".Lelse" + std::to_string(id);
        std::string endLbl = ".Lend" + std::to_string(id);
        auto condR = emitExpression(std::move(statement->children[0]));
        out << "    cmpq $0, %" << condR << "\n";
        out << "    je " << elseLbl << "\n";
        alloc.free(condR);
        std::unique_ptr<ASTNode> ifBlock = std::move(statement->children[1]);
        emitPrologue(std::move(ifBlock));
        for (auto &statement : ifBlock->children)
            generateStatement(std::move(statement));
        emitEpilogue();
        out << "    jmp   " << endLbl << "\n";
        out << elseLbl << ":\n";
        ASTNode *branch = statement->getElseBranch();
        while (branch)
        {
            if (branch->type == NodeType::ElseIfStatement)
            {
                auto r2 = emitExpression(std::move(branch->children[0]));
                out << "    cmpq $0, %" << r2 << "\n";
                out << "    je   " << elseLbl << "\n";
                alloc.free(r2);

                std::unique_ptr<ASTNode> elifBlock = std::move(branch->children[1]);
                emitPrologue(std::move(elifBlock));
                for (auto &statement : elifBlock->children)
                    generateStatement(std::move(statement));
                emitEpilogue();
                out << "    jmp   " << endLbl << "\n";

                branch = branch->getElseBranch();
            }
            else if (branch->type == NodeType::ElseStatement)
            {
                std::unique_ptr<ASTNode> elseBlock = std::move(branch->children[0]);
                emitPrologue(std::move(elseBlock));
                for (auto &statement : elseBlock->children)
                    generateStatement(std::move(statement));
                emitEpilogue();
                break;
            }
        }
        out << endLbl << ":\n\n";
        return;
    }

    void CodeGenLLVM::generate(std::unique_ptr<ASTNode> program)
    {
        out << "; ModuleID = 'zlang'\n";
        out << "source_filename = \"zlang\"\n";
        // Emit global variable definitions
        for (auto &statement : program->children)
        {
            if (statement->type == NodeType::VariableDeclaration)
            {
                auto &name = statement->value;
                auto info = statement->scope->lookupType(
                    statement->scope->lookupVariable(name).type);
                std::string ty = info.isFloat
                                     ? (info.bits == 32 ? "float" : "double")
                                     : (info.bits == 64 ? "i64" : "i32");
                out << "@" << name << " = global " << ty << " 0\n";
            }
        }
        out << "\n";

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
    }
} // namespace zlang