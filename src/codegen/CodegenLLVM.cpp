#include "all.hpp"

namespace zlang
{
    // --------- LLVM IR ----------

    void CodeGenLLVM::emitExpr(const ASTNode *node, std::ostream &out, int &tempCounter)
    {
        using NT = NodeType;

        auto makeTmp = [&]()
        {
            return std::string("%") + std::to_string(tempCounter++);
        };
        if (node->type == NT::BooleanLiteral)
        {
            std::string bit = makeTmp();
            out << "  " << bit
                << " = add i1 0, " << (node->value == "true" ? "1" : "0") << "\n";
            std::string ext = makeTmp();
            out << "  " << ext
                << " = zext i1 " << bit << " to i64\n";
            return;
        }
        if (node->type == NT::IntegerLiteral)
        {
            std::string tmp = makeTmp();
            out << "  " << tmp << " = add i64 0, " << node->value << "\n";
            return;
        }
        if (node->type == NT::FloatLiteral)
        {
            std::string val = node->value;
            if (!val.empty() && (val.back() == 'f' || val.back() == 'F'))
                val.pop_back();
            std::string tmp = makeTmp();
            out << "  " << tmp << " = fadd double 0.0, " << val << "\n";
            return;
        }
        if (node->type == NT::StringLiteral)
        {
            auto it = strings_.find(node->value);
            std::string label;
            if (it == strings_.end())
            {
                label = "@.str" + std::to_string(stringCounter++);
                strings_.emplace(node->value, label);
            }
            else
            {
                label = it->second;
            }
            // 2) emit ptrtoint using that label
            std::string tmp = makeTmp();
            out << "  " << tmp
                << " = ptrtoint [" << (node->value.size() + 1)
                << " x i8]* " << label << " to i64\n";
            return;
        }
        if (node->type == NT::VariableAccess)
        {
            std::string tmp = makeTmp();
            out << "  " << tmp << " = load i64, i64* @" << node->value << "\n";
            return;
        }
        if (node->type == NT::BinaryOp)
        {
            // Emit left and right
            emitExpr(node->children[0].get(), out, tempCounter);
            int leftIdx = tempCounter - 1;
            emitExpr(node->children[1].get(), out, tempCounter);
            int rightIdx = tempCounter - 1;

            const std::string &op = node->value;
            std::string tmp = makeTmp();

            // Arithmetic
            if (op == "+")
                out << "  " << tmp << " = add i64 %" << leftIdx << ", %" << rightIdx << "\n";
            else if (op == "-")
                out << "  " << tmp << " = sub i64 %" << leftIdx << ", %" << rightIdx << "\n";
            else if (op == "*")
                out << "  " << tmp << " = mul i64 %" << leftIdx << ", %" << rightIdx << "\n";
            else if (op == "/")
                out << "  " << tmp << " = sdiv i64 %" << leftIdx << ", %" << rightIdx << "\n";

            // Bitwise
            else if (op == "&")
                out << "  " << tmp << " = and i64 %" << leftIdx << ", %" << rightIdx << "\n";
            else if (op == "|")
                out << "  " << tmp << " = or  i64 %" << leftIdx << ", %" << rightIdx << "\n";

            // Logical
            else if (op == "&&")
            {
                // compare non-zero -> i1, then and
                std::string c1 = makeTmp();
                out << "  " << c1 << " = icmp ne i64 %" << leftIdx << ", 0\n";
                std::string c2 = makeTmp();
                out << "  " << c2 << " = icmp ne i64 %" << rightIdx << ", 0\n";
                out << "  " << tmp << " = and i1 " << c1 << ", " << c2 << "\n";
                // zext to i64
                std::string z = makeTmp();
                out << "  " << z << " = zext i1 " << tmp << " to i64\n";
                return;
            }
            else if (op == "||")
            {
                std::string c1 = makeTmp();
                out << "  " << c1 << " = icmp ne i64 %" << leftIdx << ", 0\n";
                std::string c2 = makeTmp();
                out << "  " << c2 << " = icmp ne i64 %" << rightIdx << ", 0\n";
                out << "  " << tmp << " = or  i1 " << c1 << ", " << c2 << "\n";
                std::string z = makeTmp();
                out << "  " << z << " = zext i1 " << tmp << " to i64\n";
                return;
            }

            // Comparisons
            else if (op == "==")
                out << "  " << tmp << " = icmp eq  i64 %" << leftIdx << ", %" << rightIdx << "\n";
            else if (op == "!=")
                out << "  " << tmp << " = icmp ne  i64 %" << leftIdx << ", %" << rightIdx << "\n";
            else if (op == "<")
                out << "  " << tmp << " = icmp slt i64 %" << leftIdx << ", %" << rightIdx << "\n";
            else if (op == "<=")
                out << "  " << tmp << " = icmp sle i64 %" << leftIdx << ", %" << rightIdx << "\n";
            else if (op == ">")
                out << "  " << tmp << " = icmp sgt i64 %" << leftIdx << ", %" << rightIdx << "\n";
            else if (op == ">=")
                out << "  " << tmp << " = icmp sge i64 %" << leftIdx << ", %" << rightIdx << "\n";

            return;
        }
        if (node->type == NT::UnaryOp)
        {
            const std::string &op = node->value;

            // Logical NOT
            if (op == "!")
            {
                emitExpr(node->children[0].get(), out, tempCounter);
                int v = tempCounter - 1;
                std::string c = makeTmp();
                out << "  " << c << " = icmp eq i64 %" << v << ", 0\n";
                std::string z = makeTmp();
                out << "  " << z << " = zext i1 " << c << " to i64\n";
                return;
            }

            // ++ / --
            if (op == "++" || op == "--")
            {
                emitExpr(node->children[0].get(), out, tempCounter);
                int v = tempCounter - 1;
                std::string a = makeTmp();
                out << "  " << a << " = " << (op == "++" ? "add" : "sub")
                    << " i64 %" << v << ", 1\n";
                out << "  store i64 " << a << ", i64* @" << node->children[0]->value << "\n";
                return;
            }
        }

        throw std::runtime_error("Unsupported expr in LLVM codegen");
    }

    void CodeGenLLVM::generateStatement(const ASTNode *s, std::ostream &out, int &tempCounter)
    {
        if (s->type == NodeType::VariableDeclaration && s->children.size() == 2)
        {
            emitExpr(s->children[1].get(), out, tempCounter);
            out << "  store i64 %" << (tempCounter - 1) << ", i64* @" << s->value << "\n";
        }
        else if (s->type == NodeType::VariableReassignment && !s->children.empty())
        {
            emitExpr(s->children[0].get(), out, tempCounter);
            out << "  store i64 %" << (tempCounter - 1) << ", i64* @" << s->value << "\n";
        }
        if (s->type == NodeType::IfStatement)
        {
            int id = labelCounter++;
            std::string thenLbl = "then" + std::to_string(id);
            std::string elseLbl = "else" + std::to_string(id);
            std::string endLbl = "end_if" + std::to_string(id);

            emitExpr(s->children[0].get(), out, tempCounter);
            int cReg = tempCounter - 1;
            out << "  br i1 icmp ne i64 %" << cReg << ", 0, label %" << thenLbl
                << ", label %" << elseLbl << "\n";

            out << thenLbl << ":\n";
            for (auto &st : s->children[1]->children)
                generateStatement(st.get(), out, tempCounter);
            out << "  br label %" << endLbl << "\n";

            out << elseLbl << ":\n";
            ASTNode *branch = s->getElseBranch();
            while (branch)
            {
                if (branch->type == NodeType::ElseIfStatement)
                {
                    emitExpr(branch->children[0].get(), out, tempCounter);
                    int r2 = tempCounter - 1;
                    std::string nextThen = "then" + std::to_string(id);
                    std::string nextElse = elseLbl; // same label
                    out << "  br i1 icmp ne i64 %" << r2 << ", 0, label %"
                        << nextThen << ", label %" << nextElse << "\n";
                    // body
                    out << nextThen << ":\n";
                    for (auto &st : branch->children[1]->children)
                        generateStatement(st.get(), out, tempCounter);
                    out << "  br label %" << endLbl << "\n";
                    branch = branch->getElseBranch();
                }
                else // final else
                {
                    out << "  br label %" << endLbl << "\n";
                    out << elseLbl << ":\n";
                    for (auto &st : branch->children[0]->children)
                        generateStatement(st.get(), out, tempCounter);
                    out << "  br label %" << endLbl << "\n";
                    break;
                }
            }
            out << endLbl << ":\n\n";
            return;
        }
        // end label
    }

    void CodeGenLLVM::generate(const ASTNode *program, std::ostream &out, bool isFirst)
    {
        if (isFirst)
        {
            std::vector<std::string> globals;
            for (auto &s : program->children)
                if (s->type == NodeType::VariableDeclaration)
                    globals.push_back(s->value);

            // Declare string constants
            out << "; String constants\n";
            for (auto &kv : strings_)
            {
                const auto &lit = kv.first;
                const auto &label = kv.second;
                out << label << " = private unnamed_addr constant ["
                    << (lit.size() + 1) << " x i8] c\""
                    << escapeString(lit) << "\\00\"\n";
            }

            // Declare global variables
            out << "; Global variables\n";
            for (auto &g : globals)
                out << "@" << g << " = global i64 0\n";

            out << "define i32 @main() {\n";
        }
        int tempCounter = 0;

        for (auto &s : program->children)
        {
            generateStatement(s.get(), out, tempCounter);
        }
        if (isFirst)
            out << "  ret i32 0\n}\n";
    }

    std::string escapeString(const std::string &input)
    {
        std::string output;
        for (char c : input)
        {
            switch (c)
            {
            case '\n':
                output += "\\0A";
                break;
            case '\t':
                output += "\\09";
                break;
            case '\"':
                output += "\\22";
                break;
            case '\\':
                output += "\\5C";
                break;
            default:
                output += c;
            }
        }
        return output;
    }
}