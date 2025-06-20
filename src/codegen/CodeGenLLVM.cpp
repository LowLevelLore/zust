// #include "all.hpp"

// namespace zlang {
//     static std::string fresh() {
//         static int cnt = 0;
//         return "%tmp" + std::to_string(cnt++);
//     }
//     std::string toHexFloatFromStr(const std::string &valStr, bool isF32) {
//         std::ostringstream oss;
//         oss << std::scientific << std::setprecision(16);
//         if (isF32) {
//             float v = std::stof(valStr);
//             oss << v;
//         } else {
//             double v = std::stod(valStr);
//             oss << v;
//         }
//         return oss.str();
//     }
//     std::string CodeGenLLVM::castValue(const std::string &val, const TypeInfo &fromType, const TypeInfo &toType) {
//         if (fromType.isFloat == toType.isFloat && fromType.bits == toType.bits)
//             return val;

//         std::string tmp = fresh();

//         if (!fromType.isFloat && toType.isFloat) {
//             std::string intTy = "i" + std::to_string(fromType.bits);
//             std::string floatTy = (toType.bits == 64 ? "double" : "float");
//             std::string instr = fromType.isSigned ? "sitofp" : "uitofp";
//             out << "  " << tmp << " = " << instr << " " << intTy << " " << val << " to " << floatTy << "\n";
//         } else if (fromType.isFloat && !toType.isFloat) {
//             std::string floatTy = (fromType.bits == 64 ? "double" : "float");
//             std::string intTy = "i" + std::to_string(toType.bits);
//             std::string instr = toType.isSigned ? "fptosi" : "fptoui";
//             out << "  " << tmp << " = " << instr << " " << floatTy << " " << val << " to " << intTy << "\n";
//         } else if (!fromType.isFloat && !toType.isFloat) {
//             std::string fromTy = "i" + std::to_string(fromType.bits);
//             std::string toTy = "i" + std::to_string(toType.bits);
//             if (fromType.bits < toType.bits) {
//                 std::string instr = fromType.isSigned ? "sext" : "zext";
//                 out << "  " << tmp << " = " << instr << " " << fromTy << " " << val << " to " << toTy << "\n";
//             } else if (fromType.bits > toType.bits) {
//                 out << "  " << tmp << " = trunc " << fromTy << " " << val << " to " << toTy << "\n";
//             } else if (fromType.isSigned != toType.isSigned) {
//                 // Retag value if signedness differs but bit-width is the same
//                 out << "  " << tmp << " = add " << fromTy << " " << val << ", 0\n";
//             } else {
//                 return val;
//             }
//         } else if (fromType.isFloat && toType.isFloat) {
//             std::string fromTy = (fromType.bits == 64 ? "double" : "float");
//             std::string toTy = (toType.bits == 64 ? "double" : "float");
//             if (fromType.bits < toType.bits) {
//                 out << "  " << tmp << " = fpext " << fromTy << " " << val << " to " << toTy << "\n";
//             } else if (fromType.bits > toType.bits) {
//                 out << "  " << tmp << " = fptrunc " << fromTy << " " << val << " to " << toTy << "\n";
//             } else {
//                 return val;
//             }
//         } else {
//             throw std::runtime_error("Unsupported cast from type to type");
//         }

//         noteType(tmp, toType);
//         return tmp;
//     }
//     std::string CodeGenLLVM::generateIntegerLiteral(std::unique_ptr<ASTNode> node) {
//         std::string name = fresh();
//         out << "  " << name << " = add i64 0, " << node->value << "\n";
//         noteType(name, node->scope->lookupType("int64_t"));
//         return name;
//     }
//     std::string CodeGenLLVM::generateFloatLiteral(std::unique_ptr<ASTNode> node) {
//         std::string tmp = fresh();
//         std::string val = node->value;
//         bool isF32 = (!val.empty() && (val.back() == 'f' || val.back() == 'F'));
//         if (isF32)
//             val.pop_back();

//         std::string hexVal = toHexFloatFromStr(val, isF32);

//         if (isF32) {
//             out << "  " << tmp << " = fadd float 0.0, " << hexVal << "\n";
//             noteType(tmp, node->scope->lookupType("float"));
//         } else {
//             out << "  " << tmp << " = fadd double 0.0, " << hexVal << "\n";
//             noteType(tmp, node->scope->lookupType("double"));
//         }
//         return tmp;
//     }
//     std::string CodeGenLLVM::generateStringLiteral(std::unique_ptr<ASTNode> node) {
//         std::string value = node->value;

//         auto it = stringLiterals.find(value);
//         if (it != stringLiterals.end())
//             return it->second;

//         std::string name = "@.str" + std::to_string(stringLabelCount++);
//         size_t len = value.size() + 1;  // Include null terminator
//         std::string llvmEscaped;

//         for (unsigned char c : value) {
//             if (isprint(c) && c != '"' && c != '\\') {
//                 llvmEscaped += c;
//             } else {
//                 char buf[5];
//                 snprintf(buf, sizeof(buf), "\\%02X", c);  // Use uppercase hex
//                 llvmEscaped += buf;
//             }
//         }
//         llvmEscaped += "\\00";
//         outGlobal << name << " = private unnamed_addr constant ["
//                   << len << " x i8] c\"" << llvmEscaped << "\"\n";

//         std::string ptr = fresh();
//         out << "  " << ptr << " = getelementptr inbounds ["
//             << len << " x i8], [" << len << " x i8]* "
//             << name << ", i64 0, i64 0\n";

//         // Cast pointer to i64 if storing in i64* variable
//         std::string casted = fresh();
//         out << "  " << casted << " = ptrtoint i8* " << ptr << " to i64\n";

//         noteType(casted, node->scope->lookupType("int64_t"));  // casted is now i64
//         stringLiterals[value] = casted;
//         return casted;
//     }
//     std::string CodeGenLLVM::generateBooleanLiteral(std::unique_ptr<ASTNode> node) {
//         std::string name = fresh();
//         out << "  " << name << " = add i8 0, "
//             << (node->value == "true" ? "1" : "0") << "\n";
//         noteType(name, node->scope->lookupType("boolean"));
//         return name;
//     }
//     std::string CodeGenLLVM::generateVariableAccess(std::unique_ptr<ASTNode> node) {
//         auto &scope = *node->scope;
//         auto name = node->value;

//         // Lookup type info
//         auto ti = scope.lookupType(scope.lookupVariable(name).type);
//         std::string ty = ti.isFloat
//                              ? (ti.bits == 32 ? "float" : "double")
//                              : "i" + std::to_string(ti.bits);

//         // Determine if it's a global or local variable
//         bool isGlobal = scope.isGlobalVariable(name);
//         std::string ptr;
//         if (!isGlobal) {
//             ptr = "%" + node->scope->getMapping(name);
//         } else {
//             ptr = "@" + name;
//         }

//         // Generate load instruction
//         std::string loaded = fresh();
//         out << "  " << loaded << " = load " << ty << ", " << ty << "* " << ptr << "\n";

//         noteType(loaded, ti);
//         return loaded;
//     }
//     std::string CodeGenLLVM::generateBinaryOperation(std::unique_ptr<ASTNode> node) {
//         auto lhs = emitExpression(std::move(node->children[0]));
//         auto rhs = emitExpression(std::move(node->children[1]));
//         TypeInfo t1 = regType[lhs];
//         TypeInfo t2 = regType[rhs];
//         TypeInfo tr = TypeChecker::promoteType(t1, t2);
//         std::string L = castValue(lhs, t1, tr);
//         std::string R = castValue(rhs, t2, tr);
//         std::string res = fresh();

//         if (tr.isFloat) {
//             std::string target = (tr.bits == 64 ? "double" : "float");
//             static const std::unordered_map<std::string, std::string> fp_ops = {{"+", "fadd"}, {"-", "fsub"}, {"*", "fmul"}, {"/", "fdiv"}};
//             static const std::unordered_map<std::string, std::string> fcmp_ops = {{"==", "oeq"}, {"!=", "one"}, {"<", "olt"}, {"<=", "ole"}, {">", "ogt"}, {">=", "oge"}};

//             if (fp_ops.count(node->value)) {
//                 out << "  " << res << " = " << fp_ops.at(node->value) << " " << target << " " << L << ", " << R << "\n";
//                 noteType(res, tr);
//                 return res;
//             } else {
//                 std::string cmpop = fcmp_ops.at(node->value);
//                 out << "  " << res << " = fcmp " << cmpop << " " << target << " " << L << ", " << R << "\n";
//                 std::string zext = fresh();
//                 out << "  " << zext << " = zext i1 " << res << " to i8\n";
//                 noteType(zext, node->scope->lookupType("boolean"));
//                 return zext;
//             }
//         } else {
//             std::string intTy = "i" + std::to_string(tr.bits);
//             bool isSigned = tr.isSigned;

//             if (node->value == "+" || node->value == "-" || node->value == "*" || node->value == "/") {
//                 std::string op;
//                 if (node->value == "+")
//                     op = "add";
//                 else if (node->value == "-")
//                     op = "sub";
//                 else if (node->value == "*")
//                     op = "mul";
//                 else /* "/" */
//                     op = (isSigned ? "sdiv" : "udiv");

//                 out << "  " << res << " = " << op << " " << intTy << " " << L << ", " << R << "\n";
//                 noteType(res, tr);
//                 return res;
//             }

//             static const std::unordered_map<std::string, std::pair<std::string, std::string>> cmp_map = {
//                 {"==", {"eq", "eq"}}, {"!=", {"ne", "ne"}}, {"<", {"slt", "ult"}}, {"<=", {"sle", "ule"}}, {">", {"sgt", "ugt"}}, {">=", {"sge", "uge"}}};

//             auto it = cmp_map.find(node->value);
//             if (it != cmp_map.end()) {
//                 std::string signedOp = it->second.first;
//                 std::string unsignedOp = it->second.second;
//                 std::string cmpop = isSigned ? signedOp : unsignedOp;

//                 out << "  " << res << " = icmp " << cmpop << " " << intTy << " " << L << ", " << R << "\n";
//                 std::string zext = fresh();
//                 out << "  " << zext << " = zext i1 " << res << " to i8\n";
//                 noteType(zext, node->scope->lookupType("boolean"));
//                 return zext;
//             }

//             throw std::runtime_error("Unsupported integer op " + node->value);
//         }
//     }
//     std::string CodeGenLLVM::generateUnaryOperation(std::unique_ptr<ASTNode> node) {
//         auto &scope = *node->children[0]->scope;
//         auto varName = node->children[0]->value;
//         TypeInfo ti = scope.lookupType(scope.lookupVariable(varName).type);
//         NodeType child_type = node->children[0]->type;
//         auto val = emitExpression(std::move(node->children[0]));

//         std::string llvmType;
//         if (ti.isFloat)
//             llvmType = (ti.bits == 32) ? "float" : "double";
//         else
//             llvmType = "i" + std::to_string(ti.bits);

//         if (node->value == "!") {
//             TypeInfo boolType = node->scope->lookupType("boolean");
//             val = castValue(val, regType.at(val), boolType);
//             std::string res = fresh();
//             out << "  " << res << " = icmp eq " << llvmType << " " << val << ", 0\n";
//             std::string zero = fresh();
//             out << "  " << zero << " = zext i1 " << res << " to i8\n";
//             noteType(zero, node->scope->lookupType("boolean"));
//             return zero;
//         } else if (node->value == "++" || node->value == "--") {
//             if (child_type != NodeType::VariableAccess)
//                 throw std::runtime_error(node->value + " can only be applied to variables");

//             if (ti.isFloat)
//                 throw std::runtime_error("Increment/Decrement not supported on float");

//             bool isGlobal = scope.isGlobalVariable(varName);
//             std::string ptr = (isGlobal ? "@" + varName : "%" + node->scope->getMapping(varName));

//             std::string cur = fresh();
//             out << "  " << cur << " = load " << llvmType << ", " << llvmType << "* " << ptr << "\n";

//             std::string updated = fresh();
//             out << "  " << updated << " = "
//                 << (node->value == "++" ? "add" : "sub")
//                 << " " << llvmType << " " << cur << ", 1\n";

//             out << "  store " << llvmType << " " << updated << ", " << llvmType << "* " << ptr << "\n";

//             noteType(updated, ti);
//             return updated;
//         } else {
//             throw std::runtime_error("Unsupported unary: " + node->value);
//         }
//     }
//     std::string CodeGenLLVM::emitExpression(std::unique_ptr<ASTNode> node) {
//         switch (node->type) {
//         case NodeType::IntegerLiteral:
//             return generateIntegerLiteral(std::move(node));
//         case NodeType::FloatLiteral:
//             return generateFloatLiteral(std::move(node));
//         case NodeType::StringLiteral:
//             return generateStringLiteral(std::move(node));
//         case NodeType::BooleanLiteral:
//             return generateBooleanLiteral(std::move(node));
//         case NodeType::VariableAccess:
//             return generateVariableAccess(std::move(node));
//         case NodeType::BinaryOp:
//             return generateBinaryOperation(std::move(node));
//         case NodeType::UnaryOp:
//             return generateUnaryOperation(std::move(node));
//         default:
//             node->print(std::cout, 0);
//             throw std::runtime_error("Unknown expression encountered.");
//         }
//     }
//     void CodeGenLLVM::emitEpilogue(std::unique_ptr<ASTNode> blockNode) {
//         out << "    ; Block ends\n";
//     }
//     void CodeGenLLVM::emitPrologue(std::unique_ptr<ASTNode> blockNode) {
//     }
//     void CodeGenLLVM::generateStatement(std::unique_ptr<ASTNode> statement) {
//         switch (statement->type) {
//         case NodeType::VariableReassignment: {
//             generateVariableReassignment(std::move(statement));
//             break;
//         }
//         case NodeType::VariableDeclaration: {
//             generateVariableDeclaration(std::move(statement));
//             break;
//         }
//         case NodeType::IfStatement: {
//             generateIfStatement(std::move(statement));
//             break;
//         }
//         case NodeType::UnaryOp: {
//             if (statement->value == "--" or statement->value == "++") {
//                 std::string reg = emitExpression(std::move(statement));
//             }
//             break;
//         }
//         case NodeType::BinaryOp: {
//             std::string reg = emitExpression(std::move(statement));  // I am doing this just so the increments/decrements work in x + y-- -> this itself must not have any result, but y-- should still be effective.
//             break;
//         }
//         default:
//             statement->print(std::cout, 0);
//             throw std::runtime_error("Unknown statement encountered.");
//         }
//     }
//     void CodeGenLLVM::generateVariableReassignment(std::unique_ptr<ASTNode> node) {
//         auto &scope = *node->scope;
//         auto name = node->value;
//         TypeInfo ti = scope.lookupType(scope.lookupVariable(name).type);
//         bool isGlobal = scope.isGlobalVariable(name);

//         // 1) Compute the RHS expression
//         std::string val = emitExpression(std::move(node->children.back()));
//         TypeInfo tr = regType[val];

//         // 2) Convert value to target type (ti) using castValue
//         std::string castedVal = castValue(val, tr, ti);

//         // 3) Emit the store to the correct location
//         std::string ty = ti.isFloat
//                              ? (ti.bits == 32 ? "float" : "double")
//                              : "i" + std::to_string(ti.bits);

//         if (isGlobal) {
//             out << "  store " << ty << " " << castedVal << ", " << ty << "* @" << name << "\n";
//         } else {
//             out << "  store " << ty << " " << castedVal << ", " << ty << "* %" << node->scope->getMapping(name) << "\n";
//         }
//     }
//     void CodeGenLLVM::generateVariableDeclaration(std::unique_ptr<ASTNode> node) {
//         bool isGlobal = node->scope->isGlobalVariable(node->value);
//         TypeInfo ti = node->scope->lookupType(node->scope->lookupVariable(node->value).type);
//         std::string ty = ti.isFloat
//                              ? (ti.bits == 32 ? "float" : "double")
//                              : "i" + std::to_string(ti.bits);

//         if (!isGlobal) {
//             out << "  %" << node->scope->getMapping(node->value) << " = alloca " << ty << "\n";
//         }

//         if (node->children.size() >= 2) {
//             auto val = emitExpression(std::move(node->children.back()));
//             TypeInfo tr = regType[val];

//             // Use castValue for all type conversions
//             std::string castedVal = castValue(val, tr, ti);

//             if (isGlobal) {
//                 out << "  store " << ty << " " << castedVal << ", " << ty << "* @" << node->value << "\n";
//             } else {
//                 out << "  store " << ty << " " << castedVal << ", " << ty << "* %" << node->scope->getMapping(node->value) << "\n";
//             }
//         }
//     }
//     void CodeGenLLVM::generateIfStatement(std::unique_ptr<ASTNode> statement) {
//         int id = blockLabelCount++;
//         std::string thenLbl = "if.then" + std::to_string(id);
//         std::string elseLbl = "if.else" + std::to_string(id);
//         std::string endLbl = "if.end" + std::to_string(id);

//         auto condVal = emitExpression(std::move(statement->children[0]));
//         TypeInfo condTi = regType[condVal];

//         std::string condBool = fresh();
//         out << "    " << condBool
//             << " = trunc i" << condTi.bits
//             << " " << condVal << " to i1\n";

//         out << "    br i1 " << condBool
//             << ", label %" << thenLbl
//             << ", label %"
//             << (statement->getElseBranch() ? elseLbl : endLbl)
//             << "\n\n";

//         out << thenLbl << ":\n";
//         {
//             auto ifBlock = std::move(statement->children[1]);
//             auto children = std::move(ifBlock->children);
//             emitPrologue(std::move(ifBlock));
//             for (auto &stmt : children)
//                 generateStatement(std::move(stmt));
//             emitEpilogue();
//         }
//         out << "    br label %" << endLbl << "\n\n";

//         ASTNode *branch = statement->getElseBranch();
//         if (branch) {
//             out << elseLbl << ":\n";

//             while (branch) {
//                 if (branch->type == NodeType::ElseIfStatement) {
//                     int elifId = blockLabelCount++;
//                     std::string elifThen = "elif.then" + std::to_string(elifId);
//                     std::string elifNext = "elif.next" + std::to_string(elifId);

//                     auto elifCond = emitExpression(std::move(branch->children[0]));
//                     TypeInfo elifTi = regType[elifCond];
//                     std::string elifBool = fresh();
//                     out << "    " << elifBool
//                         << " = trunc i" << elifTi.bits
//                         << " " << elifCond << " to i1\n";
//                     out << "    br i1 " << elifBool
//                         << ", label %" << elifThen
//                         << ", label %" << elifNext
//                         << "\n\n";

//                     out << elifThen << ":\n";
//                     {
//                         auto elifBlock = std::move(branch->children[1]);
//                         auto elifChildren = std::move(elifBlock->children);
//                         emitPrologue(std::move(elifBlock));
//                         for (auto &stmt : elifChildren)
//                             generateStatement(std::move(stmt));
//                         emitEpilogue();
//                     }
//                     out << "    br label %" << endLbl << "\n\n";
//                     out << elifNext << ":\n";
//                     branch = branch->getElseBranch();
//                     if (!branch) {
//                         out << "    br label %" << endLbl << "\n\n";
//                     }
//                 } else if (branch->type == NodeType::ElseStatement) {
//                     auto elseBlock = std::move(branch->children[0]);
//                     auto elseChildren = std::move(elseBlock->children);
//                     emitPrologue(std::move(elseBlock));
//                     for (auto &stmt : elseChildren)
//                         generateStatement(std::move(stmt));
//                     emitEpilogue();
//                     out << "    br label %" << endLbl << "\n\n";
//                     branch = nullptr;
//                 } else {
//                     throw std::runtime_error("Unexpected node type in else chain");
//                 }
//             }
//         }

//         // 5) end label
//         out << endLbl << ":\n";
//     }
//     void CodeGenLLVM::generate(std::unique_ptr<ASTNode> program) {
//         outGlobal << "; ModuleID = 'zlang'\n";
//         outGlobal << "source_filename = \"zlang\"\n";
//         // Emit global variable definitions
//         for (auto &statement : program->children) {
//             if (statement->type == NodeType::VariableDeclaration) {
//                 auto &name = statement->value;
//                 auto ti = statement->scope->lookupType(
//                     statement->scope->lookupVariable(name).type);
//                 std::string ty = ti.isFloat
//                                      ? (ti.bits == 32 ? "float" : "double")
//                                      : "i" + std::to_string(ti.bits);
//                 outGlobal << "@" << name << " = global " << ty << (ti.isFloat ? " 0.0" : " 0") << "\n";
//             }
//         }
//         outGlobal << "\n";

//         // Define main function
//         out
//             << "define i32 @main() {\n";
//         emitPrologue(nullptr);
//         // Generate each top-level statement
//         for (auto &stmt : program->children) {
//             generateStatement(std::move(stmt));
//         }
//         out << "  ret i32 0\n";
//         out
//             << "}\n";

//         outfinal << outGlobal.str() + "\n\n"
//                  << out.str() << "\n\n";
//     }
//     std::string CodeGenLLVM::generateFunctionCall(std::unique_ptr<ASTNode> node) {
//         return "";
//     }
//     void CodeGenLLVM::generateFunctionDeclaration(std::unique_ptr<ASTNode> node) {}
//     void CodeGenLLVM::generateExternFunctionDeclaration(std::unique_ptr<ASTNode> node) {}
// }  // namespace zlang