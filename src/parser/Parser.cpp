#include "all.hpp"

// TODO: 4 Things then I'm done
// Loops
// Pointers
// Structures
// Stdlib

namespace zust {
    Parser::Parser(Lexer& lex) : lexer(lex) {
        currentScope = std::make_shared<NamespaceScope>("GLOBAL__SCOPE", nullptr);
        // TODO: This is controversial, lets make something in the near future that
        // gets these (size_t, integer, float, double, etc) sizes dynamically
        currentScope->defineType("boolean", TypeInfo{.bits = 8,
                                                     .align = 1,
                                                     .isFloat = false,
                                                     .isSigned = false,
                                                     .isString = false,
                                                     .isBoolean = true,
                                                     .isPointer = false,
                                                     .isUserDefined = false,
                                                     .isFunction = false,
                                                     .name = "boolean"});
        currentScope->defineType("none", TypeInfo{.bits = 0,
                                                  .align = 0,
                                                  .isFloat = false,
                                                  .isSigned = false,
                                                  .isString = false,
                                                  .isBoolean = false,
                                                  .isPointer = false,
                                                  .isUserDefined = false,
                                                  .isFunction = false,
                                                  .name = "none"});
        currentScope->defineType("string", TypeInfo{.bits = 64,
                                                    .align = 8,
                                                    .isFloat = false,
                                                    .isSigned = false,
                                                    .isString = true,
                                                    .isBoolean = false,
                                                    .isPointer = true,
                                                    .isUserDefined = false,
                                                    .isFunction = false,
                                                    .name = "string"});
        currentScope->defineType("size_t", TypeInfo{.bits = 64,
                                                    .align = 8,
                                                    .isFloat = false,
                                                    .isSigned = false,
                                                    .isString = false,
                                                    .isBoolean = false,
                                                    .isPointer = false,
                                                    .isUserDefined = false,
                                                    .isFunction = false,
                                                    .name = "size_t"});
        currentScope->defineType("integer", TypeInfo{.bits = 64,
                                                     .align = 8,
                                                     .isFloat = false,
                                                     .isSigned = true,
                                                     .isString = false,
                                                     .isBoolean = false,
                                                     .isPointer = false,
                                                     .isUserDefined = false,
                                                     .isFunction = false,
                                                     .name = "integer"});

        // Unsigned integers
        currentScope->defineType("uint8_t", TypeInfo{.bits = 8,
                                                     .align = 1,
                                                     .isFloat = false,
                                                     .isSigned = false,
                                                     .isString = false,
                                                     .isBoolean = false,
                                                     .isPointer = false,
                                                     .isUserDefined = false,
                                                     .isFunction = false,
                                                     .name = "uint8_t"});

        currentScope->defineType("uint16_t", TypeInfo{.bits = 16,
                                                      .align = 2,
                                                      .isFloat = false,
                                                      .isSigned = false,
                                                      .isString = false,
                                                      .isBoolean = false,
                                                      .isPointer = false,
                                                      .isUserDefined = false,
                                                      .isFunction = false,
                                                      .name = "uint16_t"});

        currentScope->defineType("uint32_t", TypeInfo{.bits = 32,
                                                      .align = 4,
                                                      .isFloat = false,
                                                      .isSigned = false,
                                                      .isString = false,
                                                      .isBoolean = false,
                                                      .isPointer = false,
                                                      .isUserDefined = false,
                                                      .isFunction = false,
                                                      .name = "uint32_t"});

        currentScope->defineType("uint64_t", TypeInfo{.bits = 64,
                                                      .align = 8,
                                                      .isFloat = false,
                                                      .isSigned = false,
                                                      .isString = false,
                                                      .isBoolean = false,
                                                      .isPointer = false,
                                                      .isUserDefined = false,
                                                      .isFunction = false,
                                                      .name = "uint64_t"});

        // Signed integers
        currentScope->defineType("int8_t", TypeInfo{.bits = 8,
                                                    .align = 1,
                                                    .isFloat = false,
                                                    .isSigned = true,
                                                    .isString = false,
                                                    .isBoolean = false,
                                                    .isPointer = false,
                                                    .isUserDefined = false,
                                                    .isFunction = false,
                                                    .name = "int8_t"});

        currentScope->defineType("int16_t", TypeInfo{.bits = 16,
                                                     .align = 2,
                                                     .isFloat = false,
                                                     .isSigned = true,
                                                     .isString = false,
                                                     .isBoolean = false,
                                                     .isPointer = false,
                                                     .isUserDefined = false,
                                                     .isFunction = false,
                                                     .name = "int16_t"});

        currentScope->defineType("int32_t", TypeInfo{.bits = 32,
                                                     .align = 4,
                                                     .isFloat = false,
                                                     .isSigned = true,
                                                     .isString = false,
                                                     .isBoolean = false,
                                                     .isPointer = false,
                                                     .isUserDefined = false,
                                                     .isFunction = false,
                                                     .name = "int32_t"});

        currentScope->defineType("int64_t", TypeInfo{.bits = 64,
                                                     .align = 8,
                                                     .isFloat = false,
                                                     .isSigned = true,
                                                     .isString = false,
                                                     .isBoolean = false,
                                                     .isPointer = false,
                                                     .isUserDefined = false,
                                                     .isFunction = false,
                                                     .name = "int64_t"});

        // Floating-point types
        currentScope->defineType("float", TypeInfo{.bits = 32,
                                                   .align = 8,
                                                   .isFloat = true,
                                                   .isSigned = true,
                                                   .isString = false,
                                                   .isBoolean = false,
                                                   .isPointer = false,
                                                   .isUserDefined = false,
                                                   .isFunction = false,
                                                   .name = "float"});

        currentScope->defineType("double", TypeInfo{.bits = 64,
                                                    .align = 16,
                                                    .isFloat = true,
                                                    .isSigned = true,
                                                    .isString = false,
                                                    .isBoolean = false,
                                                    .isPointer = false,
                                                    .isUserDefined = false,
                                                    .isFunction = false,
                                                    .name = "double"});
        shouldTypecheck = true;
        advance();
    }

    void Parser::advance() { currentToken = lexer.nextToken(); }

    bool Parser::match(Token::Token::Kind kind) {
        if (currentToken.kind == kind) {
            advance();
            return true;
        }
        return false;
    }

    void Parser::expect(Token::Token::Kind kind, const std::string& errMsg) {
        if (!match(kind)) {
            logError({ErrorType::Syntax,
                      errMsg + " at line " + std::to_string(currentToken.line) +
                          ", column " + std::to_string(currentToken.column)});
            shouldTypecheck = false;
            exit(0);
            return;
        }
    }

    std::unique_ptr<ASTNode> Parser::parse() {
        auto program = ASTNode::makeProgramNode(currentScope);
        while (currentToken.kind != Token::Token::Kind::EndOfFile) {
            auto stmt = parseStatement();
            if (stmt)
                program->addChild(std::move(stmt));
        }
        return program;
    }

    std::unique_ptr<ASTNode> Parser::parseStatement() {
        if (currentToken.kind == Token::Kind::Return) {
            if (currentScope->isGlobalScope()) {
                logError({ErrorType::Syntax,
                          "Unexpected keyword '" + currentToken.text +
                              "' outside any function at line " +
                              std::to_string(currentToken.line) + ", column " +
                              std::to_string(currentToken.column) + "."});
                shouldTypecheck = false;
                advance();
                return nullptr;
            } else {
                advance();
                auto node = std::make_unique<ASTNode>(NodeType::ReturnStatement, "",
                                                      currentScope);
                if (currentToken.kind == Token::Kind::SemiColon) {
                    auto ret = std::make_unique<ASTNode>(NodeType::Symbol, "none",
                                                         currentScope);
                    node->addChild(std::move(ret));
                } else {
                    node->addChild(parseExpression(true));
                }
                return node;
            }
        }
        if ((currentToken.kind == Token::Kind::Symbol and
             currentToken.text == "extern") ||
            currentToken.kind == Token::Kind::Function)
            return parseFunctionDeclaration();
        if (match(Token::Token::Kind::Let))
            return parseVariableDeclaration();
        if (currentToken.kind == Token::Token::Kind::Identifier and lexer.peek().kind == Token::Kind::Equal)
            return parseVariableReassignment();
        if (currentToken.kind == Token::Kind::If ||
            currentToken.kind == Token::Kind::ElseIf ||
            currentToken.kind == Token::Kind::Else)
            return parseConditionals();
        if (currentToken.kind == Token::Kind::Symbol ||
            currentToken.kind == Token::Kind::Identifier)
            return parseExpression(true);
        logError({ErrorType::Syntax,
                  "Unexpected token '" + currentToken.text + "' at line " +
                      std::to_string(currentToken.line) + ", column " +
                      std::to_string(currentToken.column) + "."});
        shouldTypecheck = false;
        return nullptr;
    }

    std::unique_ptr<ASTNode> Parser::parseFunctionDeclaration() {
        bool isExtern = false;
        bool isVariadic = false;
        if (currentToken.kind == Token::Kind::Symbol &&
            currentToken.text == "extern") {
            isExtern = true;
            advance();
        }

        // Expect 'fn' keyword
        expect(Token::Kind::Function,
               "Expected 'fn' to start function declaration");

        // Function name
        std::string name = currentToken.text;
        expect(Token::Kind::Identifier, "Expected function name after 'fn'");

        // Parameter list
        expect(Token::Kind::LeftParen, "Expected '(' after function name");

        std::vector<ParamInfo> params;
        if (currentToken.kind != Token::Kind::RightParen) {
            // Parse first parameter
            std::string paramName = currentToken.text;
            expect(Token::Kind::Identifier, "Expected parameter name");

            expect(Token::Kind::Colon, "Expected ':' after parameter name");

            if (currentToken.kind != Token::Kind::Identifier) {
                logError(Error{ErrorType::Syntax,
                               "Expected data type after ':' at line " +
                                   std::to_string(currentToken.line) + ", column " +
                                   std::to_string(currentToken.column)});
                shouldTypecheck = false;
                return nullptr;
            }
            std::string typeName = currentToken.text;
            advance();

            params.push_back(ParamInfo{.name = paramName, .type = typeName});

            while (match(Token::Kind::Comma)) {
                if (currentToken.kind == Token::Kind::Ellipsis) {
                    if (!isExtern) {
                        logError(Error{ErrorType::Generic, "We only support variadic arguments with extern functions"});
                        shouldTypecheck = false;
                        return nullptr;
                    }
                    isVariadic = true;
                    advance();

                    // Ellipsis must be the last thing before ')'
                    if (currentToken.kind != Token::Kind::RightParen) {
                        logError(Error{ErrorType::Syntax,
                                       "Variadic parameter '...' must be the last in the parameter list at line " +
                                           std::to_string(currentToken.line) + ", column " +
                                           std::to_string(currentToken.column)});
                        shouldTypecheck = false;
                        return nullptr;
                    }
                    break;
                }

                std::string nextName = currentToken.text;
                expect(Token::Kind::Identifier,
                       "Expected parameter name after ','");

                expect(Token::Kind::Colon, "Expected ':' after parameter name");

                if (currentToken.kind != Token::Kind::Identifier) {
                    logError(Error{ErrorType::Syntax,
                                   "Expected data type after ':' at line " +
                                       std::to_string(currentToken.line) +
                                       ", column " +
                                       std::to_string(currentToken.column)});
                    shouldTypecheck = false;
                    return nullptr;
                }
                std::string nextType = currentToken.text;
                advance();

                params.push_back(ParamInfo{.name = nextName, .type = nextType});
            }
        }
        expect(Token::Kind::RightParen, "Expected ')' after parameters");

        // Return type (optional)
        std::string returnTypeName = "none";
        if (match(Token::Kind::Arrow)) {
            if (currentToken.kind != Token::Kind::Identifier) {
                logError(
                    Error{ErrorType::Syntax,
                          "Expected return type identifier after '->' at line " +
                              std::to_string(currentToken.line) + ", column " +
                              std::to_string(currentToken.column)});
                shouldTypecheck = false;
                return nullptr;
            }
            returnTypeName = currentToken.text;
            advance();
        }

        if (isExtern) {
            expect(Token::Kind::SemiColon,
                   "Expected ';' after extern function declaration");
            return ASTNode::makeExternFunctionDeclaration(name, currentScope,
                                                          params, returnTypeName, isVariadic);
        }
        enterScope(currentScope->name() + "___" + name, true);
        auto body = parseBlock();
        exitScope();
        if (name == "main") {
            if (currentScope->parent() == nullptr) {
                // Ok go ahead
            } else {
                throw std::runtime_error("Main should be in global scope");
            }
        }
        return ASTNode::makeFunctionDeclaration(name, currentScope, params,
                                                returnTypeName, std::move(body), isVariadic);
    }

    std::unique_ptr<ASTNode> Parser::parseBlock() {
        expect(Token::Kind::LeftBrace,
               "Expected a '{' to open a scope, at line " +
                   std::to_string(currentToken.line) + ", column " +
                   std::to_string(currentToken.column) + ".");
        auto blockNode =
            std::make_unique<ASTNode>(NodeType::Program, "", currentScope);
        while (!match(Token::Kind::RightBrace) &&
               currentToken.kind != Token::Kind::EndOfFile) {
            if (currentToken.kind == Token::Kind::EndOfFile) {
                logError({ErrorType::Syntax,
                          "Expected token '}' found 'End Of File' at line " +
                              std::to_string(currentToken.line) + ", column " +
                              std::to_string(currentToken.column) + "."});
                shouldTypecheck = false;
                return nullptr;
            }
            blockNode->addChild(parseStatement());
        }
        return blockNode;
    }

    std::unique_ptr<ASTNode> Parser::parseConditionals() {
        if (!match(Token::Kind::If)) {
            logError({ErrorType::Syntax, "Expected 'if'"});
            shouldTypecheck = false;
            exit(1);
        }

        expect(Token::Kind::LeftParen,
               "Expected a '(' after if block at line " +
                   std::to_string(currentToken.line) + ", column " +
                   std::to_string(currentToken.column) + ".");
        auto condition = parseExpression();
        expect(Token::Kind::RightParen,
               "Expected a ')' after condition at line " +
                   std::to_string(currentToken.line) + ", column " +
                   std::to_string(currentToken.column) + ".");

        enterScope("Block__" + std::to_string(++blockNumber), false);
        auto ifBlock = parseBlock();
        exitScope();
        auto root = ASTNode::makeIfStatement(std::move(condition),
                                             std::move(ifBlock), currentScope);
        ASTNode* current = root.get();

        while (match(Token::Kind::ElseIf)) {
            expect(Token::Kind::LeftParen,
                   "Expected a '(' after if block at line " +
                       std::to_string(currentToken.line) + ", column " +
                       std::to_string(currentToken.column) + ".");
            auto condition = parseExpression();
            expect(Token::Kind::RightParen,
                   "Expected a ')' after condition at line " +
                       std::to_string(currentToken.line) + ", column " +
                       std::to_string(currentToken.column) + ".");
            enterScope("Block__" + std::to_string(++blockNumber), false);
            auto elifBlock = parseBlock();
            exitScope();
            auto elifNode = ASTNode::makeElseIfStatement(
                std::move(condition), std::move(elifBlock), currentScope);
            current->setElseBranch(std::move(elifNode));
            current = current->getElseBranch();
        }

        // optional 'else'
        if (match(Token::Kind::Else)) {
            enterScope("Block__" + std::to_string(++blockNumber), false);
            auto elseBlock = parseBlock();
            exitScope();
            auto elseNode =
                ASTNode::makeElseStatement(std::move(elseBlock), currentScope);
            current->setElseBranch(std::move(elseNode));
        }

        return root;
    }

    std::unique_ptr<ASTNode> Parser::parseVariableDeclaration() {
        if (currentToken.kind != Token::Token::Kind::Identifier)
            expect(Token::Token::Kind::Identifier,
                   "Expected variable name after 'let'");

        std::string name = currentToken.text;
        advance();
        std::unique_ptr<ASTNode> typeNode, initNode;

        if (match(Token::Kind::Colon)) {
            typeNode = ASTNode::makeSymbolNode(currentToken.text, currentScope);
            advance();
        } else {
            // TODO: Handle type deduction but its a far far task.
            logError(
                Error(
                    ErrorType::Generic, "Expected ':' followed by a type name for declaring variables at line " + std::to_string(currentToken.line) + ", column: " + std::to_string(currentToken.column)));
            shouldTypecheck = false;
            advance();
            return nullptr;
        }
        if (match(Token::Kind::Equal)) {
            initNode = parseExpression();
        }
        if (initNode == nullptr) {
            TypeInfo ty = currentScope->lookupType(typeNode->value);
            if (zust::numeric_types.find(typeNode->value) !=
                zust::numeric_types.end()) {
                if (ty.isFloat) {
                    if (ty.bits == 32)
                        initNode =
                            ASTNode::makeFloatLiteralNode("0.0F", currentScope);
                    else
                        initNode =
                            ASTNode::makeFloatLiteralNode("0.0", currentScope);
                } else
                    initNode = ASTNode::makeIntegerLiteralNode("0", currentScope);
            } else {
                if (typeNode->value == "boolean")
                    initNode = ASTNode::makeBooleanLiteralNode(true, currentScope);

                else if (typeNode->value == "string")
                    initNode = ASTNode::makeStringLiteralNode("", currentScope);
            }
        }
        expect(Token::Kind::SemiColon, "Expected ';' after declaration");
        std::optional<std::unique_ptr<ASTNode>> result =
            ASTNode::makeVariableDeclarationNode(name, std::move(typeNode),
                                                 std::move(initNode), currentScope);
        if (!result.has_value()) {
            logError(
                Error(ErrorType::Generic,
                      "Variable '" + name + "' already defined in current scope."));
            shouldTypecheck = false;
            return nullptr;
        } else {
            return std::move(result.value());
        }
    }

    std::unique_ptr<ASTNode> Parser::parseVariableReassignment() {
        std::string name = currentToken.text;
        advance();

        expect(Token::Token::Kind::Equal, "Expected '=' for variable reassignment");
        auto expr = parseExpression();
        expect(Token::Token::Kind::SemiColon, "Expected ';' after reassignment");
        return ASTNode::makeVariableReassignmentNode(name, std::move(expr),
                                                     currentScope);
    }

    std::unique_ptr<ASTNode> Parser::parseExpression(bool expect_exclaim) {
        auto lhs = parseUnary();
        auto ans = parseBinaryRHS(0, std::move(lhs));
        if (expect_exclaim) {
            expect(Token::Kind::SemiColon, "';' Expected at the end of statement.");
        }
        return ans;
    }

    std::unique_ptr<ASTNode> Parser::parsePrimary() {
        if (currentToken.kind == Token::Kind::BoolLiteral) {
            bool val = (currentToken.text == "true");
            advance();
            return ASTNode::makeBooleanLiteralNode(val, currentScope);
        }
        if (currentToken.kind == Token::Token::Kind::StringLiteral) {
            std::string s = currentToken.text;
            advance();
            return ASTNode::makeStringLiteralNode(s, currentScope);
        }
        if (currentToken.kind == Token::Token::Kind::FloatLiteral) {
            std::string f = currentToken.text;
            advance();
            return ASTNode::makeFloatLiteralNode(f, currentScope);
        }
        if (currentToken.kind == Token::Token::Kind::IntegerLiteral) {
            std::string val = currentToken.text;
            advance();
            return ASTNode::makeIntegerLiteralNode(val, currentScope);
        }
        if (currentToken.kind == Token::Token::Kind::Identifier) {
            std::string name = currentToken.text;
            advance();

            if (currentToken.kind == Token::Kind::LeftParen) {
                advance();
                std::vector<std::unique_ptr<ASTNode>> arguments;

                if (currentToken.kind != Token::Kind::RightParen) {
                    while (true) {
                        arguments.push_back(parseExpression(false));
                        if (currentToken.kind == Token::Kind::Comma) {
                            advance();
                        } else {
                            break;
                        }
                    }
                }
                expect(Token::Kind::RightParen, "Expected ')' after function arguments");
                return ASTNode::makeFunctionCall(name, std::move(arguments), currentScope);
            }

            auto node = ASTNode::makeVariableAccessNode(name, currentScope);
            if (currentToken.kind == Token::Kind::Symbol &&
                (currentToken.text == "++" || currentToken.text == "--")) {
                std::string op = currentToken.text;
                advance();
                node = ASTNode::makeUnaryOp(op, std::move(node), currentScope);
            }

            return node;
        }
        logError({ErrorType::Syntax,
                  "Expected expression, got '" + currentToken.text + "' at line " +
                      std::to_string(currentToken.line) + ", column " +
                      std::to_string(currentToken.column)});
        shouldTypecheck = false;
        return nullptr;
    }

    std::unique_ptr<ASTNode> Parser::parseUnary() {
        if (currentToken.kind == Token::Kind::Symbol &&
            (currentToken.text == "++" || currentToken.text == "--" ||
             currentToken.text == "!")) {
            std::string op = currentToken.text;
            advance();
            auto operand = parseUnary();
            return ASTNode::makeUnaryOp(op, std::move(operand), currentScope);
        }
        return parsePrimary();
    }

    std::unique_ptr<ASTNode> Parser::parseBinaryRHS(int exprPrec,
                                                    std::unique_ptr<ASTNode> lhs) {
        while (true) {
            // Only consider tokens that can represent binary operators
            if (!(currentToken.kind == Token::Kind::Symbol ||
                  (currentToken.kind == Token::Kind::Equal and
                   currentToken.text == "==")))
                break;

            std::string op = currentToken.text;
            int tokPrec = getPrecedence(op);

            if (tokPrec < exprPrec)
                return lhs;

            advance();

            // parse RHS
            auto rhs = parseUnary();
            int nextPrec = getPrecedence(currentToken.text);

            if (tokPrec < nextPrec)
                rhs = parseBinaryRHS(tokPrec + 1, std::move(rhs));

            lhs = ASTNode::makeBinaryOp(op, std::move(lhs), std::move(rhs),
                                        currentScope);
        }
        return lhs;
    }

    int Parser::getPrecedence(const std::string& op) const {
        static std::map<std::string, int> prec = {
            {"||", 1}, {"&&", 2}, {"==", 3}, {"!=", 3}, {"<", 4}, {">", 4}, {"<=", 4}, {">=", 4}, {"+", 5}, {"-", 5}, {"*", 6}, {"/", 6}};

        auto it = prec.find(op);
        return it == prec.end() ? -1 : it->second;
    }

    void Parser::enterScope(const std::string& name, bool isFunction) {
        if (isFunction) {
            currentScope = std::make_shared<FunctionScope>(name, currentScope);
        } else {
            auto funcScope = currentScope->findEnclosingFunctionScope();
            if (!funcScope) {
                throw std::runtime_error("Block scope must be inside a function scope");
            }
            currentScope = std::make_shared<BlockScope>(name, funcScope, currentScope);
        }
    }
    void Parser::exitScope() {
        if (!currentScope->parent())
            throw std::runtime_error("Scope underflow");
        currentScope = currentScope->parent();
    }
}  // namespace zust
