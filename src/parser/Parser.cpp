#include "all.hpp"

namespace zlang
{
    Parser::Parser(Lexer &lex) : lexer(lex)
    {
        currentScope = std::make_shared<ScopeContext>(nullptr);
        // TODO: This is controversial, lets make something in the near future that gets these (size_t, integer, float, double, etc) sizes dynamically
        currentScope->defineType("boolean", TypeInfo{8, 1, false, false});
        currentScope->defineType("string", TypeInfo{64, 8, false, false}); // Assuming pointer to heap
        currentScope->defineType("size_t", TypeInfo{64, 8, false, false});
        currentScope->defineType("integer", TypeInfo{64, 8, false, true}); // Default int type

        // Unsigned integers
        currentScope->defineType("uint8_t", TypeInfo{8, 1, false, false});
        currentScope->defineType("uint16_t", TypeInfo{16, 2, false, false});
        currentScope->defineType("uint32_t", TypeInfo{32, 4, false, false});
        currentScope->defineType("uint64_t", TypeInfo{64, 8, false, false});

        // Signed integers
        currentScope->defineType("int8_t", TypeInfo{8, 1, false, true});
        currentScope->defineType("int16_t", TypeInfo{16, 2, false, true});
        currentScope->defineType("int32_t", TypeInfo{32, 4, false, true});
        currentScope->defineType("int64_t", TypeInfo{64, 8, false, true});

        // Floating-point types
        currentScope->defineType("float", TypeInfo{32, 4, true, true});
        currentScope->defineType("double", TypeInfo{64, 8, true, true});
        shouldTypecheck = true;
        advance();
    }

    void Parser::advance()
    {
        currentToken = lexer.nextToken();
    }

    bool Parser::match(Token::Token::Kind kind)
    {
        if (currentToken.kind == kind)
        {
            advance();
            return true;
        }
        return false;
    }

    void Parser::expect(Token::Token::Kind kind, const std::string &errMsg)
    {
        if (!match(kind))
        {
            logError({ErrorType::Syntax,
                      errMsg +
                          " at line " + std::to_string(currentToken.line) +
                          ", column " + std::to_string(currentToken.column)});
            shouldTypecheck = false;
            return;
        }
    }

    std::unique_ptr<ASTNode> Parser::parse()
    {
        auto program = ASTNode::makeProgramNode(currentScope);
        while (currentToken.kind != Token::Token::Kind::EndOfFile)
        {
            auto stmt = parseStatement();
            if (stmt)
                program->addChild(std::move(stmt));
        }
        return program;
    }

    std::unique_ptr<ASTNode> Parser::parseStatement()
    {
        if (match(Token::Token::Kind::Let))
            return parseVariableDeclaration();
        if (currentToken.kind == Token::Token::Kind::Identifier)
            return parseVariableReassignment();
        if (currentToken.kind == Token::Kind::If || currentToken.kind == Token::Kind::ElseIf || currentToken.kind == Token::Kind::Else)
            return parseConditionals();
        logError({ErrorType::Syntax,
                  "Unexpected token '" + currentToken.text +
                      "' at line " + std::to_string(currentToken.line) +
                      ", column " + std::to_string(currentToken.column) + "."});
        shouldTypecheck = false;
        return nullptr;
    }

    std::unique_ptr<ASTNode> Parser::parseBlock()
    {
        expect(Token::Kind::LeftBrace, "Expected a '{' to open a scope, at line " + std::to_string(currentToken.line) + ", column " + std::to_string(currentToken.column) + ".");
        enterScope();
        auto blockNode = std::make_unique<ASTNode>(NodeType::Program, "", currentScope);
        while (!match(Token::Kind::RightBrace) && currentToken.kind != Token::Kind::EndOfFile)
        {
            if (currentToken.kind == Token::Kind::EndOfFile)
            {
                logError({ErrorType::Syntax,
                          "Expected token '}' found 'End Of File' at line " + std::to_string(currentToken.line) +
                              ", column " + std::to_string(currentToken.column) + "."});
                shouldTypecheck = false;
                return nullptr;
            }
            blockNode->addChild(parseStatement());
        }
        exitScope();
        return blockNode;
    }

    std::unique_ptr<ASTNode> Parser::parseConditionals()
    {
        if (!match(Token::Kind::If))
        {
            logError({ErrorType::Syntax, "Expected 'if'"});
            shouldTypecheck = false;
            exit(1);
        }

        expect(Token::Kind::LeftParen, "Expected a '(' after if block at line " + std::to_string(currentToken.line) + ", column " + std::to_string(currentToken.column) + ".");
        auto condition = parseExpression();
        expect(Token::Kind::RightParen, "Expected a ')' after condition at line " + std::to_string(currentToken.line) + ", column " + std::to_string(currentToken.column) + ".");

        auto ifBlock = parseBlock();
        auto root = ASTNode::makeIfStatement(std::move(condition), std::move(ifBlock), currentScope);
        ASTNode *current = root.get();

        while (match(Token::Kind::ElseIf))
        {
            expect(Token::Kind::LeftParen, "Expected a '(' after if block at line " + std::to_string(currentToken.line) + ", column " + std::to_string(currentToken.column) + ".");
            auto condition = parseExpression();
            expect(Token::Kind::RightParen, "Expected a ')' after condition at line " + std::to_string(currentToken.line) + ", column " + std::to_string(currentToken.column) + ".");
            auto elifBlock = parseBlock();
            auto elifNode = ASTNode::makeElseIfStatement(std::move(condition), std::move(elifBlock), currentScope);
            current->setElseBranch(std::move(elifNode));
            current = current->getElseBranch();
        }

        // optional 'else'
        if (match(Token::Kind::Else))
        {
            auto elseBlock = parseBlock();
            auto elseNode = ASTNode::makeElseStatement(std::move(elseBlock), currentScope);
            current->setElseBranch(std::move(elseNode));
        }

        return root;
    }

    std::unique_ptr<ASTNode> Parser::parseVariableDeclaration()
    {
        if (currentToken.kind != Token::Token::Kind::Identifier)
            expect(Token::Token::Kind::Identifier, "Expected variable name after 'let'");

        std::string name = currentToken.text;
        advance();
        std::unique_ptr<ASTNode> typeNode, initNode;

        if (match(Token::Kind::Colon))
        {
            typeNode = ASTNode::makeSymbolNode(currentToken.text, currentScope);
            advance();
        }
        if (match(Token::Kind::Equal))
        {
            initNode = parseExpression();
        }
        if (initNode == nullptr)
        {
            TypeInfo ty = currentScope->lookupType(typeNode->value);
            if (zlang::numeric_types.find(typeNode->value) != zlang::numeric_types.end())
            {
                if (ty.isFloat)
                {
                    if (ty.bits == 32)
                        initNode = ASTNode::makeFloatLiteralNode("0.0F", currentScope);
                    else
                        initNode = ASTNode::makeFloatLiteralNode("0.0", currentScope);
                }
                else
                    initNode = ASTNode::makeIntegerLiteralNode("0", currentScope);
            }
            else
            {
                if (typeNode->value == "boolean")
                    initNode = ASTNode::makeBooleanLiteralNode(true, currentScope);

                else if (typeNode->value == "string")
                    initNode = ASTNode::makeStringLiteralNode("", currentScope);
            }
        }
        expect(Token::Kind::SemiColon, "Expected ';' after declaration");
        return ASTNode::makeVariableDeclarationNode(name, std::move(typeNode), std::move(initNode), currentScope);
    }

    std::unique_ptr<ASTNode> Parser::parseVariableReassignment()
    {
        std::string name = currentToken.text;
        advance();

        expect(Token::Token::Kind::Equal, "Expected '=' for variable reassignment");
        auto expr = parseExpression();
        expect(Token::Token::Kind::SemiColon, "Expected ';' after reassignment");
        return ASTNode::makeVariableReassignmentNode(name, std::move(expr), currentScope);
    }

    std::unique_ptr<ASTNode> Parser::parseExpression()
    {
        auto lhs = parseUnary();
        return parseBinaryRHS(0, std::move(lhs));
    }

    std::unique_ptr<ASTNode> Parser::parsePrimary()
    {
        if (currentToken.kind == Token::Kind::BoolLiteral)
        {
            bool val = (currentToken.text == "true");
            advance();
            return ASTNode::makeBooleanLiteralNode(val, currentScope);
        }
        if (currentToken.kind == Token::Token::Kind::StringLiteral)
        {
            std::string s = currentToken.text;
            advance();
            return ASTNode::makeStringLiteralNode(s, currentScope);
        }
        if (currentToken.kind == Token::Token::Kind::FloatLiteral)
        {
            std::string f = currentToken.text;
            advance();
            return ASTNode::makeFloatLiteralNode(f, currentScope);
        }
        if (currentToken.kind == Token::Token::Kind::IntegerLiteral)
        {
            std::string val = currentToken.text;
            advance();
            return ASTNode::makeIntegerLiteralNode(val, currentScope);
        }
        if (currentToken.kind == Token::Token::Kind::Identifier)
        {
            std::string name = currentToken.text;
            advance();

            // 2) make the VariableAccess node
            auto node = ASTNode::makeVariableAccessNode(name, currentScope);

            // 3) check if the *current* token is ++ or -- (postfix)
            if (currentToken.kind == Token::Kind::Symbol &&
                (currentToken.text == "++" || currentToken.text == "--"))
            {
                std::string op = currentToken.text; // "++" or "--"
                advance();                          // consume the ++/--
                node = ASTNode::makeUnaryOp(op, std::move(node), currentScope);
            }

            return node;
        }
        logError({ErrorType::Syntax,
                  "Expected expression, got '" + currentToken.text +
                      "' at line " + std::to_string(currentToken.line) +
                      ", column " + std::to_string(currentToken.column)});
        shouldTypecheck = false;
        return nullptr;
    }

    std::unique_ptr<ASTNode> Parser::parseUnary()
    {
        if (currentToken.kind == Token::Kind::Symbol &&
            (currentToken.text == "++" || currentToken.text == "--" || currentToken.text == "!"))
        {
            std::string op = currentToken.text;
            advance();
            auto operand = parseUnary();
            return ASTNode::makeUnaryOp(op, std::move(operand), currentScope);
        }
        return parsePrimary();
    }

    std::unique_ptr<ASTNode> Parser::parseBinaryRHS(int exprPrec,
                                                    std::unique_ptr<ASTNode> lhs)
    {
        while (true)
        {
            if (currentToken.kind != Token::Kind::Symbol)
                break;
            int tokPrec = getPrecedence(currentToken.text);
            if (tokPrec < exprPrec)
                return lhs;

            std::string op = currentToken.text;
            advance();

            // parse RHS
            auto rhs = parseUnary();
            int nextPrec = getPrecedence(currentToken.text);
            if (tokPrec < nextPrec)
                rhs = parseBinaryRHS(tokPrec + 1, std::move(rhs));

            lhs = ASTNode::makeBinaryOp(op, std::move(lhs), std::move(rhs), currentScope);
        }
        return lhs;
    }

    int Parser::getPrecedence(const std::string &op) const
    {
        static std::map<std::string, int> prec = {
            {"||", 1}, {"&&", 2}, {"==", 3}, {"!=", 3}, {"<", 4}, {">", 4}, {"<=", 4}, {">=", 4}, {"+", 5}, {"-", 5}, {"*", 6}, {"/", 6}};

        auto it = prec.find(op);
        return it == prec.end() ? -1 : it->second;
    }

    void Parser::enterScope()
    {
        currentScope = std::make_shared<ScopeContext>(currentScope);
    }
    void Parser::exitScope()
    {
        if (!currentScope->parent_)
            throw std::runtime_error("Scope underflow");
        currentScope = currentScope->parent_;
    }
} // namespace zlang
