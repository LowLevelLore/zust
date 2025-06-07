#include "all.hpp"

namespace zlang
{
    void Parser::consume()
    {
        current_ = lexer_.nextToken();
    }

    bool Parser::match(Token::Kind kind, const std::string &text) const
    {
        return current_.kind == kind && (text.empty() || current_.text == text);
    }

    bool Parser::matchConsume(Token::Kind kind, const std::string &text)
    {
        if (match(kind, text))
        {
            consume();
            return true;
        }
        return false;
    }

    Error Parser::expect(Token::Kind kind, const std::string &text)
    {
        if (matchConsume(kind, text))
            return {};
        return Error(ErrorType::Syntax, "Expected '" + text + "' at line " + std::to_string(current_.line));
    }

    Error Parser::parseProgram(std::unique_ptr<ASTNode> &out)
    {
        out = ASTNode::makeProgramNode();

        while (!match(Token::Kind::EndOfFile))
        {
            if (match(Token::Kind::Keyword, "fn"))
            {
                auto func = parseFunctionDeclaration();
                if (!func)
                    return Error(ErrorType::Syntax, "Invalid function declaration.");
                out->addChild(std::move(func));
            }
            else if (match(Token::Kind::Keyword, "if"))
            {
                auto ifNode = parseIf();
                if (!ifNode)
                    return Error(ErrorType::Syntax, "Invalid if statement.");
                out->addChild(std::move(ifNode));
            }
            else if (match(Token::Kind::Keyword, "pint"))
            {
                auto pint = parseDebugPrint();
                if (!pint)
                    return Error(ErrorType::Syntax, "Invalid pint statement.");
                out->addChild(std::move(pint));
            }
            else if (current_.kind == Token::Kind::Identifier)
            {
                Token nameToken = current_;
                Token next = lexer_.peek(1);

                if (next.kind == Token::Kind::Symbol && next.text == ":")
                {
                    auto decl = parseVariableDeclaration();
                    if (!decl)
                        return Error(ErrorType::Syntax, "Invalid variable declaration.");
                    out->addChild(std::move(decl));

                    // Check for = assignment
                    if (match(Token::Kind::Symbol, "="))
                    {
                        consume(); // consume '='
                        auto expr = parseExpression();
                        if (!expr)
                            return Error(ErrorType::Syntax, "Expected expression after '='.");
                        auto reassign = ASTNode::makeVariableReassignmentNode(nameToken.text, std::move(expr));
                        out->addChild(std::move(reassign));
                    }
                }
                else if (next.kind == Token::Kind::Symbol && next.text == ":=")
                {
                    auto reassign = parseReassignment();
                    if (!reassign)
                        return Error(ErrorType::Syntax, "Invalid reassignment.");
                    out->addChild(std::move(reassign));
                }
                else
                {
                    auto expr = parseExpression();
                    if (!expr)
                        return Error(ErrorType::Syntax, "Invalid expression.");
                    out->addChild(std::move(expr));
                }
            }
            else
            {
                auto expr = parseExpression();
                if (!expr)
                    return Error(ErrorType::Syntax, "Invalid expression.");
                out->addChild(std::move(expr));
            }
        }

        return {};
    }

    std::unique_ptr<ASTNode> Parser::parseExpression(int prec)
    {
        auto lhs = parsePrimary();
        if (!lhs)
            return nullptr;

        while (true)
        {
            int tokPrec = getTokenPrecedence();
            if (tokPrec < prec)
                break;
            std::string op = current_.text;
            consume();
            auto rhs = parseExpression(tokPrec + 1);
            if (!rhs)
                return nullptr;
            lhs = ASTNode::makeBinaryOperatorNode(op, std::move(lhs), std::move(rhs));
        }

        return lhs;
    }

    std::unique_ptr<ASTNode> Parser::parsePrimary()
    {
        if (matchConsume(Token::Kind::IntegerLiteral))
            return ASTNode::makeIntegerLiteralNode(current_.text);

        if (matchConsume(Token::Kind::FloatLiteral))
            return ASTNode::makeFloatLiteralNode(current_.text);

        if (matchConsume(Token::Kind::StringLiteral))
            return ASTNode::makeStringLiteralNode(current_.text);

        if (match(Token::Kind::Identifier))
        {
            Token name = current_;
            if (lexer_.peek(1).kind == Token::Kind::Symbol && lexer_.peek(1).text == ":=")
                return parseReassignment();

            if (lexer_.peek(1).text == "(")
                return parseIdentifierOrCall();

            consume(); // plain variable access
            return ASTNode::makeVariableAccessNode(name.text);
        }

        if (match(Token::Kind::Keyword, "fn"))
            return parseFunctionDeclaration();

        if (match(Token::Kind::Keyword, "pint"))
            return parseDebugPrint();

        if (matchConsume(Token::Kind::Symbol, "("))
            return parseGroupedExpression();

        logError(Error(ErrorType::Syntax, "Unexpected token: " + current_.text));
        return nullptr;
    }

    std::unique_ptr<ASTNode> Parser::parseVariableDeclaration()
    {
        Token nameToken = current_;
        consume(); // identifier

        if (!matchConsume(Token::Kind::Symbol, ":"))
        {
            logError(Error(ErrorType::Syntax, "Expected ':' after variable name."));
            return nullptr;
        }

        if (!match(Token::Kind::Identifier))
        {
            logError(Error(ErrorType::Syntax, "Expected type name after ':'."));
            return nullptr;
        }

        Token typeToken = current_;
        consume(); // type name

        // We drop the type and make initializer null (not stored in AST)
        return ASTNode::makeVariableDeclarationNode(nameToken.text, nullptr);
    }

    std::unique_ptr<ASTNode> Parser::parseReassignment()
    {
        Token nameToken = current_;
        consume(); // identifier

        if (!matchConsume(Token::Kind::Symbol, ":="))
        {
            logError(Error(ErrorType::Syntax, "Expected ':=' after identifier."));
            return nullptr;
        }

        auto expr = parseExpression();
        return ASTNode::makeVariableReassignmentNode(nameToken.text, std::move(expr));
    }

    std::unique_ptr<ASTNode> Parser::parseIdentifierOrCall()
    {
        std::string name = current_.text;
        consume(); // identifier consumed
        expect(Token::Kind::Symbol, "(");
        auto argsList = ASTNode::makeFunctionArgsListNode();
        while (!match(Token::Kind::Symbol, ")"))
        {
            auto arg = parseExpression();
            argsList->addChild(std::move(arg));
            if (!matchConsume(Token::Kind::Symbol, ","))
                break;
        }
        expect(Token::Kind::Symbol, ")");
        return ASTNode::makeFunctionCallNode(name, std::move(argsList));
    }

    std::unique_ptr<ASTNode> Parser::parseIf()
    {
        consume(); // consume 'if'
        auto cond = parseExpression();
        expect(Token::Kind::Symbol, "{");
        auto thenBranch = ASTNode::makeProgramNode();
        while (!match(Token::Kind::Symbol, "}"))
        {
            thenBranch->addChild(parseExpression());
        }
        expect(Token::Kind::Symbol, "}");
        std::unique_ptr<ASTNode> elseBranch = nullptr;
        if (matchConsume(Token::Kind::Identifier, "else"))
        {
            expect(Token::Kind::Symbol, "{");
            elseBranch = ASTNode::makeProgramNode();
            while (!match(Token::Kind::Symbol, "}"))
            {
                elseBranch->addChild(parseExpression());
            }
            expect(Token::Kind::Symbol, "}");
        }
        return ASTNode::makeIfNode(std::move(cond), std::move(thenBranch), std::move(elseBranch));
    }

    std::unique_ptr<ASTNode> Parser::parseFunctionDeclaration()
    {
        consume();                           // fn
        expect(Token::Kind::Identifier, ""); // function name
        std::string name = current_.text;
        consume();
        expect(Token::Kind::Symbol, "(");
        auto paramsList = ASTNode::makeFunctionParamsListNode();
        while (!match(Token::Kind::Symbol, ")"))
        {
            expect(Token::Kind::Identifier);
            std::string paramName = current_.text;
            consume();
            expect(Token::Kind::Symbol, ":");
            expect(Token::Kind::Identifier);
            std::string typeName = current_.text;
            consume();
            paramsList->addChild(ASTNode::makeFunctionParamNode(paramName, typeName));
            if (!matchConsume(Token::Kind::Symbol, ","))
                break;
        }
        expect(Token::Kind::Symbol, ")");
        expect(Token::Kind::Symbol, "->");
        expect(Token::Kind::Identifier);
        auto returnType = ASTNode::makeFunctionReturnTypeNode(current_.text);
        consume();
        expect(Token::Kind::Symbol, "{");
        auto body = ASTNode::makeProgramNode();
        while (!match(Token::Kind::Symbol, "}"))
        {
            body->addChild(parseExpression());
        }
        expect(Token::Kind::Symbol, "}");
        return ASTNode::makeFunctionDeclarationNode(name, std::move(paramsList), std::move(returnType), std::move(body));
    }

    std::unique_ptr<ASTNode> Parser::parseDebugPrint()
    {
        expect(Token::Kind::Identifier, "pint");
        expect(Token::Kind::Identifier, "");
        std::string varName = current_.text;
        consume();
        return ASTNode::makeDebugPrintIntegerNode(varName);
    }

    std::unique_ptr<ASTNode> Parser::parseStringLiteral()
    {
        expect(Token::Kind::StringLiteral, "");
        auto node = ASTNode::makeStringLiteralNode(current_.text);
        consume();
        return node;
    }

    std::unique_ptr<ASTNode> Parser::parseGroupedExpression()
    {
        consume(); // '('
        auto expr = parseExpression();
        expect(Token::Kind::Symbol, ")");
        return expr;
    }

    int Parser::getTokenPrecedence() const
    {
        if (current_.kind != Token::Kind::Symbol)
            return -1;
        static std::map<std::string, int> precedences = {
            {"+", 10},
            {"-", 10},
            {"*", 20},
            {"/", 20},
        };
        auto it = precedences.find(current_.text);
        if (it != precedences.end())
            return it->second;
        return -1;
    }

} // namespace zlang