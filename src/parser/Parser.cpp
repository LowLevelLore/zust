#include "all.hpp"

namespace zlang
{
    Parser::Parser(Lexer &lex) : lexer(lex)
    {
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
            throw std::runtime_error(
                "Parser error: " + errMsg +
                " at line " + std::to_string(currentToken.line) +
                ", column " + std::to_string(currentToken.column));
        }
    }

    std::unique_ptr<ASTNode> Parser::parse()
    {
        auto program = ASTNode::makeProgramNode();
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

        throw std::runtime_error(
            "Parser error: Unexpected token '" + currentToken.text +
            "' at line " + std::to_string(currentToken.line) +
            ", column " + std::to_string(currentToken.column));
    }

    std::unique_ptr<ASTNode> Parser::parseVariableDeclaration()
    {
        // after 'let', currentToken should be identifier
        if (currentToken.kind != Token::Token::Kind::Identifier)
            expect(Token::Token::Kind::Identifier, "Expected variable name after 'let'");

        std::string name = currentToken.text;
        advance();
        std::unique_ptr<ASTNode> typeNode, initNode;

        // after let name
        if (match(Token::Kind::Colon))
        {
            typeNode = ASTNode::makeSymbolNode(currentToken.text);
            advance();
        }
        if (match(Token::Kind::Equal))
        {
            initNode = parseExpression();
        }
        expect(Token::Kind::SemiColon, "Expected ';' after declaration");
        return ASTNode::makeVariableDeclarationNode(name, std::move(typeNode), std::move(initNode));
    }

    std::unique_ptr<ASTNode> Parser::parseVariableReassignment()
    {
        std::string name = currentToken.text;
        advance(); // consume identifier

        expect(Token::Token::Kind::Equal, "Expected '=' for variable reassignment");
        auto expr = parseExpression();
        expect(Token::Token::Kind::SemiColon, "Expected ';' after reassignment");
        return ASTNode::makeVariableReassignmentNode(name, std::move(expr));
    }

    std::unique_ptr<ASTNode> Parser::parseExpression()
    {
        auto lhs = parseUnary();
        return parseBinaryRHS(0, std::move(lhs));
    }

    std::unique_ptr<ASTNode> Parser::parsePrimary()
    {
        if (currentToken.kind == Token::Token::Kind::StringLiteral)
        {
            std::string s = currentToken.text;
            advance();
            return ASTNode::makeStringLiteralNode(s);
        }
        if (currentToken.kind == Token::Token::Kind::FloatLiteral)
        {
            std::string f = currentToken.text;
            advance();
            return ASTNode::makeFloatLiteralNode(f);
        }
        if (currentToken.kind == Token::Token::Kind::IntegerLiteral)
        {
            std::string val = currentToken.text;
            advance();
            return ASTNode::makeIntegerLiteralNode(val);
        }
        if (currentToken.kind == Token::Token::Kind::Identifier)
        {
            std::string name = currentToken.text;
            advance();
            return ASTNode::makeVariableAccessNode(name);
        }
        throw std::runtime_error(
            "Parser error: Expected expression, got '" + currentToken.text +
            "' at line " + std::to_string(currentToken.line) +
            ", column " + std::to_string(currentToken.column));
    }

    std::unique_ptr<ASTNode> Parser::parseUnary()
    {
        // prefix ++, --, !
        if (currentToken.kind == Token::Kind::Symbol &&
            (currentToken.text == "++" || currentToken.text == "--" || currentToken.text == "!"))
        {
            std::string op = currentToken.text;
            advance();
            auto operand = parseUnary();
            return ASTNode::makeUnaryOp(op, std::move(operand));
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

            lhs = ASTNode::makeBinaryOp(op, std::move(lhs), std::move(rhs));
        }
        return lhs;
    }

    int Parser::getPrecedence(const std::string &op) const
    {
        static std::map<std::string, int> prec = {
            {"||", 1}, {"&&", 2}, {"|", 3}, {"&", 4}, {"+", 5}, {"-", 5}, {"*", 6}, {"/", 6}};
        auto it = prec.find(op);
        return it == prec.end() ? -1 : it->second;
    }

} // namespace zlang
