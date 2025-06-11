#ifndef ZLANG_PARSER_HPP
#define ZLANG_PARSER_HPP

#include <memory>
#include "../lexer/Lexer.hpp"
#include "../ast/ASTNode.hpp"

namespace zlang
{
    class Parser
    {
    public:
        explicit Parser(Lexer &lexer);
        std::unique_ptr<ASTNode> parse();

    private:
        Lexer &lexer;
        Token currentToken;

        void advance();
        bool match(Token::Kind kind);
        void expect(Token::Kind kind, const std::string &errMsg);

        std::unique_ptr<ASTNode> parseStatement();
        std::unique_ptr<ASTNode> parseVariableDeclaration();
        std::unique_ptr<ASTNode> parseVariableReassignment();
        std::unique_ptr<ASTNode> parseExpression();
        std::unique_ptr<ASTNode> parsePrimary();

        int getPrecedence(const std::string &op) const;
        std::unique_ptr<ASTNode> parseUnary();
        std::unique_ptr<ASTNode> parseBinaryRHS(int exprPrec, std::unique_ptr<ASTNode> lhs);
    };
}

#endif // ZLANG_PARSER_HPP
