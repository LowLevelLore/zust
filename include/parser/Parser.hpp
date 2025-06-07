#pragma once

#include <memory>
#include <string>
#include "lexer/Lexer.hpp"

namespace zlang
{

    class Parser
    {
    public:
        Parser(Lexer lexer) : lexer_(std::move(lexer)), current_(lexer_.nextToken()) {}
        Error parseProgram(std::unique_ptr<ASTNode> &out);

    private:
        std::unique_ptr<ASTNode> parseExpression(int prec = 0);
        std::unique_ptr<ASTNode> parsePrimary();
        std::unique_ptr<ASTNode> parseVariableDeclaration();
        std::unique_ptr<ASTNode> parseReassignment();
        std::unique_ptr<ASTNode> parseGroupedExpression();
        std::unique_ptr<ASTNode> parseIdentifierOrCall();
        std::unique_ptr<ASTNode> parseFunctionDeclaration();
        std::unique_ptr<ASTNode> parseIf();
        std::unique_ptr<ASTNode> parseDebugPrint();
        std::unique_ptr<ASTNode> parseStringLiteral();

        int getTokenPrecedence() const;

        bool match(Token::Kind kind, const std::string &text = "") const;
        Error expect(Token::Kind kind, const std::string &text = "");
        bool matchConsume(Token::Kind kind, const std::string &text = "");
        void consume();

        Lexer lexer_;
        Token current_;
    };

} // namespace zlang
