#pragma once
#include <memory>

#include "../ast/ASTNode.hpp"
#include "../lexer/Lexer.hpp"
#include "common/Errors.hpp"
#include "common/Logging.hpp"
#include "parser/ScopeContext.hpp"

namespace zust {
    class Parser {
    public:
        explicit Parser(Lexer &lexer);
        std::unique_ptr<ASTNode> parse();
        bool isCorrect() {
            return shouldTypecheck;
        }

    private:
        int blockNumber = 0;
        Lexer &lexer;
        Token currentToken;
        std::shared_ptr<ScopeContext> currentScope;
        bool shouldTypecheck = true;

        void advance();
        bool match(Token::Kind kind);
        void expect(Token::Kind kind, const std::string &errMsg);

        std::unique_ptr<ASTNode> parseStatement();
        std::unique_ptr<ASTNode> parseVariableDeclaration();
        std::unique_ptr<ASTNode> parseVariableReassignment();
        std::unique_ptr<ASTNode> parseConditionals();
        std::unique_ptr<ASTNode> parseExpression(bool expect_exclaim = false);
        std::unique_ptr<ASTNode> parsePrimary();
        std::unique_ptr<ASTNode> parseFunctionDeclaration();

        int getPrecedence(const std::string &op) const;
        std::unique_ptr<ASTNode> parseUnary();
        std::unique_ptr<ASTNode> parseBinaryRHS(int exprPrec, std::unique_ptr<ASTNode> lhs);

        void enterScope(const std::string &name, bool isFunction);
        void exitScope();
        std::unique_ptr<ASTNode> parseBlock();
    };
}
