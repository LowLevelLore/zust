#pragma once
#include <string>
#include <vector>
#include "../common/Errors.hpp"

namespace zlang
{
    struct Token
    {
        enum class Kind
        {
            Let,
            Identifier,
            Colon,
            IntegerLiteral,
            FloatLiteral,
            StringLiteral,
            BoolLiteral,
            Symbol,
            Keyword,
            EndOfFile,
            SemiColon,
            Equal,
            If,
            ElseIf,
            Else,
            LeftBrace,
            RightBrace,
            LeftParen,
            RightParen,
            Unknown
        } kind;

        std::string text;
        size_t line;
        size_t column;

        std::string to_string() const
        {
            return "Token { kind = " + kindToString(kind) +
                   ", text = \"" + text +
                   "\", line = " + std::to_string(line) +
                   ", column = " + std::to_string(column) + " }";
        }

    private:
        static std::string kindToString(Kind k)
        {
            switch (k)
            {
            case Kind::Equal:
                return "Equal";
            case Kind::Colon:
                return "Colon";
            case Kind::Let:
                return "Let";
            case Kind::Identifier:
                return "Identifier";
            case Kind::IntegerLiteral:
                return "IntegerLiteral";
            case Kind::FloatLiteral:
                return "FloatLiteral";
            case Kind::StringLiteral:
                return "StringLiteral";
            case Kind::Symbol:
                return "Symbol";
            case Kind::Keyword:
                return "Keyword";
            case Kind::EndOfFile:
                return "EndOfFile";
            case Kind::SemiColon:
                return "SemiColon";
            case Kind::Unknown:
                return "Unknown";
            case Kind::BoolLiteral:
                return "Boolean";
            case Kind::If:
                return "If";
            case Kind::ElseIf:
                return "ElseIf";
            case Kind::Else:
                return "Else";
            case Kind::LeftBrace:
                return "LeftBrace";
            case Kind::RightBrace:
                return "RightBrace";
            case Kind::RightParen:
                return "RightParen";
            case Kind::LeftParen:
                return "LeftParen";
            }
            return "Invalid";
        }
    };

    inline std::ostream &operator<<(std::ostream &os, const Token &token)
    {
        return os << token.to_string();
    }
    using Error = zlang::Error;
    class Lexer
    {
    public:
        explicit Lexer(const std::string &source);

        Token nextToken();
        Token peek(size_t offset = 0) const;
        void reset();

    private:
        std::string source_;
        size_t pos_;
        size_t line_;
        size_t column_;

        char advance();
        char peekChar(size_t offset = 0) const;
        void skipWhitespaceAndComments();
        Token scanIdentifierOrKeywordOrConditional();
        Token scanNumber();
        Token scanString();
        Token scanSymbol();
    };

} // namespace zlang
