#ifndef ZLANG_LEXER_LEXER_HPP
#define ZLANG_LEXER_LEXER_HPP

#include <string>
#include <vector>
#include "common/Errors.hpp"

namespace zlang
{

    /// Token kinds in zlang
    struct Token
    {
        enum class Kind
        {
            Identifier,
            IntegerLiteral,
            FloatLiteral,
            StringLiteral,
            Symbol,
            Keyword,
            EndOfFile,
            Unknown
        } kind;

        std::string text;
        size_t line;
        size_t column;

        std::string toString() const
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
            case Kind::Unknown:
                return "Unknown";
            }
            return "Invalid";
        }
    };

    // Optional: makes it easy to stream Token to std::cout
    inline std::ostream &operator<<(std::ostream &os, const Token &token)
    {
        return os << token.toString();
    }

    using Error = zlang::Error;

    /// Lexer: tokenize source code
    class Lexer
    {
    public:
        explicit Lexer(const std::string &source);

        /// Get next token
        Token nextToken();
        /// Peek ahead
        Token peek(size_t offset = 0) const;
        /// Reset to beginning
        void reset();

    private:
        std::string source_;
        size_t pos_;
        size_t line_;
        size_t column_;

        char advance();
        char peekChar(size_t offset = 0) const;
        void skipWhitespaceAndComments();
        Token scanIdentifierOrKeyword();
        Token scanNumber();
        Token scanString();
        Token scanSymbol();
    };

} // namespace zlang

#endif // ZLANG_LEXER_LEXER_HPP