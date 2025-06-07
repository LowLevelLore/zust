#include "all.hpp"

namespace zlang
{

    static const std::vector<std::string> keywords = {
        "fn", "if", "else", "pint"};

    Lexer::Lexer(const std::string &source)
        : source_(source), pos_(0), line_(1), column_(1) {}

    Token Lexer::nextToken()
    {
        skipWhitespaceAndComments();
        if (pos_ >= source_.size())
        {
            return {Token::Kind::EndOfFile, "", line_, column_};
        }
        char c = peekChar();
        if (std::isalpha(c) || c == '_')
        {
            return scanIdentifierOrKeyword();
        }
        if (std::isdigit(c))
        {
            return scanNumber();
        }
        if (c == '"')
        {
            return scanString();
        }
        return scanSymbol();
    }

    Token Lexer::peek(size_t offset) const
    {
        Lexer copy = *this;
        for (size_t i = 0; i < offset; ++i)
            copy.nextToken();
        return copy.nextToken();
    }

    void Lexer::reset()
    {
        pos_ = 0;
        line_ = 1;
        column_ = 1;
    }

    char Lexer::advance()
    {
        char c = source_[pos_++];
        if (c == '\n')
        {
            ++line_;
            column_ = 1;
        }
        else
        {
            ++column_;
        }
        return c;
    }

    char Lexer::peekChar(size_t offset) const
    {
        if (pos_ + offset >= source_.size())
            return '\0';
        return source_[pos_ + offset];
    }

    void Lexer::skipWhitespaceAndComments()
    {
        while (true)
        {
            char c = peekChar();
            // whitespace
            if (std::isspace(c))
            {
                advance();
                continue;
            }
            // line comment
            if (c == '/' && peekChar(1) == '/')
            {
                while (c && c != '\n')
                    c = advance();
                continue;
            }
            // block comment
            if (c == '/' && peekChar(1) == ' ')
            {
                advance();
                advance();
                while (!(peekChar() == ' ' && peekChar(1) == '/'))
                {
                    if (peekChar() == '\0')
                        break;
                    advance();
                }
                if (peekChar() == '*')
                {
                    advance();
                    if (peekChar() == '/')
                        advance();
                }
                continue;
            }
            break;
        }
    }

    Token Lexer::scanIdentifierOrKeyword()
    {
        size_t startLine = line_, startCol = column_;
        std::string text;
        while (std::isalnum(peekChar()) || peekChar() == '_')
        {
            text.push_back(advance());
        }
        // check keywords
        for (const auto &kw : keywords)
        {
            if (text == kw)
                return {Token::Kind::Keyword, text, startLine, startCol};
        }
        return {Token::Kind::Identifier, text, startLine, startCol};
    }

    Token Lexer::scanNumber()
    {
        size_t startLine = line_, startCol = column_;
        std::string text;
        bool seenDot = false;
        while (std::isdigit(peekChar()) || (!seenDot && peekChar() == '.'))
        {
            if (peekChar() == '.')
                seenDot = true;
            text.push_back(advance());
        }
        return {seenDot ? Token::Kind::FloatLiteral : Token::Kind::IntegerLiteral,
                text, startLine, startCol};
    }

    Token Lexer::scanString()
    {
        size_t startLine = line_, startCol = column_;
        advance(); // consume '"'
        std::string text;
        while (peekChar() != '"' && peekChar() != '\0')
        {
            text.push_back(advance());
        }

        if (peekChar() != '"')
        {
            zlang::logError(zlang::Error(ErrorType::Syntax, "No closing bracket for string."));
            exit(0);
        }

        advance(); // consume '"'
        return {Token::Kind::StringLiteral, text, startLine, startCol};
    }

    Token Lexer::scanSymbol()
    {
        size_t startLine = line_, startCol = column_;
        char c = advance();
        std::string text(1, c);
        // multi-char symbols
        char next = peekChar();
        if ((c == ':' && next == '=') || (c == '-' && next == '>') ||
            ((c == '<' || c == '>' || c == '=' || c == '!') && next == '='))
        {
            text.push_back(advance());
        }
        return {Token::Kind::Symbol, text, startLine, startCol};
    }

} // namespace zlang