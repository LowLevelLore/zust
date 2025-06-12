#include "all.hpp"

namespace zlang
{

    Lexer::Lexer(const std::string &source)
        : source_(source), pos_(0), line_(1), column_(1)
    {
    }

    void Lexer::reset()
    {
        pos_ = 0;
        line_ = 1;
        column_ = 1;
    }

    char Lexer::advance()
    {
        if (pos_ >= source_.size())
            return '\0';
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
            if (std::isspace(static_cast<unsigned char>(c)))
            {
                advance();
                continue;
            }
            if (c == '/' && peekChar(1) == '/')
            {
                // line comment
                advance();
                advance();
                while (peekChar() != '\n' && peekChar() != '\0')
                    advance();
                continue;
            }
            break;
        }
    }

    Token Lexer::nextToken()
    {
        skipWhitespaceAndComments();
        size_t tokLine = line_;
        size_t tokCol = column_;

        char c = peekChar();
        if (c == '\0')
        {
            return Token{Token::Kind::EndOfFile, "", tokLine, tokCol};
        }

        if (std::isalpha(static_cast<unsigned char>(c)) || c == '_')
            return scanIdentifierOrKeywordOrConditional();

        if (std::isdigit(static_cast<unsigned char>(c)) || (c == '-' && std::isdigit((unsigned char)peekChar(1))))
            return scanNumber();

        if (c == '"')
            return scanString();

        return scanSymbol();
    }

    Token Lexer::peek(size_t offset) const
    {
        Lexer copy = *this;
        Token tok;
        for (size_t i = 0; i <= offset; ++i)
            tok = copy.nextToken();
        return tok;
    }

    Token Lexer::scanIdentifierOrKeywordOrConditional()
    {
        size_t startLine = line_;
        size_t startCol = column_;
        std::string text;
        while (std::isalnum(static_cast<unsigned char>(peekChar())) || peekChar() == '_')
            text.push_back(advance());

        if (text == "let")
            return Token{Token::Kind::Let, text, startLine, startCol};

        if (text == "true" || text == "false")
            return {Token::Kind::BoolLiteral, text, startLine, startCol};

        if (text == "if")
            return {Token::Kind::If, text, startLine, startCol};

        if (text == "elif")
            return {Token::Kind::ElseIf, text, startLine, startCol};

        if (text == "else")
            return {Token::Kind::Else, text, startLine, startCol};

        return Token{Token::Kind::Identifier, text, startLine, startCol};
    }

    Token Lexer::scanNumber()
    {
        size_t startLine = line_;
        size_t startCol = column_;
        std::string text;
        bool seenDot = false;
        if (peekChar() == '-' and isdigit(peekChar(1)))
        {
            text.push_back(advance());
        }
        while (std::isdigit(static_cast<unsigned char>(peekChar())) || (!seenDot && peekChar() == '.'))
        {
            if (peekChar() == '.')
                seenDot = true;
            text.push_back(advance());
        }
        if ((peekChar() == 'f' || peekChar() == 'F') && seenDot)
        {
            text.push_back(advance());
        }
        return Token{
            seenDot ? Token::Kind::FloatLiteral : Token::Kind::IntegerLiteral,
            text, startLine, startCol};
    }

    Token Lexer::scanString()
    {
        size_t startLine = line_;
        size_t startCol = column_;
        advance(); // consume '"'
        std::string text;
        while (peekChar() != '"' && peekChar() != '\0')
            text.push_back(advance());
        if (peekChar() == '"')
            advance(); // consume closing '"'
        else
            logError(Error(ErrorType::Syntax, "Unterminated string literal."));
        return Token{Token::Kind::StringLiteral, text, startLine, startCol};
    }

    Token Lexer::scanSymbol()
    {
        size_t startLine = line_;
        size_t startCol = column_;
        char c = advance();
        std::string text(1, c);
        char next = peekChar();
        // Multi-char operators
        if ((c == '&' && next == '&') ||
            (c == '|' && next == '|') ||
            (c == '+' && next == '+') ||
            (c == '-' && next == '-') ||
            (c == '>' && next == '=') ||
            (c == '<' && next == '=') ||
            (c == '!' && next == '=') ||
            (c == '=' && next == '='))
        {
            text.push_back(advance());
        }

        switch (c)
        {
        case ':':
            return Token{Token::Kind::Colon, text, startLine, startCol};
        case '=':
            return Token{Token::Kind::Equal, text, startLine, startCol};
        case ';':
            return Token{Token::Kind::SemiColon, text, startLine, startCol};
        case '{':
            return Token{Token::Kind::LeftBrace, text, startLine, startCol};
        case '}':
            return Token{Token::Kind::RightBrace, text, startLine, startCol};
        case '(':
            return Token{Token::Kind::LeftParen, text, startLine, startCol};
        case ')':
            return Token{Token::Kind::RightParen, text, startLine, startCol};
        case '+':
        case '-':
        case '*':
        case '/':
        case '|':
        case '&':
        case '!':
        case '>':
        case '<':
            return Token{Token::Kind::Symbol, text, startLine, startCol};
        default:
            return Token{Token::Kind::Symbol, text, startLine, startCol};
        }
    }

} // namespace zlang