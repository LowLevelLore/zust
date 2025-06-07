#ifndef ZLANG_COMMON_STRINGUTILS_HPP
#define ZLANG_COMMON_STRINGUTILS_HPP

#include <string>
#include <vector>

namespace zlang
{

    bool startsWith(const std::string &str, const std::string &prefix);
    bool endsWith(const std::string &str, const std::string &suffix);
    std::vector<std::string> split(const std::string &str, char delimiter);
    std::string trim(const std::string &str);

    inline bool isWhitespace(char c)
    {
        return c == ' ' || c == '\t' || c == '\n' || c == '\r';
    }

    inline bool isAlpha(char c)
    {
        return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z');
    }

    inline bool isDigit(char c)
    {
        return c >= '0' && c <= '9';
    }

    inline bool isSymbolChar(char c)
    {
        return isAlpha(c) || isDigit(c) || c == '_';
    }

} // namespace zlang

#endif // ZLANG_COMMON_STRINGUTILS_HPP
