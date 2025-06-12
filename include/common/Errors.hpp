#pragma once
#include <string>

namespace zlang
{
    enum class ErrorType
    {
        None,
        Generic,
        Syntax,
        Type,
        Arguments,
        Todo
    };

    struct Error
    {
        ErrorType type;
        std::string message;

        Error() : type(ErrorType::None), message("") {}
        Error(ErrorType t, const std::string &msg) : type(t), message(msg) {}

        operator bool() const { return type != ErrorType::None; }
    };

} // namespace zlang
