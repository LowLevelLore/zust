#ifndef ZLANG_COMMON_LOGGING_HPP
#define ZLANG_COMMON_LOGGING_HPP

#include <string>
#include "Errors.hpp"

namespace zlang
{

    void logSystemError(const std::string &message);
    void logError(const Error &err);
    void logMessage(const std::string &message);

} // namespace zlang

#endif // ZLANG_COMMON_LOGGING_HPP