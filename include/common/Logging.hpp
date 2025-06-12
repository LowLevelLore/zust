#pragma once

#include <string>
#include "Errors.hpp"

namespace zlang
{

    void logSystemError(const std::string &message);
    void logError(const Error &err);
    void logMessage(const std::string &message);

} // namespace zlang