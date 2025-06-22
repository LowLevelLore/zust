#pragma once

#include <string>
#include "Errors.hpp"

namespace zust
{

    void logSystemError(const std::string &message);
    void logError(const Error &err);
    void logMessage(const std::string &message);

} // namespace zust