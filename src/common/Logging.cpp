#include "common/Logging.hpp"
#include "common/Colors.hpp"
#include <iostream>
#include <system_error>

namespace zlang {

void logSystemError(const std::string &message) {
    std::cerr << colors::RED << "[System Error] " << colors::RESET << message
              << ": " << std::generic_category().message(errno) << std::endl;
}

void logError(const Error &err) {
    if (err) {
        std::cerr << colors::RED << "[Error] " << colors::RESET << err.message
                  << std::endl;
    }
}

void logMessage(const std::string &message) {
    std::cout << colors::GREEN << "[Info] " << colors::RESET << message
              << std::endl;
}

} // namespace zlang