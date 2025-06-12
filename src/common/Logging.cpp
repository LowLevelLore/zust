#include "common/Logging.hpp"
#include "common/Colors.hpp"
#include <iostream>
#include <system_error>

namespace zlang
{

    void logSystemError(const std::string &message)
    {
        std::cerr << colors::RED << "[System Error] " << colors::RESET << message
                  << ": " << std::generic_category().message(errno) << std::endl;
    }

    void logError(const Error &err)
    {
        if (err)
        {
            switch (err.type)
            {
            case ErrorType::None:
                return;
            case ErrorType::Arguments:
                std::cerr << colors::RED << "[Arguments Error] ";
                break;
            case ErrorType::Generic:
                std::cerr << colors::RED << "[Generic Error] ";
                break;
            case ErrorType::Syntax:
                std::cerr << colors::RED << "[Syntax Error] ";
                break;
            case ErrorType::Type:
                std::cerr << colors::RED << "[Type Error] ";
                break;
            case ErrorType::Todo:
                std::cerr << colors::RED << "[TODO] ";
                break;
            default:
                return;
            }
            std::cerr << colors::YELLOW << err.message << colors::RESET
                      << std::endl;
        }
    }

    void logMessage(const std::string &message)
    {
        std::cout << colors::GREEN << "[Info] " << colors::RESET << message
                  << std::endl;
    }

} // namespace zlang