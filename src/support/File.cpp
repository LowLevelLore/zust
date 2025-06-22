#include "support/File.hpp"
#include <fstream>
#include <sstream>

namespace zust
{

    std::optional<std::string> File::readAllText(const std::string &filepath)
    {
        std::ifstream file(filepath, std::ios::binary);
        if (!file.is_open())
        {
            return std::nullopt;
        }

        std::ostringstream contents;
        contents << file.rdbuf();

        if (file.fail() && !file.eof())
        {
            // Error reading file
            return std::nullopt;
        }

        return contents.str();
    }

} // namespace zust
