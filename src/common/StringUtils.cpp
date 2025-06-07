#include "common/StringUtils.hpp"
#include <algorithm>
#include <sstream>

namespace zlang
{

    bool startsWith(const std::string &str, const std::string &prefix)
    {
        return str.size() >= prefix.size() &&
               str.compare(0, prefix.size(), prefix) == 0;
    }

    bool endsWith(const std::string &str, const std::string &suffix)
    {
        return str.size() >= suffix.size() &&
               str.compare(str.size() - suffix.size(), suffix.size(), suffix) == 0;
    }

    std::vector<std::string> split(const std::string &str, char delimiter)
    {
        std::vector<std::string> parts;
        std::stringstream ss(str);
        std::string item;

        while (std::getline(ss, item, delimiter))
        {
            parts.push_back(item);
        }

        return parts;
    }

    std::string trim(const std::string &str)
    {
        const auto strBegin = std::find_if_not(str.begin(), str.end(), ::isspace);
        const auto strEnd = std::find_if_not(str.rbegin(), str.rend(), ::isspace).base();

        if (strBegin >= strEnd)
            return "";

        return std::string(strBegin, strEnd);
    }

} // namespace zlang
