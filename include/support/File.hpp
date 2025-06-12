#pragma once

#include <string>
#include <optional>

namespace zlang
{

    class File
    {
    public:
        // Read the entire contents of a file into a string
        // Returns std::nullopt if file couldn't be opened or read
        static std::optional<std::string> readAllText(const std::string &filepath);
    };

} // namespace zlang
