#pragma once

#include <string>

class NameMapper
    {
    public:
        inline std::string mapVariable(const std::string &name, const std::string &scopeName)
        {
            std::string mangled = scopeName + "___" + name + "___v" + std::to_string(varCounter_++);
            return mangled;
        }

        inline std::string mapFunction(const std::string &name, const std::string &scopeName)
        {
            std::string mangled = scopeName + "___" + name + "___f" + std::to_string(funcCounter_++);
            return mangled;
        }

        inline std::string mapType(const std::string &name, const std::string &scopeName)
        {
            std::string mangled = scopeName + "___" + name + "___t" + std::to_string(typeCounter_++);
            return mangled;
        }

    private:
        size_t varCounter_ = 0;
        size_t funcCounter_ = 0;
        size_t typeCounter_ = 0;
    };
