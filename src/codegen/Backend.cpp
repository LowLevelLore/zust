#include "codegen/Backend.hpp"

#include <algorithm>
#include <iomanip>
#include <sstream>

namespace zust {

    namespace {
        // Escapes a string for embedding in a JSON string literal (quotes,
        // backslashes, and control characters) so a backend whose
        // description or assemble/link command happens to contain one of
        // those doesn't produce invalid JSON for --formats --json.
        std::string jsonEscape(std::string_view s) {
            std::ostringstream out;
            for (unsigned char c : s) {
                switch (c) {
                    case '"':
                        out << "\\\"";
                        break;
                    case '\\':
                        out << "\\\\";
                        break;
                    case '\b':
                        out << "\\b";
                        break;
                    case '\f':
                        out << "\\f";
                        break;
                    case '\n':
                        out << "\\n";
                        break;
                    case '\r':
                        out << "\\r";
                        break;
                    case '\t':
                        out << "\\t";
                        break;
                    default:
                        if (c < 0x20) {
                            out << "\\u" << std::hex << std::setw(4) << std::setfill('0') << static_cast<int>(c)
                                << std::dec;
                        } else {
                            out << static_cast<char>(c);
                        }
                }
            }
            return out.str();
        }
    }  // namespace

    BackendRegistry &BackendRegistry::instance() {
        static BackendRegistry registry;
        return registry;
    }

    void BackendRegistry::registerBackend(TargetInfo info, Factory factory) {
        infos_.push_back(std::move(info));
        factories_.push_back(std::move(factory));
    }

    std::unique_ptr<Backend> BackendRegistry::create(std::string_view name) const {
        for (std::size_t i = 0; i < infos_.size(); ++i) {
            if (infos_[i].name == name) {
                return factories_[i]();
            }
        }
        return nullptr;
    }

    const TargetInfo *BackendRegistry::find(std::string_view name) const {
        for (const auto &info : infos_) {
            if (info.name == name)
                return &info;
        }
        return nullptr;
    }

    std::vector<const TargetInfo *> BackendRegistry::list() const {
        std::vector<const TargetInfo *> result;
        result.reserve(infos_.size());
        for (const auto &info : infos_) {
            result.push_back(&info);
        }
        return result;
    }

    std::string BackendRegistry::hostDefaultName() {
#if defined(_WIN64)
        return "x86_64-mswin";
#elif defined(__linux__)
        return "x86_64-linux";
#else
        // Matches the previous main.cpp behavior: on a platform that is
        // neither, the Linux backend is what falls out.
        return "x86_64-linux";
#endif
    }

    void BackendRegistry::printFormats(std::ostream &out) const {
        out << "Acceptable formats include:\n"
            << " -> default (resolves to '" << hostDefaultName() << "' on this host)\n";
        for (const auto &info : infos_) {
            out << " -> " << info.name << "    " << info.description << "\n";
        }
    }

    void BackendRegistry::printFormatsJson(std::ostream &out) const {
        out << "{\n  \"default\": \"" << jsonEscape(hostDefaultName()) << "\",\n  \"targets\": [\n";
        for (std::size_t i = 0; i < infos_.size(); ++i) {
            const TargetInfo &info = infos_[i];
            out << "    {\n"
                << "      \"name\": \"" << jsonEscape(info.name) << "\",\n"
                << "      \"description\": \"" << jsonEscape(info.description) << "\",\n"
                << "      \"asmExt\": \"" << jsonEscape(info.asmExt) << "\",\n"
                << "      \"isNative\": " << (info.isNative ? "true" : "false") << ",\n"
                << "      \"assembleCmd\": [";
            for (std::size_t j = 0; j < info.assembleCmd.size(); ++j) {
                out << "\"" << jsonEscape(info.assembleCmd[j]) << "\"" << (j + 1 < info.assembleCmd.size() ? ", " : "");
            }
            out << "],\n      \"linkCmd\": [";
            for (std::size_t j = 0; j < info.linkCmd.size(); ++j) {
                out << "\"" << jsonEscape(info.linkCmd[j]) << "\"" << (j + 1 < info.linkCmd.size() ? ", " : "");
            }
            out << "]\n    }" << (i + 1 < infos_.size() ? "," : "") << "\n";
        }
        out << "  ]\n}\n";
    }

}  // namespace zust
