#pragma once

#include <string>

#include "Errors.hpp"

namespace zust {

    void logSystemError(const std::string &message);
    void logError(const Error &err);
    void logMessage(const std::string &message);
    // Non-fatal: never sets a phase's "should I still generate code" flag.
    // Used by e.g. definite-return analysis (docs/PRD-ZIR.md Wave 2.4), which
    // is deliberately advisory-only in this rewrite -- turning it into an
    // error would change compile_fail's frozen stderr-substring surface.
    void logWarning(const std::string &message);

}  // namespace zust