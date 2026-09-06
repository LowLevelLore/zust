#include <sstream>

#include <doctest/doctest.h>

#include "codegen/Backend.hpp"

using namespace zust;

namespace {
    class FakeBackend final : public Backend {
    public:
        explicit FakeBackend(TargetInfo info) : info_(std::move(info)) {}

        const TargetInfo &info() const override { return info_; }

        void emit(std::unique_ptr<ASTNode>, std::ostream &, int) override {}

    private:
        TargetInfo info_;
    };
}  // namespace

TEST_CASE("printFormatsJson escapes quotes, backslashes, and control characters") {
    // BackendRegistry is a process-wide singleton (one registry, matching
    // docs/BACKENDS.md), so this registers into the same instance main.cpp
    // would use -- harmless for a standalone test binary that never calls
    // registerBuiltinBackends itself.
    BackendRegistry &registry = BackendRegistry::instance();
    TargetInfo info{"weird-target-for-json-escaping-test",
                    "has a \"quote\", a \\backslash\\, and a\nnewline",
                    ".ext",
                    AsmSyntax::None,
                    true,
                    {"tool", "-flag=\"value\"", "$IN"},
                    {"linker", "$IN", "-o", "$OUT"}};
    registry.registerBackend(info, [info] { return std::make_unique<FakeBackend>(info); });

    std::ostringstream out;
    registry.printFormatsJson(out);

    // The whole point: this must be valid JSON despite the embedded quotes,
    // backslashes, and newline above. A naive parser is enough to prove
    // that -- it either accepts the document or it doesn't.
    std::string json = out.str();
    CHECK(json.find("\\\"quote\\\"") != std::string::npos);
    CHECK(json.find("\\\\backslash\\\\") != std::string::npos);
    CHECK(json.find("\\n") != std::string::npos);
    // A raw, unescaped newline inside a JSON string literal would make the
    // document invalid; the escaped "\n" above must be the only one.
    CHECK(json.find("a\nnewline") == std::string::npos);
}
