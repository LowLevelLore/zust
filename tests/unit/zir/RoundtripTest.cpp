#include <doctest/doctest.h>

#include <filesystem>
#include <fstream>
#include <sstream>

#include "zir/Printer.hpp"
#include "zir/TextParser.hpp"

using namespace zust::zir;

namespace {
    std::string readFile(const std::filesystem::path &p) {
        std::ifstream f(p, std::ios::binary);
        REQUIRE_MESSAGE(f.good(), "could not open fixture: " << p);
        std::ostringstream ss;
        ss << f.rdbuf();
        return ss.str();
    }
}  // namespace

// Every fixture here is already in Printer's exact canonical form (that's
// what makes this a fixed-point check, not just a parse-doesn't-crash
// check): parsing it and printing the result back out must reproduce the
// file byte-for-byte. docs/PRD-ZIR.md Wave 1.4's actual exit criterion.
TEST_CASE("print(parse(fixture)) == fixture for every tests/zir/roundtrip/*.zir file") {
    std::filesystem::path dir = std::filesystem::path(ZUST_ZIR_FIXTURES_DIR) / "roundtrip";
    REQUIRE_MESSAGE(std::filesystem::exists(dir), "fixtures directory missing: " << dir);

    int fixturesChecked = 0;
    for (const auto &entry : std::filesystem::directory_iterator(dir)) {
        if (entry.path().extension() != ".zir")
            continue;
        fixturesChecked++;
        CAPTURE(entry.path().string());

        std::string original = readFile(entry.path());
        std::string error;
        std::optional<Module> parsed = TextParser::parse(original, error);
        REQUIRE_MESSAGE(parsed.has_value(), "parse failed: " << error);

        std::string reprinted = Printer::print(*parsed);
        CHECK(reprinted == original);
    }
    CHECK(fixturesChecked > 0);
}
