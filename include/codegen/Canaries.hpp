#include <cstdint>
#include <random>

class CanaryGenerator {
public:
    static std::uint64_t generate() {
        std::random_device rd;  // Cryptographically secure random source
        std::mt19937_64 gen(rd());
        std::uniform_int_distribution<std::uint64_t> dis;
        return dis(gen);
    }
};