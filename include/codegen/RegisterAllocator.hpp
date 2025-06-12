#pragma once

#include <string>
#include <vector>
#include <unordered_set>
#include <stdexcept>

namespace zlang
{
    /// Simple register allocator for x86_64
    class RegisterAllocator
    {
    public:
        /// Initialize allocator with target-specific registers
        static RegisterAllocator forSysV(); // Linux ABI
        static RegisterAllocator forMSVC(); // Windows ABI

        /// Allocate a free register; throws if none available
        std::string allocate();

        /// Free a previously allocated register; throws if invalid
        void free(const std::string &reg);

        /// Allocate a free XMM register; throws if none available
        std::string allocateXMM();

        /// Free a previously allocated XMM register; throws if invalid
        void freeXMM(const std::string &reg);

        /// Reset allocator state (mark all as free)
        void reset();

    private:
        RegisterAllocator(std::vector<std::string> regs);

        std::vector<std::string> available;
        std::unordered_set<std::string> inUse;

        std::vector<std::string> availableXMM = { // Default XMMs
            "xmm0", "xmm1", "xmm2", "xmm3", "xmm4", "xmm5", "xmm6", "xmm7",
            "xmm8", "xmm9", "xmm10", "xmm11", "xmm12", "xmm13", "xmm14", "xmm15"};
        std::unordered_set<std::string> inUseXMM;
    };
}
