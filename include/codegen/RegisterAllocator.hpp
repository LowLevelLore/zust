#pragma once

#include <list>
#include <optional>
#include <stdexcept>
#include <string>
#include <unordered_set>
#include <vector>

namespace zlang {

    // Tips for noobs: CALLER saved are saved by the caller, CALLEE saved are restored by the function/routine
    // So CALLER Union CALLEE should be equal to the set of all the registers, so the current state will never be corrupted.

    // Time wasted here: 3days and still counting.. And I am still unemployed.

    // GPRs on LINUX (SysV ABI)
    static const std::vector<std::string> SCRATCH_GPR_LINUX = {"rax", "rcx", "rdx", "rsi", "rdi", "r8", "r9", "r10", "r11"};
    static const std::vector<std::string> ARG_GPR_LINUX = {"rdi", "rsi", "rdx", "rcx", "r8", "r9"};
    static const std::vector<std::string> CALLER_GPR_LINUX = {"rax", "rcx", "rdx", "rsi", "rdi", "r8", "r9", "r10", "r11"};
    static const std::vector<std::string> CALLEE_GPR_LINUX = {"rbx", "r12", "r13", "r14", "r15"};
    // XMMs on LINUX (SysV ABI)
    static const std::vector<std::string> SCRATCH_XMM_LINUX = {"xmm0", "xmm1", "xmm2", "xmm3", "xmm4", "xmm5", "xmm6", "xmm7",
                                                               "xmm8", "xmm9", "xmm10", "xmm11", "xmm12", "xmm13", "xmm14", "xmm15"};
    static const std::vector<std::string> ARG_XMM_LINUX = {"xmm0", "xmm1", "xmm2", "xmm3", "xmm4", "xmm5", "xmm6", "xmm7"};
    static const std::vector<std::string> CALLER_XMM_LINUX = {"xmm0", "xmm1", "xmm2", "xmm3", "xmm4", "xmm5", "xmm6", "xmm7",
                                                              "xmm8", "xmm9", "xmm10", "xmm11", "xmm12", "xmm13", "xmm14", "xmm15"};  // Will be used freely by function
    static const std::vector<std::string> CALLEE_XMM_LINUX = {/* None */};                                                            // Why would you do this linus ?                                                                                                                         // The function is responsible to restore the state
    // GPRs on WINDOWS (MSVC)
    static const std::vector<std::string> SCRATCH_GPR_MSVC = {"rax", "rcx", "rdx", "r8", "r9", "r10", "r11"};
    static const std::vector<std::string> ARG_GPR_MSVC = {"rcx", "rdx", "r8", "r9"};
    static const std::vector<std::string> CALLER_GPR_MSVC = {"rax", "rcx", "rdx", "r8", "r9", "r10", "r11"};
    static const std::vector<std::string> CALLEE_GPR_MSVC = {"rbx", "rdi", "rsi", "r12", "r13", "r14", "r15"};
    // XMMs on WINDOWS (MSVC)
    static const std::vector<std::string> SCRATCH_XMM_MSVC = {"xmm0", "xmm1", "xmm2", "xmm3", "xmm4", "xmm5"};
    static const std::vector<std::string> ARG_XMM_MSVC = {"xmm0", "xmm1", "xmm2", "xmm3"};
    static const std::vector<std::string> CALLER_XMM_MSVC = {"xmm0", "xmm1", "xmm2", "xmm3", "xmm4", "xmm5"};
    static const std::vector<std::string> CALLEE_XMM_MSVC = {"xmm6", "xmm7", "xmm8", "xmm9", "xmm10", "xmm11", "xmm12", "xmm13", "xmm14", "xmm15"};

    class RegisterAllocator {
        struct SpillInfo {
            std::string spillSlot;  // e.g. -16(%rbp) or [rbp - 16]
            bool isXMM;
        };

        std::unordered_map<std::string, SpillInfo> spilledRegs;
        std::unordered_map<std::string, SpillInfo> spilledArgumentRegs;

    public:
        /// Initialize allocator with target-specific registers
        static RegisterAllocator forSysV();  // Linux ABI
        static RegisterAllocator forMSVC();  // Windows ABI

        std::string allocate();
        bool isInUse(std::string reg);
        bool isInUseXMM(std::string reg);

        void free(const std::string &reg);

        std::string allocateXMM();

        bool freeXMM(const std::string &reg);

        std::string allocateArgument(uint8_t position);
        std::string allocateArgumentXMM(uint8_t position);
        void freeArgument(const std::string &reg);
        bool freeArgumentXMM(const std::string &reg);
        bool isInUseArgument(const std::string &reg) const;
        bool isInUseArgumentXMM(const std::string &reg) const;

        void reset();
        RegisterAllocator() = default;
        static std::string getBaseReg(const std::string &reg);

        std::string pickVictimXMM();
        std::string pickVictim();
        void markSpilledXMM(const std::string &reg, const std::string &spillSlot);
        void markSpilled(const std::string &reg, const std::string &spillSlot);
        bool isSpilled(const std::string &reg) const;
        std::string spillSlotFor(const std::string &reg) const;
        void unSpillXMM(const std::string &reg, zlang::CodegenOutputFormat format, std::ostream &out);
        void unSpill(const std::string &reg, zlang::CodegenOutputFormat format, std::ostream &out);
        void touch(const std::string &reg);
        void touchXMM(const std::string &reg);
        void emitSpillRestore(const std::string &reg, const std::string &slot, bool isXMM, zlang::CodegenOutputFormat format, std::ostream &out);

    private:
        RegisterAllocator(std::vector<std::string> regs, std::vector<std::string> XMMregs, std::vector<std::string> argumentRegs, std::vector<std::string> argumentXMMRegs);
        std::vector<std::string> available;
        std::unordered_set<std::string> inUse;

        std::vector<std::string> availableXMM;
        std::unordered_set<std::string> inUseXMM;

        std::unordered_set<std::string> inUseArgumentXMM;
        std::unordered_set<std::string> inUseArgument;

        std::list<std::string> lruRegs;
        std::list<std::string> lruXMMRegs;

    public:
        const std::vector<std::string> availableArgumentRegs;
        const std::vector<std::string> availableArgumentRegsXMM;
    };
}
