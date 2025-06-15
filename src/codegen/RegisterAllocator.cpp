#include "all.hpp"

namespace zlang
{
    RegisterAllocator::RegisterAllocator(std::vector<std::string> regs)
        : available(std::move(regs))
    {
    }

    RegisterAllocator RegisterAllocator::forSysV()
    {
        // Exclude %rsp, %rbp; use caller-saved first
        return RegisterAllocator({"rax", "rcx", "rdx", "rsi", "rdi", "r8", "r9", "r10", "r11"});
    }

    RegisterAllocator RegisterAllocator::forMSVC()
    {
        // Windows: volatile registers
        return RegisterAllocator({"rax", "rcx", "rdx", "r8", "r9", "r10", "r11", "r12", "r13", "r14", "r15"});
    }

    std::string RegisterAllocator::allocate()
    {
        if (available.empty())
            throw std::runtime_error("RegisterAllocator: no registers available");
        std::string reg = available.back();
        available.pop_back();
        inUse.insert(reg);
        return reg;
    }

    static std::string getBaseReg(const std::string &reg)
    {
        static const std::unordered_map<std::string, std::string> reg_to_base = {
            {"rax", "rax"}, {"eax", "rax"}, {"ax", "rax"}, {"al", "rax"}, {"rbx", "rbx"}, {"ebx", "rbx"}, {"bx", "rbx"}, {"bl", "rbx"}, {"rcx", "rcx"}, {"ecx", "rcx"}, {"cx", "rcx"}, {"cl", "rcx"}, {"rdx", "rdx"}, {"edx", "rdx"}, {"dx", "rdx"}, {"dl", "rdx"}, {"rsi", "rsi"}, {"esi", "rsi"}, {"si", "rsi"}, {"sil", "rsi"}, {"rdi", "rdi"}, {"edi", "rdi"}, {"di", "rdi"}, {"dil", "rdi"}, {"rbp", "rbp"}, {"ebp", "rbp"}, {"bp", "rbp"}, {"bpl", "rbp"}, {"rsp", "rsp"}, {"esp", "rsp"}, {"sp", "rsp"}, {"spl", "rsp"}, {"r8", "r8"}, {"r8d", "r8"}, {"r8w", "r8"}, {"r8b", "r8"}, {"r9", "r9"}, {"r9d", "r9"}, {"r9w", "r9"}, {"r9b", "r9"}, {"r10", "r10"}, {"r10d", "r10"}, {"r10w", "r10"}, {"r10b", "r10"}, {"r11", "r11"}, {"r11d", "r11"}, {"r11w", "r11"}, {"r11b", "r11"}, {"r12", "r12"}, {"r12d", "r12"}, {"r12w", "r12"}, {"r12b", "r12"}, {"r13", "r13"}, {"r13d", "r13"}, {"r13w", "r13"}, {"r13b", "r13"}, {"r14", "r14"}, {"r14d", "r14"}, {"r14w", "r14"}, {"r14b", "r14"}, {"r15", "r15"}, {"r15d", "r15"}, {"r15w", "r15"}, {"r15b", "r15"}};

        auto it = reg_to_base.find(reg);
        if (it == reg_to_base.end())
            throw std::runtime_error("Unknown register variant '" + reg + "'");
        return it->second;
    }

    void RegisterAllocator::free(const std::string &reg)
    {
        auto it = inUse.find(getBaseReg(reg));
        if (it == inUse.end())
        {
            if (freeXMM(reg))
            {
                return;
            }
            throw std::runtime_error("RegisterAllocator: attempted to free unallocated register '" + reg + "'");
        }
        inUse.erase(it);
        available.push_back(reg);
    }

    void RegisterAllocator::reset()
    {
        // Move all inUse back to available
        for (auto &reg : inUse)
            available.push_back(reg);
        inUse.clear();
    }

    std::string RegisterAllocator::allocateXMM()
    {
        if (availableXMM.empty())
            throw std::runtime_error("RegisterAllocator: no XMM registers available");
        std::string reg = availableXMM.back();
        availableXMM.pop_back();
        inUseXMM.insert(reg);
        return reg;
    }

    bool RegisterAllocator::freeXMM(const std::string &reg)
    {
        auto it = inUseXMM.find(reg);
        if (it == inUseXMM.end())
            return false;
        inUseXMM.erase(it);
        availableXMM.push_back(reg);
        return true;
    }

} // namespace zlang