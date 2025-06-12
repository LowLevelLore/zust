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

    void RegisterAllocator::free(const std::string &reg)
    {
        auto it = inUse.find(reg);
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