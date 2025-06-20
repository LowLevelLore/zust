#include "all.hpp"

namespace zlang {
    RegisterAllocator::RegisterAllocator(std::vector<std::string> regs, std::vector<std::string> XMMregs, std::vector<std::string> argumentRegs, std::vector<std::string> argumentXMMRegs)
        : available(std::move(regs)), availableXMM(std::move(XMMregs)), availableArgumentRegs(std::move(argumentRegs)), availableArgumentRegsXMM(std::move(argumentXMMRegs)) {
    }

    RegisterAllocator RegisterAllocator::forSysV() {
        return RegisterAllocator(
            SCRATCH_GPR_LINUX,
            SCRATCH_XMM_LINUX,
            ARG_GPR_LINUX,
            ARG_XMM_LINUX);
    }

    RegisterAllocator RegisterAllocator::forMSVC() {
        return RegisterAllocator(
            SCRATCH_GPR_MSVC,
            SCRATCH_XMM_MSVC,
            ARG_GPR_MSVC,
            ARG_XMM_MSVC);
    }

    std::string RegisterAllocator::allocate() {
        if (available.empty()) {
            throw std::runtime_error("No free general-purpose registers available");
        }
        std::string reg = available.back();
        available.pop_back();
        inUse.insert(reg);
        lruRegs.push_back(reg);
        return reg;
    }

    bool RegisterAllocator::isInUse(std::string reg) {
        return inUse.find(reg) != inUse.end();
    }

    bool RegisterAllocator::isInUseXMM(std::string reg) {
        return inUseXMM.find(reg) != inUseXMM.end();
    }

    std::string RegisterAllocator::getBaseReg(const std::string &reg) {
        static const std::unordered_map<std::string, std::string> reg_to_base = {
            {"rax", "rax"}, {"eax", "rax"}, {"ax", "rax"}, {"al", "rax"}, {"rbx", "rbx"}, {"ebx", "rbx"}, {"bx", "rbx"}, {"bl", "rbx"}, {"rcx", "rcx"}, {"ecx", "rcx"}, {"cx", "rcx"}, {"cl", "rcx"}, {"rdx", "rdx"}, {"edx", "rdx"}, {"dx", "rdx"}, {"dl", "rdx"}, {"rsi", "rsi"}, {"esi", "rsi"}, {"si", "rsi"}, {"sil", "rsi"}, {"rdi", "rdi"}, {"edi", "rdi"}, {"di", "rdi"}, {"dil", "rdi"}, {"rbp", "rbp"}, {"ebp", "rbp"}, {"bp", "rbp"}, {"bpl", "rbp"}, {"rsp", "rsp"}, {"esp", "rsp"}, {"sp", "rsp"}, {"spl", "rsp"}, {"r8", "r8"}, {"r8d", "r8"}, {"r8w", "r8"}, {"r8b", "r8"}, {"r9", "r9"}, {"r9d", "r9"}, {"r9w", "r9"}, {"r9b", "r9"}, {"r10", "r10"}, {"r10d", "r10"}, {"r10w", "r10"}, {"r10b", "r10"}, {"r11", "r11"}, {"r11d", "r11"}, {"r11w", "r11"}, {"r11b", "r11"}, {"r12", "r12"}, {"r12d", "r12"}, {"r12w", "r12"}, {"r12b", "r12"}, {"r13", "r13"}, {"r13d", "r13"}, {"r13w", "r13"}, {"r13b", "r13"}, {"r14", "r14"}, {"r14d", "r14"}, {"r14w", "r14"}, {"r14b", "r14"}, {"r15", "r15"}, {"r15d", "r15"}, {"r15w", "r15"}, {"r15b", "r15"}};

        if (reg.starts_with("xmm")) {
            return reg;
        }

        auto it = reg_to_base.find(reg);
        if (it == reg_to_base.end())
            throw std::runtime_error("Unknown register variant '" + reg + "'");
        return it->second;
    }

    void RegisterAllocator::free(const std::string &reg) {
        auto it = inUse.find(getBaseReg(reg));
        if (it == inUse.end()) {
            if (freeXMM(getBaseReg(reg))) {
                return;
            }
            std::cout << "Free Normal" << std::endl;
            throw std::runtime_error("RegisterAllocator: attempted to free unallocated register '" + reg + "'");
        }
        inUse.erase(it);
        lruRegs.remove(reg);
        available.push_back(getBaseReg(reg));
    }

    void RegisterAllocator::reset() {
        // Move all inUse back to available
        for (auto &reg : inUse)
            available.push_back(reg);
        inUse.clear();
    }

    std::string RegisterAllocator::allocateXMM() {
        if (availableXMM.empty()) {
            throw std::runtime_error("No free XMM registers available");
        }
        std::string reg = availableXMM.back();
        availableXMM.pop_back();
        inUseXMM.insert(reg);
        lruXMMRegs.push_back(reg);
        return reg;
    }

    bool RegisterAllocator::freeXMM(const std::string &reg) {
        auto it = inUseXMM.find(reg);
        if (it == inUseXMM.end())
            return false;
        inUseXMM.erase(it);
        availableXMM.push_back(reg);
        lruXMMRegs.remove(reg);
        return true;
    }

    std::string RegisterAllocator::allocateArgument(uint8_t position) {
        std::string reg = availableArgumentRegs[position];
        inUseArgument.insert(reg);
        return reg;
    }
    std::string RegisterAllocator::allocateArgumentXMM(uint8_t position) {
        std::string reg = availableArgumentRegsXMM[position];
        inUseArgumentXMM.insert(reg);
        return reg;
    }
    void RegisterAllocator::freeArgument(const std::string &reg) {
        auto it = inUseArgument.find(getBaseReg(reg));
        if (it == inUseArgument.end()) {
            if (freeArgumentXMM(getBaseReg(reg))) {
                return;
            }
            throw std::runtime_error("RegisterAllocator: attempted to free unallocated register '" + reg + "'");
        }
        inUseArgument.erase(it);
    }
    bool RegisterAllocator::freeArgumentXMM(const std::string &reg) {
        auto it = inUseArgumentXMM.find(getBaseReg(reg));
        if (it == inUseArgumentXMM.end()) {
            return false;
        }
        inUseArgumentXMM.erase(it);
        return true;
    }
    bool RegisterAllocator::isInUseArgument(const std::string &reg) const {
        return inUseArgument.find(reg) != inUseArgument.end();
    }
    bool RegisterAllocator::isInUseArgumentXMM(const std::string &reg) const {
        return inUseArgumentXMM.find(reg) != inUseArgumentXMM.end();
    }
    std::string RegisterAllocator::pickVictimXMM() {
        if (lruXMMRegs.empty()) {
            throw std::runtime_error("No victim available for XMM registers");
        }
        return lruXMMRegs.front();
    }
    std::string RegisterAllocator::pickVictim() {
        if (lruRegs.empty()) {
            throw std::runtime_error("No victim available for general-purpose registers");
        }
        return lruRegs.front();
    }

    void RegisterAllocator::markSpilledXMM(const std::string &reg, const std::string &spillSlot) {
        spilledRegs[reg] = {spillSlot, true};
    }

    void RegisterAllocator::markSpilled(const std::string &reg, const std::string &spillSlot) {
        spilledRegs[reg] = {spillSlot, false};
    }

    bool RegisterAllocator::isSpilled(const std::string &reg) const {
        return spilledRegs.find(reg) != spilledRegs.end();
    }

    std::string RegisterAllocator::spillSlotFor(const std::string &reg) const {
        auto it = spilledRegs.find(reg);
        if (it == spilledRegs.end())
            throw std::runtime_error("Register not spilled: " + reg);
        return it->second.spillSlot;
    }

    void RegisterAllocator::unSpillXMM(const std::string &reg, zlang::CodegenOutputFormat format, std::ostream &out) {
        auto it = spilledRegs.find(reg);
        if (it == spilledRegs.end())
            throw std::runtime_error("XMM reg not spilled: " + reg);

        emitSpillRestore(reg, it->second.spillSlot, true, format, out);
        spilledRegs.erase(it);
    }

    void RegisterAllocator::unSpill(const std::string &reg, zlang::CodegenOutputFormat format, std::ostream &out) {
        auto it = spilledRegs.find(reg);
        if (it == spilledRegs.end())
            throw std::runtime_error("GPR reg not spilled: " + reg);

        emitSpillRestore(reg, it->second.spillSlot, false, format, out);
        spilledRegs.erase(it);
    }

    void RegisterAllocator::emitSpillRestore(const std::string &reg, const std::string &slot, bool isXMM, zlang::CodegenOutputFormat format, std::ostream &out) {
        if (format == CodegenOutputFormat::X86_64_LINUX) {
            if (isXMM)
                out << "    movdqu " << slot << ", %" << reg << "\n";
            else
                out << "    mov " << slot << ", %" << reg << "\n";
        }
        if (format == CodegenOutputFormat::X86_64_MSWIN) {
            if (isXMM)
                out << "    movdqu " << reg << ", " << slot << "\n";
            else
                out << "    mov " << reg << ", " << slot << "\n";
        }
    }

    void RegisterAllocator::touch(const std::string &reg) {
        if (inUse.count(reg)) {
            lruRegs.remove(reg);
            lruRegs.push_back(reg);
        }
    }

    void RegisterAllocator::touchXMM(const std::string &reg) {
        if (inUseXMM.count(reg)) {
            lruXMMRegs.remove(reg);
            lruXMMRegs.push_back(reg);
        }
    }
}  // namespace zlang