#include "codegen/machine/MachineIR.hpp"

#include <stdexcept>

namespace zust::codegen::machine {

    bool isXmm(PhysReg r) { return r >= PhysReg::XMM0 && r <= PhysReg::XMM15; }

    namespace {
        // [width64][width32][width16][width8] per GPR, for both syntaxes --
        // the sub-register *names* are identical in AT&T and Intel (only the
        // `%`/operand-order conventions differ, handled by each AsmWriter).
        struct GprNames {
            const char *r64, *r32, *r16, *r8;
        };

        const GprNames &gprNames(PhysReg r) {
            static const GprNames table[] = {
                {"", "", "", ""},  // None
                {"rax", "eax", "ax", "al"},
                {"rbx", "ebx", "bx", "bl"},
                {"rcx", "ecx", "cx", "cl"},
                {"rdx", "edx", "dx", "dl"},
                {"rsi", "esi", "si", "sil"},
                {"rdi", "edi", "di", "dil"},
                {"rbp", "ebp", "bp", "bpl"},
                {"rsp", "esp", "sp", "spl"},
                {"r8", "r8d", "r8w", "r8b"},
                {"r9", "r9d", "r9w", "r9b"},
                {"r10", "r10d", "r10w", "r10b"},
                {"r11", "r11d", "r11w", "r11b"},
                {"r12", "r12d", "r12w", "r12b"},
                {"r13", "r13d", "r13w", "r13b"},
                {"r14", "r14d", "r14w", "r14b"},
                {"r15", "r15d", "r15w", "r15b"},
            };
            auto idx = static_cast<std::size_t>(r);
            if (idx >= sizeof(table) / sizeof(table[0]))
                throw std::runtime_error("physRegName: not a GPR");
            return table[idx];
        }

        const char *xmmName(PhysReg r) {
            static const char *table[] = {"xmm0", "xmm1", "xmm2", "xmm3", "xmm4",  "xmm5",  "xmm6",  "xmm7",
                                          "xmm8", "xmm9", "xmm10", "xmm11", "xmm12", "xmm13", "xmm14", "xmm15"};
            return table[static_cast<std::size_t>(r) - static_cast<std::size_t>(PhysReg::XMM0)];
        }
    }  // namespace

    std::string sanitizeSymbol(const std::string &name) {
        std::string out = name;
        for (char &c : out)
            if (c == '.')
                c = '_';
        return out;
    }

    // `intelSyntax` is accepted for symmetry with how callers reason about
    // the two writers, but register *spelling* doesn't actually differ
    // between AT&T and Intel (only the leading `%` and operand order do,
    // which each AsmWriter adds itself) -- this exists as one place either
    // writer can ask "what do I call this register" without duplicating the
    // sub-register tables.
    const char *physRegName(PhysReg r, std::uint32_t widthBits, bool /*intelSyntax*/) {
        if (isXmm(r))
            return xmmName(r);
        const GprNames &n = gprNames(r);
        switch (widthBits) {
            case 64:
                return n.r64;
            case 32:
                return n.r32;
            case 16:
                return n.r16;
            case 8:
                return n.r8;
            default:
                throw std::runtime_error("physRegName: unsupported width");
        }
    }

}  // namespace zust::codegen::machine
