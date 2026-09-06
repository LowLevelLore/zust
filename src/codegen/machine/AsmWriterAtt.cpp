#include <sstream>
#include <unordered_set>

#include "codegen/machine/AsmWriter.hpp"

// AT&T syntax (Wave 5.6's other half; exercised by Wave 5's own trivial-
// function smoke test and future Linux (Wave 6.1) work, not by the
// Windows-target golden suite this session's Wave 6.2 verifies against).

namespace zust::codegen::machine {

    using namespace zust::zir;

    namespace {
        char suffixFor(std::uint32_t widthBits) {
            switch (widthBits) {
                case 64:
                    return 'q';
                case 32:
                    return 'l';
                case 16:
                    return 'w';
                default:
                    return 'b';
            }
        }

        // Mnemonics whose AT&T form needs a size suffix appended when it
        // isn't already implied by an SSE mnemonic or a register name alone
        // (GNU as accepts the suffix unconditionally, so this always adds
        // it rather than trying to prove it's redundant).
        bool takesSuffix(const std::string &mnemonic) {
            static const std::unordered_set<std::string> set = {
                "mov",  "add",  "sub",    "and",  "or",   "xor",  "cmp",  "neg",
                "not",  "imul", "idiv",   "div",  "shl",  "shr",  "sar",  "movzx",
                "movsx", "movsxd"};
            return set.count(mnemonic) != 0;
        }

        bool isTextOperandless(const std::string &mnemonic) {
            static const std::unordered_set<std::string> set = {"cqo", "cdq", "cwd", "ret", "ud2"};
            return set.count(mnemonic) != 0;
        }

        std::string reg(PhysReg r, std::uint32_t width) { return std::string("%") + physRegName(r, width, false); }

        std::string operandText(const MachineFunction &mf, const MachineOperand &op) {
            switch (op.kind) {
                case OperandKind::Reg:
                    if (!op.isMemory)
                        return reg(op.preg, op.widthBits);
                    {
                        std::string base = reg(op.preg, 64);
                        if (op.memDisp == 0)
                            return "(" + base + ")";
                        return std::to_string(op.memDisp) + "(" + base + ")";
                    }
                case OperandKind::Imm:
                    return "$" + std::to_string(static_cast<std::int64_t>(op.immBits));
                case OperandKind::FrameIndex: {
                    std::int64_t offset = mf.frameSlotOffsets[static_cast<std::size_t>(op.frameIndex)];
                    return std::to_string(offset) + "(%rbp)";
                }
                case OperandKind::Global:
                    if (op.isMemory)
                        return op.symbol + "(%rip)";
                    return op.symbol + "(%rip)";  // lea's address-of form -- same textual operand either way
                case OperandKind::Block:
                case OperandKind::Func:
                    return op.symbol;
            }
            return "";
        }

        // AT&T operand order is source-first; every mnemonic this pipeline
        // emits that takes 2+ real operands (everything except `lea`,
        // which is *already* address-then-destination in both syntaxes) is
        // Intel dest-first internally, so the common case is "reverse it".
        std::vector<std::size_t> attOrder(const std::string &mnemonic, std::size_t count) {
            std::vector<std::size_t> order(count);
            for (std::size_t i = 0; i < count; ++i) order[i] = count - 1 - i;
            if (mnemonic == "lea" || mnemonic == "call" || mnemonic == "jmp" || mnemonic.rfind('j', 0) == 0 ||
                count <= 1)
                for (std::size_t i = 0; i < count; ++i) order[i] = i;
            return order;
        }

        void printCalleeSaveRestore(const MachineFunction &mf, std::ostream &out, bool saving) {
            auto emitOne = [&](std::size_t i) {
                PhysReg r = mf.calleeSavedUsed[i];
                std::int64_t offset = mf.calleeSavedOffsets[i];
                std::string mem = std::to_string(offset) + "(%rbp)";
                if (isXmm(r)) {
                    if (saving)
                        out << "    movsd   " << reg(r, 64) << ", " << mem << "\n";
                    else
                        out << "    movsd   " << mem << ", " << reg(r, 64) << "\n";
                } else {
                    if (saving)
                        out << "    movq    " << reg(r, 64) << ", " << mem << "\n";
                    else
                        out << "    movq    " << mem << ", " << reg(r, 64) << "\n";
                }
            };
            if (saving) {
                for (std::size_t i = 0; i < mf.calleeSavedUsed.size(); ++i) emitOne(i);
            } else {
                for (std::size_t i = mf.calleeSavedUsed.size(); i-- > 0;) emitOne(i);
            }
        }

        void printPrologue(const MachineFunction &mf, std::ostream &out) {
            out << "    push    %rbp\n";
            out << "    movq    %rsp, %rbp\n";
            if (mf.frameSize > 0)
                out << "    subq    $" << mf.frameSize << ", %rsp\n";
            printCalleeSaveRestore(mf, out, true);
        }

        void printEpilogue(const MachineFunction &mf, std::ostream &out) {
            printCalleeSaveRestore(mf, out, false);
            out << "    movq    %rbp, %rsp\n";
            out << "    pop     %rbp\n";
        }

        void printFunction(const MachineFunction &mf, std::ostream &out) {
            out << mf.name << ":\n";
            printPrologue(mf, out);
            for (const MachineBasicBlock &block : mf.blocks) {
                out << block.label << ":\n";
                for (const MachineInst &inst : block.insts) {
                    if (inst.mnemonic == "ret")
                        printEpilogue(mf, out);
                    std::string mnemonic = inst.mnemonic;
                    if (!isTextOperandless(mnemonic) && takesSuffix(mnemonic) && !inst.operands.empty()) {
                        // Suffix from the widest explicit operand -- always
                        // correct for this pipeline's own same-width
                        // 2-operand instructions.
                        std::uint32_t width = 32;
                        for (const MachineOperand &op : inst.operands)
                            if (op.kind == OperandKind::Reg || op.kind == OperandKind::FrameIndex)
                                width = op.widthBits;
                        mnemonic += suffixFor(width);
                    }
                    out << "    " << mnemonic;
                    if (!isTextOperandless(inst.mnemonic)) {
                        out << " ";
                        std::vector<std::size_t> order = attOrder(inst.mnemonic, inst.operands.size());
                        for (std::size_t i = 0; i < order.size(); ++i) {
                            if (i)
                                out << ", ";
                            out << operandText(mf, inst.operands[order[i]]);
                        }
                    }
                    out << "\n";
                }
            }
            out << "\n";
        }
    }  // namespace

    void AsmWriterAtt::emit(const Module &m, const std::vector<MachineFunction> &externs,
                            const std::vector<MachineFunction> &funcs,
                            const std::vector<X86InstSel::FloatConstant> &floatConsts, const TargetABI &,
                            std::ostream &out) {
        out << "    .data\n";
        for (const GlobalVar &g : m.globals()) {
            if (g.hasInit)
                continue;
            std::uint32_t bytes = m.layout().sizeOfBytes(m.types(), g.type);
            out << sanitizeSymbol(g.name) << ":\n    ." << (bytes == 8 ? "quad" : bytes == 4 ? "long" : bytes == 2 ? "word" : "byte")
                << " 0\n";
        }

        out << "\n    .section .rodata\n";
        for (const GlobalVar &g : m.globals()) {
            if (!g.hasInit)
                continue;
            out << sanitizeSymbol(g.name) << ":\n    .byte ";
            for (std::size_t i = 0; i < g.initBytes.size(); ++i) {
                if (i)
                    out << ",";
                out << static_cast<unsigned>(static_cast<unsigned char>(g.initBytes[i]));
            }
            out << "\n";
        }
        for (const X86InstSel::FloatConstant &fc : floatConsts) {
            out << fc.label << ":\n    ." << (fc.widthBits == 32 ? "long" : "quad") << " " << fc.bits << "\n";
        }

        out << "\n    .text\n";
        for (const MachineFunction &mf : externs) out << "    .extern " << mf.name << "\n";
        out << "\n";

        for (const MachineFunction &mf : funcs) {
            out << "    .globl " << mf.name << "\n";
            printFunction(mf, out);
        }
    }

}  // namespace zust::codegen::machine
