#include <iomanip>
#include <sstream>
#include <unordered_set>

#include "codegen/machine/AsmWriter.hpp"

namespace zust::codegen::machine {

    using namespace zust::zir;

    namespace {
        const char *ptrQualifier(std::uint32_t widthBits, RegClass rc) {
            if (rc == RegClass::XMM)
                return widthBits == 32 ? "DWORD PTR " : "QWORD PTR ";
            switch (widthBits) {
                case 64:
                    return "QWORD PTR ";
                case 32:
                    return "DWORD PTR ";
                case 16:
                    return "WORD PTR ";
                case 8:
                    return "BYTE PTR ";
                default:
                    return "";
            }
        }

        std::string dataDirective(std::uint32_t bytes) {
            switch (bytes) {
                case 8:
                    return "QWORD";
                case 4:
                    return "DWORD";
                case 2:
                    return "WORD";
                default:
                    return "BYTE";
            }
        }

        std::string hexLiteral(std::uint64_t bits) {
            std::ostringstream ss;
            ss << "0" << std::uppercase << std::hex << bits << "h";
            return ss.str();
        }

        // MASM string literals have no C-style backslash escapes -- every
        // byte is written as a numeric `db` list (matches the legacy
        // CodeGenWindows convention this replaces).
        std::string bytesToDb(const std::string &bytes) {
            std::ostringstream ss;
            for (std::size_t i = 0; i < bytes.size(); ++i) {
                if (i)
                    ss << ",";
                ss << hexLiteral(static_cast<unsigned char>(bytes[i]));
            }
            return ss.str();
        }

        std::string operandText(const MachineFunction &mf, const MachineOperand &op) {
            switch (op.kind) {
                case OperandKind::Reg: {
                    const char *name = physRegName(op.preg, op.isMemory ? 64 : op.widthBits, true);
                    if (!op.isMemory)
                        return name;
                    std::string q = ptrQualifier(op.widthBits, op.regClass);
                    std::string disp = op.memDisp == 0 ? ""
                                                       : (op.memDisp > 0 ? " + " + std::to_string(op.memDisp)
                                                                         : " - " + std::to_string(-op.memDisp));
                    return q + "[" + name + disp + "]";
                }
                case OperandKind::Imm:
                    return std::to_string(static_cast<std::int64_t>(op.immBits));
                case OperandKind::FrameIndex: {
                    std::int64_t offset = mf.frameSlotOffsets[static_cast<std::size_t>(op.frameIndex)];
                    std::string q = ptrQualifier(op.widthBits, op.regClass);
                    std::string sign = offset >= 0 ? " + " : " - ";
                    return q + "[rbp" + sign + std::to_string(offset >= 0 ? offset : -offset) + "]";
                }
                case OperandKind::Global:
                    if (op.isMemory)
                        return std::string(ptrQualifier(op.widthBits, op.regClass)) + "[" + op.symbol + "]";
                    return "OFFSET " + op.symbol;
                case OperandKind::Block:
                case OperandKind::Func:
                    return op.symbol;
            }
            return "";
        }

        // Mnemonics whose text form takes no operands at all -- their
        // Instruction::operands entries (if any) exist purely so
        // LiveIntervals sees an implicit def (`cqo` defining rdx, say),
        // never to be printed.
        bool isTextOperandless(const std::string &mnemonic) {
            static const std::unordered_set<std::string> set = {"cqo", "cdq", "cwd", "ret", "ud2"};
            return set.count(mnemonic) != 0;
        }

        void printCalleeSaveRestore(const MachineFunction &mf, std::ostream &out, bool saving) {
            auto emitOne = [&](std::size_t i) {
                PhysReg r = mf.calleeSavedUsed[i];
                std::int64_t offset = mf.calleeSavedOffsets[i];
                std::string sign = offset >= 0 ? " + " : " - ";
                std::string mem = "[rbp" + sign + std::to_string(offset >= 0 ? offset : -offset) + "]";
                if (isXmm(r)) {
                    const char *reg = physRegName(r, 64, true);
                    if (saving)
                        out << "    movsd   QWORD PTR " << mem << ", " << reg << "\n";
                    else
                        out << "    movsd   " << reg << ", QWORD PTR " << mem << "\n";
                } else {
                    const char *reg = physRegName(r, 64, true);
                    if (saving)
                        out << "    mov     QWORD PTR " << mem << ", " << reg << "\n";
                    else
                        out << "    mov     " << reg << ", QWORD PTR " << mem << "\n";
                }
            };
            if (saving) {
                for (std::size_t i = 0; i < mf.calleeSavedUsed.size(); ++i)
                    emitOne(i);
            } else {
                for (std::size_t i = mf.calleeSavedUsed.size(); i-- > 0;)
                    emitOne(i);
            }
        }

        void printPrologue(const MachineFunction &mf, std::ostream &out) {
            out << "    push    rbp\n";
            out << "    mov     rbp, rsp\n";
            if (mf.frameSize > 0)
                out << "    sub     rsp, " << mf.frameSize << "\n";
            printCalleeSaveRestore(mf, out, /*saving=*/true);
        }

        void printEpilogue(const MachineFunction &mf, std::ostream &out) {
            printCalleeSaveRestore(mf, out, /*saving=*/false);
            out << "    mov     rsp, rbp\n";
            out << "    pop     rbp\n";
        }

        void printFunction(const MachineFunction &mf, std::ostream &out) {
            out << mf.name << " PROC\n";
            printPrologue(mf, out);
            for (const MachineBasicBlock &block : mf.blocks) {
                out << block.label << ":\n";
                for (const MachineInst &inst : block.insts) {
                    if (inst.mnemonic == "ret")
                        printEpilogue(mf, out);
                    out << "    " << inst.mnemonic;
                    if (!isTextOperandless(inst.mnemonic)) {
                        out << " ";
                        for (std::size_t i = 0; i < inst.operands.size(); ++i) {
                            if (i)
                                out << ", ";
                            out << operandText(mf, inst.operands[i]);
                        }
                    }
                    if (!inst.comment.empty())
                        out << "    ; " << inst.comment;
                    out << "\n";
                }
            }
            out << mf.name << " ENDP\n\n";
        }
    }  // namespace

    void AsmWriterIntel::emit(const Module &m, const std::vector<MachineFunction> &externs,
                              const std::vector<MachineFunction> &funcs,
                              const std::vector<X86InstSel::FloatConstant> &floatConsts, const TargetABI &,
                              std::ostream &out) {
        out << ".data\n\n";
        for (const GlobalVar &g : m.globals()) {
            if (g.hasInit)
                continue;  // constants (strings) go in .const below
            std::uint32_t bytes = m.layout().sizeOfBytes(m.types(), g.type);
            out << sanitizeSymbol(g.name) << " " << dataDirective(bytes) << " 0\n";
        }

        out << "\n.const\n\n";
        for (const GlobalVar &g : m.globals()) {
            if (!g.hasInit)
                continue;
            out << sanitizeSymbol(g.name) << " db " << bytesToDb(g.initBytes) << "\n";
        }
        for (const X86InstSel::FloatConstant &fc : floatConsts) {
            out << fc.label << " " << (fc.widthBits == 32 ? "DWORD" : "QWORD") << " " << hexLiteral(fc.bits) << "\n";
        }

        out << "\n.code\n\n";
        for (const MachineFunction &mf : externs)
            out << "EXTERN " << mf.name << ":FAR\n";
        out << "\n";

        for (const MachineFunction &mf : funcs)
            printFunction(mf, out);

        out << "END\n";
    }

}  // namespace zust::codegen::machine
