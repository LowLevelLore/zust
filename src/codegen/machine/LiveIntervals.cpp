#include "codegen/machine/LiveIntervals.hpp"

#include <algorithm>
#include <unordered_map>

namespace zust::codegen::machine {

    std::vector<LiveInterval> LiveIntervals::compute(const MachineBasicBlock &block) {
        std::unordered_map<std::uint32_t, LiveInterval> byVreg;

        for (std::size_t i = 0; i < block.insts.size(); ++i) {
            const MachineInst &inst = block.insts[i];
            for (std::size_t oi = 0; oi < inst.operands.size(); ++oi) {
                const MachineOperand &op = inst.operands[oi];
                if (op.kind != OperandKind::Reg || !op.isVirtual)
                    continue;
                bool isDef = std::find(inst.defIndices.begin(), inst.defIndices.end(), oi) != inst.defIndices.end();
                auto it = byVreg.find(op.vreg);
                if (it == byVreg.end()) {
                    // First sighting. A def starts the interval; a use with
                    // no prior def (not expected -- see class comment)
                    // still gets a well-formed interval rather than being
                    // dropped.
                    byVreg[op.vreg] = LiveInterval{op.vreg, static_cast<int>(i), static_cast<int>(i)};
                } else if (!isDef) {
                    it->second.end = static_cast<int>(i);
                }
            }
        }

        std::vector<LiveInterval> result;
        result.reserve(byVreg.size());
        for (auto &[vreg, interval] : byVreg) result.push_back(interval);
        std::sort(result.begin(), result.end(), [](const LiveInterval &a, const LiveInterval &b) {
            return a.start < b.start;
        });
        return result;
    }

}  // namespace zust::codegen::machine
