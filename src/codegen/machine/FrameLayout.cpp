#include "codegen/machine/FrameLayout.hpp"

namespace zust::codegen::machine {

    void FrameLayout::compute(MachineFunction &mf, const TargetABI &) {
        std::size_t calleeSavedCount = mf.calleeSavedUsed.size();
        std::size_t localCount = mf.frameSlots.size();
        std::size_t totalSlots = calleeSavedCount + localCount;

        // Callee-saved save areas first (closest to rbp), then locals and
        // spills -- an arbitrary but stable ordering; nothing depends on
        // which comes first.
        mf.calleeSavedOffsets.assign(calleeSavedCount, 0);
        for (std::size_t i = 0; i < calleeSavedCount; ++i)
            mf.calleeSavedOffsets[i] = -static_cast<std::int64_t>(8 * (i + 1));

        mf.frameSlotOffsets.assign(localCount, 0);
        for (std::size_t i = 0; i < localCount; ++i) {
            mf.frameSlotOffsets[i] = -static_cast<std::int64_t>(8 * (calleeSavedCount + i + 1));
        }

        std::int64_t rawSize = static_cast<std::int64_t>(8 * totalSlots);
        mf.frameSize = (rawSize + 15) & ~std::int64_t{15};
        mf.needsFramePointer = true;
    }

}  // namespace zust::codegen::machine
