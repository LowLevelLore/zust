#include "codegen/machine/NativeBackend.hpp"

#include "codegen/machine/AsmWriter.hpp"
#include "codegen/machine/FrameLayout.hpp"
#include "codegen/machine/LinearScan.hpp"
#include "codegen/machine/X86InstSel.hpp"

namespace zust::codegen::machine {

    void emitNative(zir::Module &m, const TargetABI &abi, bool intelSyntax, std::ostream &out) {
        X86InstSel sel(m, abi);
        LinearScan alloc(abi);

        std::vector<MachineFunction> externs, funcs;
        for (std::size_t i = 0; i < m.functions().size(); ++i) {
            zir::Function &fn = m.function(zir::FuncId(static_cast<zir::FuncId::Value>(i)));
            MachineFunction mf = sel.select(fn);
            if (fn.isExtern()) {
                externs.push_back(std::move(mf));
                continue;
            }
            alloc.run(mf);
            FrameLayout::compute(mf, abi);
            funcs.push_back(std::move(mf));
        }

        if (intelSyntax)
            AsmWriterIntel::emit(m, externs, funcs, sel.floatConstants(), abi, out);
        else
            AsmWriterAtt::emit(m, externs, funcs, sel.floatConstants(), abi, out);
    }

}  // namespace zust::codegen::machine
