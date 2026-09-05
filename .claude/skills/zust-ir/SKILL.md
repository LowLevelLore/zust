---
name: zust-ir
description: "Designing and implementing ZIR, the zust typed SSA intermediate representation, and its optimization passes. Use when: building the IR, writing AST-to-ZIR lowering, implementing or debugging an optimization pass, working on the ZIR verifier or interpreter, or migrating a backend to consume ZIR."
---

# ZIR — Building the IR and Its Passes

The full specification is `docs/IR-DESIGN.md`. Read it first; this skill is how to
*work* on ZIR, not what ZIR is.

Status: not yet implemented. ROADMAP M3–M5.

## Non-negotiables

1. **Typed SSA.** Every value defined once, every value has a `TypeId`.
2. **Block arguments, not phi nodes.** `br ^bb(%a, %b)`. This keeps critical-edge
   splitting local and removes a whole class of phi-ordering bugs.
3. **Arena allocated.** `ValueId`/`BlockId`/`InstId`/`TypeId` are `uint32` indices
   into flat vectors. No `unique_ptr` graphs — the optimizer traverses these
   structures thousands of times.
4. **Locals lower to `alloca`.** Always. `mem2reg` promotes them later. Do not try
   to be clever in the lowerer; correctness there is worth more than the pass you
   would be duplicating.
5. **The verifier runs after every pass** under assertions. A verifier failure is
   a compiler bug — print the function and abort.
6. **Target-neutral.** Sizes come from `TargetLayout`, not from the type. No
   backend concepts (registers, stack slots, ABI) appear in ZIR.

## Implementation order

Do not skip ahead; each step is testable on its own.

1. `TypeTable` with interning. Test: identical structural types get equal `TypeId`.
2. Data structures: `Module`, `Function`, `BasicBlock`, `Instruction`, arenas.
3. **Printer** — textual form per the spec.
4. **Parser** for the textual form. Test: print → parse → print is a fixed point.
   This is what makes every later pass unit-testable from a `.zir` file.
5. **Verifier.** Test: hand-write broken IR, assert each check fires.
6. **AST → ZIR lowering.** Test: every existing `tests/runtime` case lowers and verifies.
7. **Interpreter.** Test: interpreter output equals current native output for every
   `tests/runtime` case. This is the oracle everything else is checked against.
8. **Pass manager** + `AnalysisManager` with `DominatorTree`.
9. Passes, in the order below.
10. Backend migration (see `zust-backend-abi`), LLVM first.

## Writing a pass

```cpp
class Mem2Reg final : public Pass {
public:
    const char *name() const override { return "mem2reg"; }
    bool run(Function &fn, AnalysisManager &am) override;   // true if changed
};
```

Rules:

- Return `true` **only if you actually changed something**. The pass manager
  iterates to a fixpoint; a pass that always returns `true` hangs the compiler.
- Declare which analyses you invalidate. Anything touching control flow
  invalidates `DominatorTree` and `LoopInfo`.
- Never delete a value that still has uses. Check the use list; the verifier will
  catch it, but the error is clearer if you assert at the deletion site.
- One pass, one transformation. `simplifycfg` does not fold constants; `sccp` does
  not delete blocks. Composition is the point.
- Every pass gets unit tests written as `.zir` input → expected `.zir` output,
  under `tests/zir/<pass>/`. This is why the textual round-trip matters.

## Pass order and what each is for

| Pass | Does | Depends on |
|---|---|---|
| `mem2reg` | Promotes non-address-taken `alloca`s to SSA values. **Highest value pass — everything else is weak without it.** | DominatorTree |
| `instcombine` | Local algebraic identities: `x+0`, `x*1`, `x*2`→`x<<1`, double negation | — |
| `sccp` | Sparse conditional constant propagation; folds constants and removes branches known-taken | — |
| `simplifycfg` | Merges blocks with a single pred/succ, folds constant branches, drops empty and unreachable blocks | — |
| `dce` / `adce` | Removes values with no uses and no side effects | use lists |
| `gvn` | Global value numbering — removes redundant computation | DominatorTree, AA |
| `inline` | Inlines by cost model; unlocks all the above across calls | CallGraph |
| `licm` | Hoists loop-invariant computation out of loops | LoopInfo, AA |
| `indvars` | Simplifies induction variables, enables strength reduction | LoopInfo, SCEV-lite |
| `tailcall` | Turns self-tail-calls into jumps. Matters — the language encourages recursion | — |

Pipelines (`-O0`…`-O3`) are specified in `docs/IR-DESIGN.md`.

## Debugging a pass

```bash
./build/zpiler --emit=zir prog.zz                    # IR straight out of lowering
./build/zpiler -O2 --print-zir-after=mem2reg prog.zz # after one pass
./build/zpiler -O2 --print-zir-after-all prog.zz     # after every pass
./build/zpiler -O2 --disable-pass=gvn prog.zz        # bisect which pass broke it
```

Standard triage for "optimized output is wrong":

1. Confirm `-O0` is correct. If not, the bug is in lowering, not a pass.
2. Bisect with `--disable-pass` until it works. That names the culprit.
3. Dump before and after that pass; diff the IR.
4. Reduce to the smallest function that still miscompiles.
5. Save it as a `.zir` regression test under `tests/zir/<pass>/`.

The **interpreter is the arbiter**: run the IR before and after the pass through
it. If they disagree, the pass is wrong, regardless of how reasonable the output
assembly looks.

## Migrating a backend to ZIR

Do this incrementally with the suite green at every step:

1. Add the ZIR path behind a flag; keep the legacy AST path as the default.
2. Make the ZIR path produce output for one construct at a time.
3. Compare byte-for-byte against the legacy path on the whole test suite. Not
   "equivalent" — identical, until you deliberately change something.
4. Flip the default; keep `--legacy-codegen` as an escape hatch for one release.
5. Delete the legacy path.

The LLVM backend goes first: ZIR → LLVM IR is nearly 1:1 once ZIR is SSA, so it
validates the IR design cheaply before you commit to it in the native backends.
