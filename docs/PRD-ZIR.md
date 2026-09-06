# PRD — Stage Isolation + ZIR (five-stage pipeline, -O0..-O3)

Status: **active**. This is the tracked work breakdown for the ZIR rewrite. Tick
boxes as items land. Ownership tags: **[S]** shared/target-neutral · **[L]**
Linux/SysV · **[W]** Windows/Win64 · **[V]** LLVM.

See `docs/IR-DESIGN.md` for the ZIR spec, `docs/BACKENDS.md` for the backend
registry design.

## Hard constraints (do not violate)

1. **`tests/**/*.zz` and `tests/expected/**` are frozen.** No `--bless`, ever, in this
   work. New tests may be *added* under `tests/zir/`; existing ones are never edited.
2. **All 40 existing cases pass at every optimization level on every backend** —
   40 × {linux, windows, llvm} × {-O0,-O1,-O2,-O3} = 480 green cells.
3. **Linux and Windows are equally first-class.** No item is done until green on both.
4. **Backends decide nothing** — no folding/DCE/strength-reduction in an emitter.
5. **No layer outside `src/codegen/` branches on target** —
   `grep -rn "CodegenOutputFormat\|TargetTriple" src/parser src/support src/zir` must
   stay empty (this replaces those two enums entirely; see Wave 0).

## The landmine: Sema does not check function bodies today

`TypeChecker::checkNode`'s `NodeType::Function` case
(`src/typechecker/TypeChecker.cpp:103-133`) validates only the parameter list and
return type, then returns — **it never recurses into the body**. Confirmed by
reading the code directly: there is no call into `children[2]` anywhere in that
case. `NodeType::Program` does recurse over top-level statements, but nothing
ever hands a function body to `checkNode`.

Consequence: today's compiler only type-checks globals, `extern` signatures,
function *signatures*, and bare top-level expression statements. Everything
inside `fn { ... }` — every `if`, every `printf(...)`, every comparison — is
currently **unchecked**.

If real body-checking is wired in with today's literal rules, it breaks on
first contact with the existing tests, because:

- **No variadic exemption in call-arity/type checking**
  (`TypeChecker.cpp:78-84`) — every `printf(fmt, arg, ...)` call has more
  arguments than the extern's one declared parameter, and would fail arity
  checking.
- **`boolean` is not numeric** (`TypeChecker.hpp:14-16`) so `x == 1` where
  `x: boolean` (used by `print_bool` in at least 8 test files, e.g.
  `conditionals/boolean_logic.zz`, `functions/basics.zz`, `operations/binary.zz`)
  would fail the comparison-operand rule.
- **Call argument types vs `%f`/`%d` format strings** — `printf("%f\n", x: float)`
  has arg type `float` vs the extern's declared `string`; a literal type match
  would reject it.

**Rule for this rewrite:** Sema-over-bodies is landed once (Wave 2, item 2.3), on
the **legacy AST backends**, before any ZIR work touches it, with these
exemptions baked in from the start:
- Variadic call tails are exempt from arity and per-argument type checking.
- `boolean` is treated as comparable with the numeric types (or the comparison
  rule stays permissive), matching what programs already assume.
- `if`/`while`/`for` conditions are accepted at any scalar type (native backends
  already test the *whole* register against zero; see the boolean-representation
  note below).

If a test stops compiling after 2.3 lands, that is a Sema bug to fix by relaxing
the rule — **never** a signal to edit a `.zz` file.

## Behavior inventory — what ZIRGen must reproduce exactly

Each item below is empirically confirmed against the current backends. These are
observable behaviors the 40 goldens depend on (or, where noted, behaviors the
goldens do NOT exercise but a naive rewrite would still change). Get these into
`tests/zir/` as pinning cases where the existing suite doesn't already cover them.

- [x] **`&&` / `||` are non-short-circuiting.** All three backends lower them to a
      plain bitwise `and`/`or` on the promoted type, evaluating both operands
      unconditionally — `CodeGenLinux.cpp:349`, `CodeGenWindows.cpp:404`,
      `CodeGenLLVM.cpp:316`. No existing test has a side-effecting RHS, so a
      short-circuiting `condbr` lowering would pass the whole suite and still be
      a silent language change. Lower to `and`/`or` on `i1` deliberately; add a
      new `tests/zir/` case with an effectful RHS.
- [x] **Global hoisting into `main`** — `CodeGenLLVM.cpp:700-744`, mirrored in
      both native backends. Four rules:
      1. Every top-level `VariableDeclaration` becomes a **zero-initialized**
         global; the initializer expression is not a static initializer.
      2. Top-level statements split three ways: `main` set aside;
         `VariableDeclaration` / `VariableReassignment` / `UnaryOp ++|--` collected
         into `declarationsAndReassignments` in source order; everything else
         (other functions, externs) emitted at top level in source order,
         **before** `main`.
      3. `main`'s body = the collected list (source order) followed by `main`'s
         own statements — global initializers run as stores at the top of `main`.
      4. `main` gets an unconditional trailing `ret 0` after its body. ZIR forbids
         two terminators per block, so lowering needs a verifier-legal tail
         (e.g. only append it when the body doesn't already end in `ret`).
- [x] **`promoteType` lattice** — reproduce `TypeChecker.hpp:28-77` exactly,
      including the easy-to-miss rule that **an int wider than a float promotes to
      a float of the int's width** (`float32 + int64 → double`, not `float32`), and
      that two same-width ints of differing signedness promote to the **signed**
      one. `mixed_types.stdout` and `binary.stdout` depend on both.
- [x] **Float comparisons use unsigned setcc after `ucomis{s,d}`** (ROADMAP M0-7).
      Do not regress. The known NaN gap (`<`/`<=` true against NaN on natives,
      false via LLVM's ordered predicates) is unreachable today — decide the ZIR
      `fcmp` semantics deliberately rather than let it fall out of whichever
      backend is written first.
- [x] **Variadic calls** — SysV sets `al` to the vector-arg count; Win64 duplicates
      float args into the paired GPR at the same index. Every one of the 40 cases
      calls `printf`, so this is on the critical path everywhere. Both rules live
      in `X86InstSel::selectCall` behind `TargetABI::variadicRule`; SysV's
      independent GPR/XMM argument counters vs Win64's shared slots are
      `placeArgs()` behind `TargetABI::sharedArgSlots`.
- [x] **Boolean representation.** Decide once: **`i1` in ZIR, 1 byte in memory**,
      with explicit `zext`/`trunc` at every load/store boundary, and lower every
      condition as `icmp ne <ty> %v, 0` (matches the natives' "test the whole
      register" behavior and is a no-op fix for LLVM's current bit-0-only `trunc`
      divergence).
- [x] **Float literals stored as IEEE bits**, not re-derived from decimal text at
      print time — three different current pipelines (raw source text on Linux,
      decoded on Windows, `stof`/`stod` + 17-sig-digit reprint on LLVM) agree only
      by accident. Store the bit pattern in the ZIR constant and emit from bits on
      every backend to remove this as a source of divergence.
- [x] **`break`/`continue`** — `continue` in a `for` jumps to the post-statement
      (still runs the increment); in a `while` it jumps to the condition. Both
      throw if used outside a loop (also rejected by the parser at parse time).
- [x] **Missing return** — currently unchecked and currently produces a function
      with no trailing `ret` at all (execution falls into whatever is emitted
      next). No existing test exercises this. The ZIR verifier will force a
      decision (every block needs exactly one terminator) — pick `unreachable`
      to match today's undefined behavior, not a synthesized `ret 0`, and record
      the choice with a `tests/zir/` case.
- [x] **Integer widening picks `zext` vs `sext` by the *source* type's
      signedness**, matching `castValue`'s existing split — getting this backwards
      at a variadic boundary turns a wrapped `uint8_t` value like `255` into `-1`.
- [x] **DCE must treat calls as side-effecting**, or `-O1` deletes the `printf`
      every golden depends on. (Verified: 40/40 green at `-O1`/`-O2`/`-O3` on
      Linux and Windows.)
- [x] The three `tests/expected/runtime/variables.{linux,llvm,windows}.stdout`
      overrides are confirmed byte-identical to the shared `variables.stdout` —
      they are `--bless` residue, not a real per-backend divergence. Leave them
      (constraint 1); do not treat their existence as evidence of a real
      three-way difference to reproduce.

## Wave 0 — prerequisites (serial, no behavior change)

- [x] **0.1 [S]** Evict target knowledge from non-codegen layers (ROADMAP M2.5-1).
      `FunctionScope::allocateSpillSlot` returns a plain offset; backends format
      it. `RegisterAllocator::emitSpillRestore`/`unSpill` stop emitting assembly
      text. *Exit:* `grep -rn "CodegenOutputFormat" src/parser
      src/codegen/RegisterAllocator.cpp` empty; suite green on all 3 targets.
- [x] **0.2 [S]** `Backend` + `BackendRegistry` + `TargetInfo`, still AST-consuming
      for now. Delete `TargetTriple`; collapse the `main.cpp` switch; `--formats`
      / `--formats --json` generated from the registry;
      `conftest.py`/`test_pipeline.py` consume the JSON instead of a hardcoded
      `TARGETS` dict. *Exit:* suite green; adding a target is one directory + one
      line in `registerBuiltinBackends`.
- [x] **0.3 [S]** Delete `include/all.hpp`; give every TU precise includes; move
      `GLOBAL_NAME_MAPPER` off being a per-TU global. *Exit:* suite green, no file
      includes `all.hpp`.
- [x] **0.4 [S]** Add `Span` to `Token` and `ASTNode`, threaded through every
      `make*Node` factory. No diagnostic text changes. *Exit:* suite green,
      `compile_fail` stderr substrings unchanged.

## Wave 1 — ZIR core (parallel with Wave 2)

Build in this order — each step is independently testable
(`.claude/skills/zust-ir`).

- [x] **1.1 [S]** `TypeTable` with interning + `TargetLayout` (sizes/alignment
      come from the layout, never hardcoded on the type — `docs/IR-DESIGN.md`
      calls out today's hardcoded `size_t = 64` as the mistake not to repeat).
      *Exit:* unit test — structurally identical types intern to equal `TypeId`.
- [x] **1.2 [S]** `Module`/`Function`/`BasicBlock`/`Instruction` arenas +
      `Builder`. `ValueId`/`BlockId`/`InstId`/`TypeId` are `uint32` indices; block
      arguments, not phi nodes. *Exit:* a hand-built module round-trips.
- [x] **1.3 [S]** `Printer` — textual form per `docs/IR-DESIGN.md`. *Exit:* the
      spec's `@factorial` example prints byte-for-byte.
- [x] **1.4 [S]** `.zir` `TextParser`. *Exit:* print→parse→print is a fixed point
      on every fixture in `tests/zir/roundtrip/`.
- [x] **1.5 [S]** `Verifier`, all 8 checks from `docs/IR-DESIGN.md`. *Exit:* eight
      hand-broken `.zir` fixtures each trip exactly their intended check.

## Wave 2 — Sema (parallel with Wave 1)

- [x] **2.1 [S]** Resolver: names → `SymbolId`. Parser stops calling
      `ScopeContext::defineVariable → allocateStack` — no more frame slots at
      parse time. *Exit:* every reference has a `SymbolId`; legacy backends keep
      working via a temporary `SymbolId → offset` shim.
- [x] **2.2 [S]** `TypeRules::promote` ported bit-for-bit from
      `TypeChecker::promoteType`, over `TypeId`. *Exit:* a generated 13×13 table
      test comparing the new function against the old one before the old one is
      deleted.
- [x] **2.3 [S] — the landmine item.** Sema over function bodies, with the
      exemptions above. Land alone, on the legacy backends, before ZIR consumes
      anything. *Exit:* all 40 cases still compile unchanged.
- [x] **2.4 [S]** Definite-return analysis, **warning-only** in this rewrite (an
      error here would change `compile_fail` surface, which is frozen). *Exit:*
      reports nothing on the current suite.

## Wave 3 — Lowering

- [x] **3.1 [S]** `ZirGen` for expressions/statements. Every rule in the behavior
      inventory above lives here and nowhere else. Locals →
      `alloca`/`load`/`store`, no cleverness (mem2reg promotes later). *Exit:*
      all 40 cases lower and verify clean at `-O0`; `--emit=zir` round-trips.
- [x] **3.2 [S]** `ZirGen` global-init hoisting, isolated in one function with the
      four-rule ordering written out verbatim. *Exit:* `--emit=zir` on
      `operations/unary.zz` and `operations/binary.zz` shows the hoisted
      statements in exact source order at the head of `@main`.

## Wave 4 — first consumer (LLVM) + pass pipeline

LLVM goes first: it is the cheap oracle. `llc` on Linux CI validates the whole IR
design before a single line of x86 instruction selection is written, and it is
what stands in for the ZIR interpreter this rewrite deliberately skips.

- [x] **4.1 [V]** `LlvmBackend`: ZIR → textual `.ll`, behind `--zir-codegen`.
      *Exit:* `TARGET=llvm pytest -q` green (40/40) with the flag; goldens
      untouched.
- [x] **4.2 [V]** Flip the default; delete `CodeGenLLVM.cpp`. *Exit:* green with
      no flag.
- [x] **4.3 [S]** `PassManager` + `AnalysisManager` + `DominatorTree`. `-O0` =
      lower + verify only. *Exit:* `-O0` output identical to 4.2's.
- [x] **4.4 [S]** `-O1`: `mem2reg`, `constfold`, `simplifycfg`, `dce`. Each pass
      lands with its own `tests/zir/<pass>/` unit tests; a pass returns `true`
      **only** when it changed something (the manager iterates to fixpoint — an
      always-`true` pass hangs the compiler). *Exit:* `TARGET=llvm` green at
      `-O0` and `-O1`; verifier clean after every pass on all 40.
- [x] **4.5 [S]** `-O2` (+`sccp`, `gvn`, `instcombine`, `tailcall`) and `-O3`
      (+`inline`, `licm`, loop unroll). *Exit:* `TARGET=llvm` green at all four
      levels.

## Wave 5 — shared x86 machine layer (all [S]; can start once Wave 3 is stable)

- [x] **5.1** `MachineFunction`/`MachineInst`/`MachineOperand` + virtual registers.
- [x] **5.2** `X86InstSel`: ZIR → `MachineInst` on virtual registers, argument/
      return placement parameterized by `TargetABI`.
- [x] **5.3** `LiveIntervals` over the linearized function. Scoped to
      block-local live ranges (see include/codegen/machine/LiveIntervals.hpp) --
      exactly what -O0 ZirGen output has (every local is memory, reloaded fresh
      per use; nothing crosses a block boundary as a bare SSA value). Correct
      and sufficient for Wave 6.2's -O0 exit criterion; raising past -O0
      (Wave 6.4) needs real live-in/live-out dataflow instead, once mem2reg's
      cross-block merges are in the picture.
- [x] **5.4** `LinearScan` with spilling and explicit callee-saved handling --
      the allocatable pool (`TargetABI::allocatableGpr/Xmm`) *is* exactly the
      callee-saved registers (see Win64Abi.cpp), so every vreg lands somewhere
      call-safe unconditionally, without needing a separate `crossesCall` check.
      Live-range splitting and move coalescing not attempted (immaterial at -O0,
      where nothing is being coalesced away regardless; a real loss once passes
      run ahead of native codegen).
- [x] **5.5** `FrameLayout` — the frame computed **once**, post-allocation
      (locals + spills + callee-saves + shadow space), replacing "reserve during
      emission". Stack canary/`-fstack-protector` not carried over from the
      legacy backends -- not attempted this wave.
- [x] **5.6** `AsmWriterAtt` and `AsmWriterIntel`.

*Exit for Wave 5 collectively:* one trivial function (`fn f(x:int64_t)->int64_t
{ return x+1; }`) emits correct AT&T and Intel text and links on both OSes.

## Wave 6 — the native targets (the Linux/Windows split point)

- [x] **6.1 [L]** `SysVAbi.cpp` — the `TargetABI` value for SysV: 6 int arg regs
      (`rdi rsi rdx rcx r8 r9`), independent GPR/XMM argument counters, 8 XMM arg
      regs, callee-saved `rbx r12-r15`, 128-byte red zone, no shadow space,
      variadic rule = `al` holds the vector-arg count, AT&T syntax. Shares every
      line of the Wave 5 machine layer with `Win64Abi`; the only shared-layer
      code the two ABIs' difference touched was `X86InstSel`'s argument slot
      assignment (now `placeArgs()` behind `sharedArgSlots`) and three AT&T
      writer fixes (`lea` operand order, `movz/movs` width-in-mnemonic
      spelling). SysV has no callee-saved XMM, so the allocatable XMM pool is
      caller-saved `xmm8-xmm13` — sound only because `-O0`/mem2reg + block-local
      live ranges never leave a bare float SSA value across a call (documented
      in `SysVAbi.cpp`). *Exit:* `TARGET=linux pytest -q` green (40/40) at `-O0`.
- [x] **6.2 [W]** `Win64Abi.cpp` — the `TargetABI` value for Win64: 4 int arg regs
      (`rcx rdx r8 r9`), **shared** GPR/XMM argument slots (slot N is one or the
      other, never both), callee-saved `rbx rdi rsi r12-r15` + `xmm6-xmm15`,
      32-byte shadow space, no red zone, variadic rule = duplicate float args
      into the paired GPR, Intel/MASM syntax. **Highest-risk item in this whole
      plan** — see Risks below. *Exit:* `TARGET=windows pytest -q` green (40/40)
      at `-O0`. Registered as the default `x86_64-mswin` backend at every
      optimization level (see 6.4 for how `-O1`+ was made sound).
- [x] **6.3 [L]** Raise Linux through `-O1`/`-O2`/`-O3`. No Linux-specific work
      was needed: Wave 6.4's target-neutral `computeCrossBlockValues()` /
      dedicated-slot handling in `X86InstSel` already makes mem2reg's
      cross-block values sound for any backend. *Exit:* `TARGET=linux pytest -q`
      green (40/40) at `-O0`, `-O1`, `-O2`, and `-O3`.
- [x] **6.4 [W]** Raise Windows through `-O1`/`-O2`/`-O3`. `LiveIntervals`/
      `LinearScan` (5.3/5.4) are still deliberately scoped to block-local live
      ranges — that didn't change — but `-O1`'s `mem2reg` breaks the assumption
      that made it sound (every -O0 local reloads fresh from memory, so no
      bare SSA value ever crossed a block boundary) in two ways, not just the
      one first suspected: an explicit merge at a block parameter (defined in
      one block, used in another), *and* — far more commonly — a plain
      dominance-only cross-block use with no merge at all, e.g. a function
      parameter that's never reassigned, read from a later block it simply
      dominates. Fix: `X86InstSel::computeCrossBlockValues()` is a whole-
      function pre-pass computing every value's defining block and flagging
      any value read outside it; every such value (plus every non-entry block
      parameter, unconditionally) gets a dedicated frame slot instead of ever
      needing a vreg live across a block boundary — written once right after
      its definition (or, for a block parameter, by every predecessor edge
      that branches to it, via `storeBranchArgs`/`edgeLabel`'s trampoline
      blocks), and reloaded fresh on every read from a different block. A
      read from the *same* block that defines the value still uses its live
      vreg directly (cheaper, and still sound since per-block LinearScan never
      sees it cross a boundary). This is a deliberate trade — mem2reg's
      register-residency win is given up for exactly the values that need to
      survive a block boundary, in exchange for never touching
      LiveIntervals/LinearScan's block-local scope at all. Root-caused via a
      real miscompile (not guessed): `functions/recursive.zz`'s `is_even`
      parameter `n`, read in two blocks with no merge involved, got corrupted
      by each block's independent register allocation reusing the same
      physical register inconsistently. *Exit:* `TARGET=windows pytest -q`
      green (40/40) at `-O0`, `-O1`, `-O2`, and `-O3`, verified on both MinGW
      g++ and MSVC 19.50.

6.1 and 6.2 share every line of Wave 5 and differ only in the `TargetABI` value
and the writer choice — this is the natural hand-off point. The Windows owner
should also make sure the new backend does not reproduce these existing
Windows-only bugs (none golden-visible today, all confirmed by reading
`CodeGenWindows.cpp`): a `movdqu` 16-byte store where an 8-byte float store was
intended when a function has 2+ float params; a stack-argument load that stores
into the caller's slot instead of reading from it; an invalid register-name
construction on a 32-to-64 unsigned cast; and always using 64-bit `cqo`/`idiv`
even for a 32-bit signed division.

## Wave 7 — cleanup

- [~] **7.1 [S]** Deleted `CodeGenLinux.cpp`, `CodeGenWindows.cpp`, `CodeGen.hpp`,
      `CodeGen.cpp`, `RegisterAllocator.*`, and the now-orphaned `Canaries.hpp`;
      dropped the `codegen/CodeGen.hpp` include from `RegisterBackends.cpp`.
      `test_runner.py` / `generate_expected_outputs.py` were already gone.
      **Still to do:** the `FunctionScope` frame API (`allocateStack`,
      `allocateSpillSlot`, `getStackOffset`, `getSpillSize`, `freeSpillSlot`)
      and `NameMapper` / `GLOBAL_NAME_MAPPER` — these are still called from
      `parser/ScopeContext.cpp` and are entangled with parse-time name
      resolution, so removing them is its own change with `compile_fail`
      surface risk. The scope *tree* (`FunctionScope`,
      `findEnclosingFunctionScope`) stays regardless — Sema and the parser use
      it for lexical scoping, not frame layout.
- [x] **7.2 [S]** Added an `OPT` axis to `conftest.py` (`OPT=0,1,2,3`, all four
      by default), parametrized alongside `TARGET` for `test_runtime` /
      `test_runtime_fail` — full matrix is 40 × 3 × 4 = 480 cells, run by
      default. `compile_fail` stays single-run (diagnostics are opt-independent).
- [ ] **7.3 [S]** CI gate: the constraint-5 grep, plus a check that
      `git diff --stat -- tests/ ':!tests/zir' ':!tests/conftest.py'
      ':!tests/test_pipeline.py'` is empty against the branch point.
- [x] **7.4 [S]** `docs/ARCHITECTURE.md` gets a Wave-6-complete status banner
      above the now-historical "original pipeline" diagram; M3/M4/M5 ticked in
      `docs/ROADMAP.md` with a done/not-done breakdown each; `docs/CONVENTIONS.md`
      updated to point ABI data at `TargetABI` values and note `RegisterAllocator`
      is deleted. `docs/IR-DESIGN.md`'s pass table was already consistent
      (`inline`/`licm` at `-O3`) — no change needed.

## Optimization levels

| Level | Pipeline |
|---|---|
| `-O0` *(default)* | lowering + verifier only |
| `-O1` | `mem2reg`, `constfold`, `simplifycfg`, `dce` |
| `-O2` | `-O1` + `sccp`, `gvn`, `instcombine`, `tailcall`, then a second cleanup round |
| `-O3` | `-O2` + `inline`, `licm`, loop unroll, more fixpoint iterations |

`-Os` is out of scope.

## Risks, ranked

1. **Sema-over-bodies breaks compilation of ~30/40 tests** if landed with literal
   textbook rules instead of the exemptions above — see "The landmine".
2. **Win64 ABI (6.2)** — shared arg slots, shadow space, the wider callee-saved
   set, variadic float duplication. Only exercised in CI; most likely place for
   the Windows matrix to go red.
3. **Global-hoisting order (3.2)** drifting from the four-rule reproduction —
   silent wrong stdout on `operations/unary.zz` / `operations/binary.zz`.
4. **`promoteType`'s float/int-width and same-width-signedness rules** inverted —
   silent wrong stdout on `mixed_types` / `binary`.
5. **Linear-scan leaving a value live across a call in a caller-saved
   register** — corruption that passes some tests and not others. Mitigated by
   5.4's explicit `crossesCall` exit criterion.
6. **`&&`/`||` short-circuit drift** — invisible to the current suite; mitigated
   by a deliberately-added pinning case.
7. **A pass changing observable behavior** (overflow, div-by-zero, float
   precision) — fold using the same width/signedness the target uses at runtime.
8. **Deleting the legacy backends (7.1) before parity is proven** — sequenced
   after 4.2, 6.1, and 6.2 are all green, specifically to avoid this.

## Verification

```bash
cmake -S . -B build -DCMAKE_BUILD_TYPE=RelWithDebInfo -DZUST_BUILD_TESTS=ON && cmake --build build -j
TARGET=linux,llvm python3 -m pytest -q && ctest --test-dir build
TARGET=windows python -m pytest -q
git status --porcelain tests/ | grep -q . && echo "CONSTRAINT VIOLATED" || echo "tests untouched"
grep -rn "CodegenOutputFormat\|TargetTriple" src/parser src/support src/zir include/parser include/zir
```

Once Wave 7.2 lands, the full 480-cell matrix is the default `pytest -q` run.
Bisect a suspected pass bug with `--disable-pass=<name>`; dump IR with
`--print-zir-after=<pass>`.
