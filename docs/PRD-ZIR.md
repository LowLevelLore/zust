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

- [ ] **`&&` / `||` are non-short-circuiting.** All three backends lower them to a
      plain bitwise `and`/`or` on the promoted type, evaluating both operands
      unconditionally — `CodeGenLinux.cpp:349`, `CodeGenWindows.cpp:404`,
      `CodeGenLLVM.cpp:316`. No existing test has a side-effecting RHS, so a
      short-circuiting `condbr` lowering would pass the whole suite and still be
      a silent language change. Lower to `and`/`or` on `i1` deliberately; add a
      new `tests/zir/` case with an effectful RHS.
- [ ] **Global hoisting into `main`** — `CodeGenLLVM.cpp:700-744`, mirrored in
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
- [ ] **`promoteType` lattice** — reproduce `TypeChecker.hpp:28-77` exactly,
      including the easy-to-miss rule that **an int wider than a float promotes to
      a float of the int's width** (`float32 + int64 → double`, not `float32`), and
      that two same-width ints of differing signedness promote to the **signed**
      one. `mixed_types.stdout` and `binary.stdout` depend on both.
- [ ] **Float comparisons use unsigned setcc after `ucomis{s,d}`** (ROADMAP M0-7).
      Do not regress. The known NaN gap (`<`/`<=` true against NaN on natives,
      false via LLVM's ordered predicates) is unreachable today — decide the ZIR
      `fcmp` semantics deliberately rather than let it fall out of whichever
      backend is written first.
- [ ] **Variadic calls** — SysV sets `al` to the vector-arg count; Win64 duplicates
      float args into the paired GPR at the same index. Every one of the 40 cases
      calls `printf`, so this is on the critical path everywhere.
- [ ] **Boolean representation.** Decide once: **`i1` in ZIR, 1 byte in memory**,
      with explicit `zext`/`trunc` at every load/store boundary, and lower every
      condition as `icmp ne <ty> %v, 0` (matches the natives' "test the whole
      register" behavior and is a no-op fix for LLVM's current bit-0-only `trunc`
      divergence).
- [ ] **Float literals stored as IEEE bits**, not re-derived from decimal text at
      print time — three different current pipelines (raw source text on Linux,
      decoded on Windows, `stof`/`stod` + 17-sig-digit reprint on LLVM) agree only
      by accident. Store the bit pattern in the ZIR constant and emit from bits on
      every backend to remove this as a source of divergence.
- [ ] **`break`/`continue`** — `continue` in a `for` jumps to the post-statement
      (still runs the increment); in a `while` it jumps to the condition. Both
      throw if used outside a loop (also rejected by the parser at parse time).
- [ ] **Missing return** — currently unchecked and currently produces a function
      with no trailing `ret` at all (execution falls into whatever is emitted
      next). No existing test exercises this. The ZIR verifier will force a
      decision (every block needs exactly one terminator) — pick `unreachable`
      to match today's undefined behavior, not a synthesized `ret 0`, and record
      the choice with a `tests/zir/` case.
- [ ] **Integer widening picks `zext` vs `sext` by the *source* type's
      signedness**, matching `castValue`'s existing split — getting this backwards
      at a variadic boundary turns a wrapped `uint8_t` value like `255` into `-1`.
- [ ] **DCE must treat calls as side-effecting**, or `-O1` deletes the `printf`
      every golden depends on.
- [ ] The three `tests/expected/runtime/variables.{linux,llvm,windows}.stdout`
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

- [ ] **1.1 [S]** `TypeTable` with interning + `TargetLayout` (sizes/alignment
      come from the layout, never hardcoded on the type — `docs/IR-DESIGN.md`
      calls out today's hardcoded `size_t = 64` as the mistake not to repeat).
      *Exit:* unit test — structurally identical types intern to equal `TypeId`.
- [ ] **1.2 [S]** `Module`/`Function`/`BasicBlock`/`Instruction` arenas +
      `Builder`. `ValueId`/`BlockId`/`InstId`/`TypeId` are `uint32` indices; block
      arguments, not phi nodes. *Exit:* a hand-built module round-trips.
- [ ] **1.3 [S]** `Printer` — textual form per `docs/IR-DESIGN.md`. *Exit:* the
      spec's `@factorial` example prints byte-for-byte.
- [ ] **1.4 [S]** `.zir` `TextParser`. *Exit:* print→parse→print is a fixed point
      on every fixture in `tests/zir/roundtrip/`.
- [ ] **1.5 [S]** `Verifier`, all 8 checks from `docs/IR-DESIGN.md`. *Exit:* eight
      hand-broken `.zir` fixtures each trip exactly their intended check.

## Wave 2 — Sema (parallel with Wave 1)

- [ ] **2.1 [S]** Resolver: names → `SymbolId`. Parser stops calling
      `ScopeContext::defineVariable → allocateStack` — no more frame slots at
      parse time. *Exit:* every reference has a `SymbolId`; legacy backends keep
      working via a temporary `SymbolId → offset` shim.
- [ ] **2.2 [S]** `TypeRules::promote` ported bit-for-bit from
      `TypeChecker::promoteType`, over `TypeId`. *Exit:* a generated 13×13 table
      test comparing the new function against the old one before the old one is
      deleted.
- [ ] **2.3 [S] — the landmine item.** Sema over function bodies, with the
      exemptions above. Land alone, on the legacy backends, before ZIR consumes
      anything. *Exit:* all 40 cases still compile unchanged.
- [ ] **2.4 [S]** Definite-return analysis, **warning-only** in this rewrite (an
      error here would change `compile_fail` surface, which is frozen). *Exit:*
      reports nothing on the current suite.

## Wave 3 — Lowering

- [ ] **3.1 [S]** `ZirGen` for expressions/statements. Every rule in the behavior
      inventory above lives here and nowhere else. Locals →
      `alloca`/`load`/`store`, no cleverness (mem2reg promotes later). *Exit:*
      all 40 cases lower and verify clean at `-O0`; `--emit=zir` round-trips.
- [ ] **3.2 [S]** `ZirGen` global-init hoisting, isolated in one function with the
      four-rule ordering written out verbatim. *Exit:* `--emit=zir` on
      `operations/unary.zz` and `operations/binary.zz` shows the hoisted
      statements in exact source order at the head of `@main`.

## Wave 4 — first consumer (LLVM) + pass pipeline

LLVM goes first: it is the cheap oracle. `llc` on Linux CI validates the whole IR
design before a single line of x86 instruction selection is written, and it is
what stands in for the ZIR interpreter this rewrite deliberately skips.

- [ ] **4.1 [V]** `LlvmBackend`: ZIR → textual `.ll`, behind `--zir-codegen`.
      *Exit:* `TARGET=llvm pytest -q` green (40/40) with the flag; goldens
      untouched.
- [ ] **4.2 [V]** Flip the default; delete `CodeGenLLVM.cpp`. *Exit:* green with
      no flag.
- [ ] **4.3 [S]** `PassManager` + `AnalysisManager` + `DominatorTree`. `-O0` =
      lower + verify only. *Exit:* `-O0` output identical to 4.2's.
- [ ] **4.4 [S]** `-O1`: `mem2reg`, `constfold`, `simplifycfg`, `dce`. Each pass
      lands with its own `tests/zir/<pass>/` unit tests; a pass returns `true`
      **only** when it changed something (the manager iterates to fixpoint — an
      always-`true` pass hangs the compiler). *Exit:* `TARGET=llvm` green at
      `-O0` and `-O1`; verifier clean after every pass on all 40.
- [ ] **4.5 [S]** `-O2` (+`sccp`, `gvn`, `instcombine`, `tailcall`) and `-O3`
      (+`inline`, `licm`, loop unroll). *Exit:* `TARGET=llvm` green at all four
      levels.

## Wave 5 — shared x86 machine layer (all [S]; can start once Wave 3 is stable)

- [ ] **5.1** `MachineFunction`/`MachineInst`/`MachineOperand` + virtual registers.
- [ ] **5.2** `X86InstSel`: ZIR → `MachineInst` on virtual registers, argument/
      return placement parameterized by `TargetABI`.
- [ ] **5.3** `LiveIntervals` over the linearized function.
- [ ] **5.4** `LinearScan` with spilling, live-range splitting, move coalescing,
      and explicit `crossesCall` handling — a value live across a call must land
      in a callee-saved register or be spilled, never left in a caller-saved one.
- [ ] **5.5** `FrameLayout` — the frame computed **once**, post-allocation
      (locals + spills + callee-saves + shadow space), replacing "reserve during
      emission". Stack canary preserved, gated by `-fstack-protector` (default on).
- [ ] **5.6** `AsmWriterAtt` and `AsmWriterIntel`.

*Exit for Wave 5 collectively:* one trivial function (`fn f(x:int64_t)->int64_t
{ return x+1; }`) emits correct AT&T and Intel text and links on both OSes.

## Wave 6 — the native targets (the Linux/Windows split point)

- [ ] **6.1 [L]** `SysVAbi.cpp` — the `TargetABI` value for SysV: 6 int arg regs
      (`rdi rsi rdx rcx r8 r9`), independent GPR/XMM argument counters, 8 XMM arg
      regs, callee-saved `rbx r12-r15`, 128-byte red zone, no shadow space,
      variadic rule = `al` holds the vector-arg count, AT&T syntax. *Exit:*
      `TARGET=linux pytest -q` green (40/40) at `-O0`.
- [ ] **6.2 [W]** `Win64Abi.cpp` — the `TargetABI` value for Win64: 4 int arg regs
      (`rcx rdx r8 r9`), **shared** GPR/XMM argument slots (slot N is one or the
      other, never both), callee-saved `rbx rdi rsi r12-r15` + `xmm6-xmm15`,
      32-byte shadow space, no red zone, variadic rule = duplicate float args
      into the paired GPR, Intel/MASM syntax. **Highest-risk item in this whole
      plan** — see Risks below. *Exit:* `TARGET=windows pytest -q` green (40/40)
      at `-O0`.
- [ ] **6.3 [L]** Raise Linux through `-O1`/`-O2`/`-O3`.
- [ ] **6.4 [W]** Raise Windows through `-O1`/`-O2`/`-O3`.

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

- [ ] **7.1 [S]** Delete `CodeGenLinux.cpp`, `CodeGenWindows.cpp`, `CodeGen.hpp`,
      `CodeGen.cpp`, `RegisterAllocator.*`, the `FunctionScope` frame API,
      `NameMapper`, `GLOBAL_NAME_MAPPER`, `test_runner.py`,
      `generate_expected_outputs.py` (ROADMAP M0-6).
- [ ] **7.2 [S]** Add an `OPT` axis to `conftest.py` (`OPT=0,1,2,3`), parametrized
      alongside `TARGET` — full matrix is 40 × 3 × 4 = 480 cells, run by default.
- [ ] **7.3 [S]** CI gate: the constraint-5 grep, plus a check that
      `git diff --stat -- tests/ ':!tests/zir' ':!tests/conftest.py'
      ':!tests/test_pipeline.py'` is empty against the branch point.
- [ ] **7.4 [S]** Refresh `docs/ARCHITECTURE.md`'s pipeline diagram and tick
      M3/M4/M5 in `docs/ROADMAP.md`. Reconcile `docs/IR-DESIGN.md`'s pass-manager
      table (it currently places `inline`/`licm` at `-O2`; this PRD places them
      at `-O3`) and `docs/CONVENTIONS.md` (still directs ABI data into
      `RegisterAllocator`, a file this plan deletes).

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
