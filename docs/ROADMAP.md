# zust Roadmap

Goal: a full-fledged, performant, statically typed compiled language with a
retargetable optimizing compiler. Linux x86_64 and Windows x86_64 are both
first-class at every milestone.

Milestones are ordered by dependency, not by calendar. Each has an **exit
criterion** — a checkable statement, not a vibe. Do not start a milestone until
the previous one's exit criterion holds.

---

## M0 — Stabilize the ground (blocking everything)

The tree currently has committed build artifacts and a disabled language feature.
Fix that before building on it.

- **M0-1 — Restore block scoping. ✅ DONE.** The parser did push a `BlockScope`
  correctly; the fault was in `FunctionScope::allocateStack`, which recorded the
  new slot in the *function's* `offsetTable_` keyed by the bare variable name. A
  block-scoped `let` therefore clobbered the enclosing variable's entry, and the
  outer name resolved to the inner slot for the rest of the function. Since
  `defineVariable` already records the offset in the defining scope, the extra
  write was both redundant and destructive; removing it fixed shadowing on all
  backends. Regression test: `tests/runtime/variables/block_shadowing.zz`.
- **M0-2 — Purge generated files from git.** `program`, `out.o`, `out.asm`,
  `test.asm`, `build/`, `__pycache__/`, `.pytest_cache/` are tracked or partially
  tracked. Remove, extend `.gitignore`.
- **M0-3 — Split CMake into `zust_core` (static lib) + `zpiler` (thin CLI).**
  Required so unit tests can link the compiler without spawning a process.
- **M0-4 — Unit test target.** Catch2 or doctest via `FetchContent`, guarded by
  `-DZUST_BUILD_TESTS=ON`. First tests: lexer token stream, `promoteType` table,
  `ScopeContext` lookup/shadowing.
- **M0-5 — CI hardening.** Add a debug + ASan/UBSan Linux job, cache correctly
  (the current cache key hashes `src/**` but restores a stale `build/`), and run
  `clang-format --dry-run -Werror` on changed files.
- **M0-6 — Delete `test_runner.py` and `generate_expected_outputs.py`** once
  pytest fully covers them; two test harnesses is one too many.
- **M0-7 — Fix the LLVM/native backend divergence. ✅ DONE.** The 5 divergent
  cases turned out to be three distinct bugs, and in every one the LLVM backend
  was right and the native backends were wrong:
  1. `variables/shadowing` — the M0-1 stack-slot clobber above.
  2. `conditionals/boolean_logic`, `conditionals/complex_bool`,
     `loops/nested_control` — `CodeGenLLVM` simply did not implement `&&`/`||`
     and threw "Unsupported integer op". Now lowered to `and`/`or`, matching the
     native backends' non-short-circuiting semantics.
  3. `operations/mixed_types` — **both** native backends emitted the *signed*
     setcc forms after `ucomis{s,d}`. That instruction reports its result in
     CF/ZF and clears OF/SF, so `setl` was unsatisfiable and `setg` degraded into
     a bare "operands differ" test: every float `<` was false and every float `>`
     was true for unequal operands. Fixed in `CodeGenLinux` and `CodeGenWindows`
     by using the unsigned forms. Regression test:
     `tests/runtime/operations/float_comparisons.zz`.

  Two goldens were corrected as part of this — they had enshrined the buggy
  native output (`shadowing.stdout` last line `20`→`10`, `mixed_types.stdout`
  gained the missing second `1`).

  **Still open:** `ucomis{s,d}` sets ZF=CF=PF=1 for unordered (NaN) operands, so
  `<` and `<=` currently return true against a NaN. Correct handling needs a `PF`
  check. Not reachable today — the language has no NaN literal and no way to
  produce one — but it must be fixed before floating-point division by zero or
  `0.0/0.0` is expressible.

**Exit:** `TARGET=linux,llvm pytest -q` and `TARGET=windows pytest -q` green on a
clean checkout, shadowing test asserts real block scoping, `git status` clean
after a full build.

*Status: `TARGET=linux,llvm` is green (80/80) and shadowing is genuinely tested.
`TARGET=windows` is unverified locally — the FP-comparison fix in
`CodeGenWindows` is confirmed only at the emission level (it now emits `seta`/
`setb`/`setae`/`setbe` rather than `setg`/`setl`) and needs CI to run it.*

---

## M1 — Frontend hardening: spans and real diagnostics

Everything downstream needs source locations. Adding them later means touching
every node again.

- `struct Span { uint32_t start, end; }` over a `SourceFile` with a line-index
  table. Tokens carry a `Span`; every `ASTNode` carries a `Span`.
- `DiagnosticEngine`: severity, code (`E0001`), primary span with a caret, labeled
  secondary spans, notes, and a hint. Rendered with the existing `Colors.hpp`.
- Parser **error recovery**: synchronize on `;` and `}` so one bad statement does
  not cascade into fifty errors.
- Lexer completeness: `0x`/`0b`/`0o` literals, digit separators (`1_000_000`),
  char literals, escapes (`\n \t \\ \" \0 \xNN \u{...}`), block comments with
  nesting, raw strings.
- `--emit=tokens`, `--emit=ast` (stable, testable text format), `--json-diagnostics`
  for editors.

**Exit:** every diagnostic prints file:line:col with a caret under the offending
span; a file with 5 independent syntax errors reports all 5; a `tests/diagnostics/`
suite golden-tests the rendered output.

---

## M2 — Sema: split resolution from type checking

- **Resolver pass.** Names → `SymbolId` (a `uint32` index into a symbol table).
  Kills the process-global `NameMapper` and the string-keyed `variable_name_mappings`.
  Handles shadowing correctly by construction: a new `let` makes a new `SymbolId`.
- **Type table.** Interned `TypeId` with a proper tagged representation
  (`Int{bits,signed} | Float{bits} | Bool | Ptr(TypeId) | Array(TypeId,N) |
  Slice(TypeId) | Struct(StructId) | Fn(sig)`), replacing the flat boolean bag in
  `TypeInfo`. Structural equality via interning, so type comparison is a `==` on ints.
- **Explicit conversion rules.** Write down the promotion/coercion lattice
  (`docs/TYPES.md`), then make `promoteType` implement exactly that. Today's
  implicit narrowing behavior must become an error or an explicit `as` cast.
- **Definite-return analysis** (the existing `// TODO: Check that all paths inside
  the function return appropriate value.`) and **definite-initialization** analysis.
- **Const evaluation** for array lengths and constant initializers.
- Two-pass declaration collection so functions and globals can be used before
  their textual definition.

**Exit:** parser no longer touches `ScopeContext`; the AST after Sema has a
`SymbolId` on every reference and a `TypeId` on every expression; a function
missing a return on one path is a compile error.

---

## M2.5 — Pluggable backends (AST-level)

Full plan: `docs/BACKENDS.md`. Done **before** ZIR, so that when the IR lands the
plug point moves by a signature change rather than a re-plumb.

Adding a backend today costs 13 edits across 9 files in 4 layers, because target
knowledge has leaked out of codegen: `ScopeContext::allocateSpillSlot` and
`RegisterAllocator::emitSpillRestore` both format target-specific assembly, and
two parallel enums (`CodegenOutputFormat`, `TargetTriple`) are mapped by hand in
`main.cpp`.

- **M2.5-1 — Evict target knowledge from non-codegen layers.** `allocateSpillSlot`
  returns an integer offset; `RegisterAllocator` stops emitting assembly. Pure
  refactor, goldens unchanged. Everything else depends on this.
- **M2.5-2 — One target identity.** Delete `TargetTriple`; a target is its name
  string, which is also the `--format` value.
- **M2.5-3 — `Backend` interface + `BackendRegistry`**, with explicit
  `registerBuiltinBackends()` (not static-initializer self-registration — the
  linker drops unreferenced objects out of a static library and the backend
  silently vanishes in release builds).
- **M2.5-4 — `--formats --json`, consumed by the test harness.** `tests/conftest.py`
  stops hardcoding `TARGETS` and `test_pipeline.py` stops branching on target name;
  both read the registry, so driver and tests cannot disagree about how to build.
- **M2.5-5 — Split `CodeGen.hpp`.** Each backend's class declaration moves next to
  its implementation and out of the header that reaches every translation unit.
- **M2.5-6 — Prove it: add a C backend** (`--format c`). It shares nothing with the
  existing three — no registers, no frames, no assembler — so it is the real test
  of whether the registry needs special cases.

**Exit:** adding a backend touches one new directory plus one line in
`registerBuiltinBackends`. `grep -rn "CodegenOutputFormat\|TargetTriple" src/parser
src/support` is empty, `--formats` is generated from the registry (it currently
hand-maintains a list that omitted `llvm-ir` for that backend's entire existence),
and the C backend passes the golden suite.

---

## M3 — ZIR: the intermediate representation

The centerpiece. Spec lives in `docs/IR-DESIGN.md`. Tracked implementation
work (spans M3/M4/M5 together — stage isolation, ZIR, and -O0..-O3 land as one
piece of work) lives in `docs/PRD-ZIR.md`; check it for current progress before
starting related work.

- Typed SSA: `Module → Function → BasicBlock → Instruction`, block arguments
  instead of phi nodes, `alloca`/`load`/`store` for locals (SSA-ified later by
  mem2reg).
- Arena allocation with `ValueId`/`BlockId`/`InstId` indices — no pointer chasing,
  cheap cloning, trivially serializable.
- **AST → ZIR lowering** replacing all direct AST→asm lowering.
- **Verifier**: type agreement on every operand, dominance (every use dominated by
  its def), terminator-per-block, no unreachable-but-referenced blocks. Runs after
  every pass under `-DZUST_ASSERTIONS`.
- **Textual form**, parseable and printable, so IR is golden-testable and passes
  can be unit-tested from `.zir` files.
- **ZIR interpreter** — an execution oracle independent of any backend. This is
  what makes differential testing possible.

**Exit:** every existing `tests/runtime` case compiles through ZIR and produces
byte-identical output on all three backends; `zpiler --emit=zir` round-trips
through the parser; the interpreter agrees with native output on every case.

---

## M4 — Optimization passes

All in ZIR. This is why the IR exists.

Ordered by value-per-effort:

1. **mem2reg / SROA** — promote `alloca`s to SSA values. Single biggest win;
   everything else depends on it.
2. **Constant folding + SCCP** (sparse conditional constant propagation).
3. **DCE / ADCE** and dead-store elimination.
4. **simplify-cfg** — merge blocks, fold branches on constants, remove empty blocks.
5. **GVN / CSE** — redundant expression elimination.
6. **Inlining** with a cost model (size, call-site count, recursion depth).
7. **LICM** — hoist loop-invariant code; requires a loop-info analysis.
8. **Strength reduction / induction-variable simplification**.
9. **Tail-call elimination** — makes recursive `.zz` code competitive.
10. **Peephole / instcombine** — algebraic identities.

Infrastructure this needs: dominator tree, loop info, use-def chains (intrusive
use lists), alias analysis (start with type-based + "distinct allocas don't alias").

Driver: `-O0 -O1 -O2 -O3 -Os`, `--print-zir-after=<pass>`, `--print-zir-after-all`,
`--disable-pass=<name>`, and a **pass-fuzzing** mode that runs random pass orders
and checks the interpreter still agrees.

**Exit:** `-O2` beats `-O0` by >2x on the benchmark suite; every pass has unit
tests over `.zir` inputs; verifier clean after each pass on the whole test suite.

---

## M5 — Backends off ZIR

- **LLVM backend rewrite**: ZIR → LLVM IR is mostly 1:1 once ZIR is SSA. Should
  shrink `CodeGenLLVM.cpp` substantially. Optionally move from textual `.ll` to
  the LLVM C++ API behind `-DZUST_USE_LLVM=ON`, keeping textual emission as the
  no-dependency default.
- **Native backend unification**: one `X86Backend` parameterized by a
  `TargetABI` struct (arg registers, callee-saved set, shadow space, red zone,
  syntax flavor, variadic float convention). Linux and Windows become data, not
  duplicated code. Deletes ~1500 lines of divergence.
- **Instruction selection** over ZIR (tree tiling; `a*8+b` → one `lea`).
- **Linear-scan register allocation** on live intervals, replacing the LRU
  allocator. Includes proper live-range splitting and coalescing of moves.
- **Frame layout**: compute the frame once from the allocation result, instead of
  reserving during emission. Keeps the stack canary; makes it optional per `-fstack-protector`.
- **Peephole** on machine instructions (redundant mov, `xor` for zeroing,
  `test` for compare-to-zero).

Backends also move their plug point from the AST to ZIR here — `Backend::emit`
takes a `const zir::Module &` instead of a `unique_ptr<ASTNode>`. Because M2.5
already built the registry, the CLI, driver, and test harness are untouched; see
`docs/BACKENDS.md` §3. Taking the module by const reference (rather than consuming
it, as the AST path does) is what allows one run to emit several targets and a
backend to make more than one pass.

**Exit:** `CodeGenLinux.cpp` and `CodeGenWindows.cpp` are gone, replaced by a
shared backend + two `TargetABI` descriptions; generated code has no redundant
spill/reload for straight-line arithmetic; all three backends pass identical
goldens. Proof of the abstraction: an AArch64 Linux backend is added as
predominantly a data exercise.

---

## M6 — The language becomes full-fledged

Sequenced so each rests on the last. All need Sema (M2) and ZIR (M3) in place.

1. **Pointers & references** — `*T`, `&x`, `*p`, null safety story decided.
2. **Arrays & slices** — `[T; N]`, `[]T` with a length, bounds checking (elidable
   by the optimizer, disableable with a flag).
3. **Structs** — field layout, alignment/padding rules, by-value vs by-reference
   passing per ABI (the classification rules differ sharply between SysV and Win64 —
   this is the single hardest ABI item in the project).
4. **Methods & `impl` blocks.**
5. **Enums + exhaustive `match`** — tagged unions, decision-tree lowering,
   exhaustiveness checking in Sema.
6. **Generics via monomorphization** — instantiate at ZIR level, then let existing
   passes optimize each instance.
7. **Traits / interfaces** — static dispatch first; vtables only if needed.
8. **Modules & imports** — file-based, with a real namespace/visibility model.
   Separate compilation to `.zirobj` for incremental builds.
9. **Closures / first-class functions** — environment capture, lowering to a
   struct + function pointer.
10. **Error handling** — a decided model (`Result`-style values, no unwinding)
    rather than the ad-hoc exits used today.

**Exit:** the language can express its own test suite without `extern printf` —
that is, a self-hosted-ish standard library is writable in it.

---

## M7 — Runtime & standard library

- `libzust` runtime: entry shim, panic/abort with a message and backtrace hook,
  bounds-check failure handler, stack-overflow guard page.
- Allocator interface + a default bump/free-list allocator.
- Core library: `String`/`str`, `Vec<T>`, `Option<T>`, `Result<T,E>`, formatted
  output that does not shell out to libc `printf`, math, file I/O.
- Platform abstraction so the same stdlib source builds against both syscalls and
  Win32.

**Exit:** a nontrivial program (JSON parser, or the zust lexer itself) compiles and
runs with zero `extern` declarations on both OSes.

---

## M8 — Toolchain & developer experience

- **Driver does the whole job**: `zpiler build prog.zz -o prog` invokes the
  assembler and linker itself (`as`/`ld`/`lld` on Linux, `ml64`/`link.exe`/`lld-link`
  on Windows) so users never run three commands.
- `--emit=tokens|ast|zir|llvm|asm|obj|exe`, `-c`, `-S`, `--target=<triple>`.
- **Debug info**: DWARF on Linux, CodeView on Windows. Line tables first, then
  variable locations. Verify with a `gdb`/`lldb` batch script in CI.
- **Cross compilation**: build a Windows binary on Linux and vice versa. Requires
  the ABI-as-data work from M5.
- **LSP server** — diagnostics, hover types, goto-definition, completion, reusing
  Sema. Plus a TextMate/tree-sitter grammar for editors.
- **Formatter** (`zust fmt`) and a package/build manifest.
- **Playground**: WASM build of the compiler for a browser demo.

**Exit:** `zpiler build hello.zz && ./hello` works on a clean machine with only a
linker installed; VS Code shows live diagnostics and hover types.

---

## M9 — Performance & correctness at scale

Continuous from M4 onward, formalized here.

- **Benchmark suite** (`bench/`): nbody, binary-trees, fannkuch, spectral-norm,
  mandelbrot, a string-heavy workload, and a compile-time benchmark. Tracked in CI
  with a regression threshold; results plotted per commit.
- **Baseline targets**: within 2x of `gcc -O2` on scalar numeric benchmarks;
  compiler front end processes >100k lines/sec.
- **Differential testing**: random program generator (csmith-style, restricted to
  the defined subset) comparing ZIR interpreter vs all three backends. Any
  disagreement is a bug in exactly one of them.
- **Fuzzing**: libFuzzer over lexer/parser/Sema for crashes and hangs; the
  compiler must never abort on malformed input.
- **Sanitizers in CI**: ASan + UBSan on the compiler, and on the *generated*
  programs where the backend supports it.
- **Compile-time budget**: track and cap per-pass time; `--time-passes`.

**Exit:** a week of continuous fuzzing finds no crashes; benchmark dashboard is
green with no unexplained regressions.

---

## Dependency graph

```
M0 ──▶ M1 ──▶ M2 ──▶ M2.5 ──▶ M3 ──▶ M4 ──▶ M5
                       │       │      │      │
                       │       └──────┴──────┴──▶ M6 ──▶ M7 ──▶ M8
                       │                            │
   backend registry ───┘                            └──▶ M9 (continuous from M4)
   (reused unchanged by M5's move to IR-level plugging)
```

## Things deliberately not on this list

- Garbage collection. The language is manual/ownership-based; revisit only with a
  concrete motivating use case.
- Self-hosting the compiler in zust. Attractive, but it locks the language design
  early; consider only after M7.
- Non-x86 targets (RISC-V, WASM) as *products*. The M2.5 registry and the M5
  ABI-as-data work make them cheap; adding them before that would multiply the
  divergence problem rather than prove anything. The two backends that *are*
  scheduled — a C backend at M2.5 and AArch64 at M5 — exist to verify the
  abstractions, not to be shipped targets.
