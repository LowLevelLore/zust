# Optimization Guide

Two separate concerns, often confused:

1. **Generated-code performance** — how fast programs the compiler produces run.
2. **Compiler performance** — how fast `zpiler` itself is.

Both need measurement before change. A claimed speedup without a number is not a
speedup.

---

## Part 1 — Generated code

### Where the wins are, today

The current AST-walking backends leave large, easily measurable amounts on the
table. Inspect `out.asm` in the repo root for a representative sample:

```asm
    movq $50, %r11          # materialize constant into a register
    movq %r11, -24(%rbp)    # spill it immediately to the local's slot
    movq -24(%rbp), %r11    # reload it on the very next instruction
    movq %r11, %rax
```

Four instructions where one (`movq $50, %rax`) suffices. Every local is a memory
slot, every expression round-trips through the stack, and the LRU register
allocator has no notion of live ranges, so it cannot keep anything in a register
across a statement.

Concretely, the biggest wins in order:

| # | Optimization | Why it matters here | Milestone |
|---|--------------|--------------------|-----------|
| 1 | **mem2reg** | Every local is an `alloca`; promoting them is what makes all other passes effective. Expect the largest single improvement. | M4 |
| 2 | **Linear-scan regalloc** | Replaces the LRU spiller. Removes the store/reload pairs above. | M5 |
| 3 | **Constant folding + SCCP** | The compiler currently emits `movq $10, %r11; movq $20, %r10; addq…` for `10 + 20`. | M4 |
| 4 | **simplify-cfg** | Lowering emits many empty and fall-through-only blocks. | M4 |
| 5 | **Instruction selection** | `lea` for address arithmetic, `imul` by constant → shift/add, `test` instead of `cmp $0`. | M5 |
| 6 | **Inlining** | Small accessor functions dominate idiomatic code; also unlocks the passes above across call boundaries. | M4 |
| 7 | **Tail-call elimination** | The language encourages recursion (see `examples/functions.zz`); without TCO, recursive code is unnecessarily slow and stack-hungry. | M4 |
| 8 | **GVN/CSE** | Repeated subexpressions in array indexing and struct field access. | M4 |
| 9 | **LICM** | Loop-invariant address computation and bounds checks. | M4 |
| 10 | **Bounds-check elision** | Needed once slices land, or M6 array safety costs 2x. | M6 |

### Cheap wins available before ZIR exists

These do not require the IR and can land during M0/M1 if generated-code
performance is blocking a demo:

- **Stack canary should be opt-out.** Every function currently emits a `movabs` +
  store on entry and a load/compare/branch on exit, unconditionally. Gate it
  behind `-fstack-protector` (default on for now, off at `-O2`+ once we trust the
  code, and never in leaf functions with no arrays).
- **Don't push all callee-saved registers unconditionally.** The prologue pushes
  `rbx, r12–r15` plus an alignment dummy in *every* function, including leaf
  functions that use none of them. Emit only what the function actually clobbers.
  This is pure win and requires only a "which registers did I touch" set.
- **Immediate operands.** `movq $50, %r11; movq %r11, -24(%rbp)` should be
  `movq $50, -24(%rbp)`. A peephole over the emitted instruction list catches most
  of these.
- **Don't re-materialize identical string literals.** `out.asm` shows `.Lstr0`
  through `.Lstr5` all holding `"%d\n"`. Intern literal constants by content.

Do these as **peepholes over an emitted instruction list**, not as ad-hoc
special-cases inside expression emission — otherwise they will have to be deleted
at M5.

### Benchmarking

`bench/` (to be created, M9) holds programs with a fixed workload and a checksum
output so a wrong answer fails loudly:

- `nbody`, `spectral-norm`, `mandelbrot` — float-heavy
- `binary-trees` — allocation-heavy (needs M7)
- `fannkuch`, `fasta` — integer and array-heavy
- `fib`, `ackermann` — call-overhead and recursion
- `string-cat` — string workload

Harness reports, per benchmark and per backend, wall time over N runs (min, not
mean — min is the least noisy estimator for CPU-bound work), plus `gcc -O2` and
`clang -O2` equivalents as the reference line.

**Target:** within 2x of `gcc -O2` on scalar numeric code after M5.

Run benchmarks pinned (`taskset -c 2`), with the CPU governor at `performance`,
and never on a CI shared runner for absolute numbers — CI tracks *relative*
regression only, with a threshold wide enough to survive runner noise.

---

## Part 2 — The compiler itself

### Current known costs

- `ASTNode::value` is a `std::string` on every node, including nodes where it is
  empty. Interning and a `SymbolId`/`TypeId` scheme (M2) removes most of this.
- `ScopeContext` lookups are `unordered_map<std::string, …>` chains — a string
  hash per lookup, walked up the scope chain. `SymbolId` resolution (M2) makes
  these array indices.
- The lexer has no token buffer and `reset()` re-lexes from the start.
- `NameMapper` mangles names by string concatenation on every declaration.
- Codegen builds output in `std::ostringstream` and concatenates; fine for now,
  but should become a single output buffer with `reserve()`.

### Rules

- Measure with `perf record` / `perf report` (Linux) or a sampling profiler; do
  not guess. `--time-passes` (M4) gives per-phase attribution.
- Prefer index-based arenas over `unique_ptr` graphs for anything the optimizer
  touches repeatedly. This is why ZIR is arena-allocated.
- `reserve()` vectors whose size is known or estimable.
- Avoid `std::string` in hot paths; `std::string_view` or an interned id.
- Do not add a dependency to make the compiler faster without a benchmark showing
  it actually did.

**Target:** >100k lines/sec through the frontend (lex + parse + sema) on a modern
desktop core, at `-O0`.

### Compile-time budget

Track total compile time for the test suite in CI. A change that makes the
compiler >5% slower needs a justification in the commit message.
