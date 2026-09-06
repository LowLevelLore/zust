# ZIR — zust Intermediate Representation

Status: **implemented** (PRD-ZIR Waves 1–6). This document is the reference for
ZIR as it exists in the tree; `docs/ZIR-PASSES.md` documents the optimization
passes that run over it, and `docs/PRD-ZIR.md` tracks the migration itself.

The design principles below (typed SSA, block arguments, arena allocation,
locals-as-`alloca`) are unchanged from the original spec. Where the
implementation deliberately diverged or deferred something, it is called out
inline as **[divergence]** or **[deferred]**.

## Why

The three legacy backends shared zero lowering logic, so every language feature
cost three implementations and there was nowhere to put an optimization. ZIR is
the one place semantics get lowered and optimized; backends became emitters. As
of Wave 6 all three backends (`llvm-ir`, `x86_64-linux`, `x86_64-mswin`) consume
ZIR and the legacy `CodeGen*` emitters are gone.

## File map

| Path | Contents |
|------|----------|
| `include/zir/Ids.hpp` | `Id<Tag>` — the `uint32` index wrappers (`TypeId`, `ValueId`, `BlockId`, `InstId`, `FuncId`, `GlobalId`) |
| `include/zir/Types.hpp`, `src/zir/Types.cpp` | `Type`, `TypeTable` (interning), `TargetLayout` (sizes/alignment) |
| `include/zir/Instruction.hpp` | `Opcode`, `CmpPred`, `Instruction`, `Terminator`, `BlockRef`, `ConstValue` |
| `include/zir/Module.hpp` | `Module`, `Function`, `BasicBlock`, `GlobalVar` — the arenas |
| `include/zir/Builder.hpp` | `Builder` — the sanctioned way to create instructions |
| `include/zir/Printer.hpp`, `src/zir/Printer.cpp` | textual form (`--emit=zir`) |
| `include/zir/TextParser.hpp`, `src/zir/TextParser.cpp` | `.zir` reader (round-trip + pass unit tests) |
| `include/zir/Verifier.hpp`, `src/zir/Verifier.cpp` | the 8 structural checks |
| `include/zir/DominatorTree.hpp`, `src/zir/DominatorTree.cpp` | CFG + dominance analysis |
| `include/zir/Uses.hpp`, `src/zir/Uses.cpp` | per-`ValueId` use counts |
| `include/zir/TypeRules.hpp`, `src/zir/TypeRules.cpp` | `promote()` — the numeric promotion lattice, ported from `TypeChecker` |
| `include/zir/PassManager.hpp` | `Pass`, `ModulePass`, `PassManager`, `AnalysisManager` |
| `include/zir/passes/`, `src/zir/passes/` | the optimization passes + `buildPipeline` |
| `include/zirgen/ZirGen.hpp`, `src/zirgen/ZirGen.cpp` | AST → ZIR lowering |

## Shape

```
Module
 ├── sourceName / targetName
 ├── types:     TypeTable (interned, one per Module)
 ├── layout:    TargetLayout (pointer/size-type widths)
 ├── globals:   [GlobalVar]
 └── functions: [Function]              (extern decls live here too — see below)
                 ├── name / signature (a Fn TypeId) / isExtern / isVariadic
                 ├── entry: BlockId
                 ├── blocks:      [BasicBlock]
                 │                 ├── label
                 │                 ├── params: [ValueId]     (block arguments, not phis)
                 │                 ├── insts:  [InstId]       (indices into the Function's arena)
                 │                 └── term:   Terminator     (exactly one, always present)
                 ├── insts:       [Instruction]               (the arena the InstIds point into)
                 ├── valueTypes:  [TypeId]                     (type of every ValueId this fn defines)
                 └── valueNames:  ValueId → string             (source name, for the printer)
```

**Typed SSA.** Every value is defined exactly once and has a `TypeId`, recorded
in `Function::valueTypes_` at creation time — so a pass can ask `fn.typeOf(v)`
without first walking back to the definition.

**Block arguments instead of phi nodes.** `br ^bb(%a, %b)` passes values the way
a call passes arguments. Every `BlockRef` (a branch target) carries an `args`
list that must match the target block's `params` in count and type (Verifier
check 4). This removes the "phi must be at the top, operand order must match
predecessor order" class of bugs and makes critical-edge splitting a local
rewrite. `mem2reg` places these parameters at dominance-frontier blocks; every
other pass treats them uniformly.

**Arena-allocated, index-addressed.** `ValueId`, `BlockId`, `InstId`, `TypeId`,
`FuncId`, `GlobalId` are all `Id<Tag>` — a `uint32` wrapper with a
`kInvalid = UINT32_MAX` sentinel (so a default-constructed id is detectably
unset, never a silent alias of index 0). No pointer chasing; stable across
`vector` growth; `std::hash` is specialized so ids work as map keys.

> **Arena hazard.** `Function::addInst` pushes to *both* the instruction arena
> and the target block's `InstId` list. Any code that holds a
> `std::vector<InstId>&` or an `Instruction&` across an `addInst` call has a
> dangling reference after a reallocation. Passes that grow a block while
> iterating it must snapshot what they need by value first and re-fetch the
> list afterward (see the `Inline` pass for the canonical pattern).

**Locals start as `alloca`.** ZirGen emits `alloca`/`load`/`store` for every
local — simple and obviously correct. `mem2reg` (at `-O1`+) promotes the ones
whose address never escapes into SSA values. At `-O0` the `alloca`s survive all
the way to the backend, which gives every local a frame slot.

**Functions and extern declarations are the same object.** A `Function` with an
empty `blocks_` is an `extern` declaration; both are addressed in the same
`FuncId` space and called identically (`call <ret> @name(...)`). This keeps one
array instead of two that could each independently produce `FuncId(0)`.

## Type system

```
TypeKind := Void | Int | Float | Ptr | Array | Fn
```

| Kind | Fields | Notes |
|------|--------|-------|
| `Void` | — | non-value instructions, `ret void` |
| `Int` | `bits ∈ {1,8,16,32,64}`, `isSigned` | `bool` == `Int{1, false}` |
| `Float` | `bits ∈ {32,64}` | |
| `Ptr` | `pointee: TypeId` | **[divergence]** opaque in practice — the printer emits just `ptr`, and no pass or backend inspects the pointee. Pointer provenance is tracked at the instruction level (`alloca` result, `globaladdr` result), not the type level. |
| `Array` | `elem: TypeId`, `arrayLen` | used only for string-literal globals today |
| `Fn` | `params`, `ret`, `variadic` | a function's `signature` is a `Fn` TypeId |

**[deferred]** `Struct` from the original spec is not implemented (M6).

**Interned.** `TypeTable` gives structurally identical types the same `TypeId`,
so type equality is an integer compare. The table is queryable through a
`const TypeTable&` (its store is `mutable`) because "give me the bool type" is
logically a pure lookup even when it inserts on first use — the Verifier, which
only ever sees a `const Module&`, relies on this.

**Sizes and alignment are target-dependent** and come from `TargetLayout`
(`pointerBits`, `sizeTypeBits`, `sizeOfBytes`, `alignOfBytes`) — never baked
into `Type`. The legacy `TypeInfo` hardcoding `size_t` at 64 bits is exactly the
mistake this split avoids. A 1-bit `bool` rounds up to 1 byte in memory.

### Textual type spelling

`i<bits>` (signed) / `u<bits>` (unsigned), `f32` / `f64`, `ptr`, `void`,
`[N x T]`, `fn(T, …, ...) -> T`. **[divergence]** The `i`/`u` split is a printer
convention; the original spec's grammar put signedness on the type but its
example never had to disambiguate the two textually.

## Instructions

`Instruction` is one flat struct — only the fields relevant to `op` are
meaningful, the rest sit at default (mirroring `Type`). Key fields: `op`,
`type` (result type), `result` (`kInvalid` for `store` / void `call`),
`operands`, `pred` (`icmp`/`fcmp`), `constant` (raw bits), `callee` (`call`),
`global` (`globaladdr`), `elemType` (`alloca` / `gep`), `align` (`alloca`).

**Operand order** is fixed: `binop`/`fbinop`/`icmp`/`fcmp` → `[a, b]`;
`unop`/`cast` → `[a]`; `load` → `[ptr]`; `store` → `[value, ptr]`;
`gep` → `[base, idx…]`; `call` → `[args…]`; `select` → `[cond, a, b]`.

**Constants store raw bits.** `ConstValue{ bits }` holds the exact bit pattern —
for floats, the IEEE-754 pattern, not a re-parsed decimal. Deciding this at
lowering time (rather than re-deriving from source text at print time, as three
different legacy pipelines did) removed a whole class of cross-backend
divergence.

### Value-producing opcodes

| Opcode(s) | Textual form |
|-----------|--------------|
| `Const` | `%v = const <ty> <literal>` |
| `Alloca` | `%p = alloca <ty>` |
| `Load` | `%v = load <ty>, %p` |
| `Add Sub Mul SDiv UDiv SRem URem And Or Xor Shl LShr AShr` | `%v = <op> <ty> %a, %b` |
| `FAdd FSub FMul FDiv` | `%v = fadd|fsub|fmul|fdiv <ty> %a, %b` |
| `ICmp` | `%v = icmp <pred> <ty> %a, %b` — `eq ne slt sle sgt sge ult ule ugt uge` |
| `FCmp` | `%v = fcmp <pred> <ty> %a, %b` — `oeq one olt ole ogt oge` (ordered) |
| `Neg Not` | `%v = neg|not <ty> %a` |
| `Trunc ZExt SExt FPTrunc FPExt FPToSI FPToUI SIToFP UIToFP PtrToInt IntToPtr Bitcast` | `%v = <op> %a to <ty>` |
| `Gep` | `%p = gep <ty>, %base, %idx…` |
| `Call` | `%v = call <ret> @f(%a, …)` |
| `Select` | `%v = select %cond, %a, %b` |
| `GlobalAddr` | `%p = globaladdr @name` |

**[divergence]** `GlobalAddr` was added in Wave 3 — it is how lowering
references a module-level global (every string literal is a `GlobalVar`, and
every use needs its address). It produces an opaque `ptr`.

**[deferred]** `Gep`, `Select`, `PtrToInt`, `IntToPtr` exist in the enum,
printer, and parser but ZirGen does not emit them yet — the source language has
no arrays, structs, pointers, or `?:` (all M6). `Not` on an `i1` is logical
negation (`x ^ 1`), not bitwise complement — the backend relies on this.

### Non-value opcodes

| Opcode | Textual form |
|--------|--------------|
| `Store` | `store <ty> %v, %p` |
| `Call` (void) | `call void @f(%a, …)` |

### Terminators

Exactly one ends every block (`BasicBlock::term_` is a single field, not a
list — see Verifier check 1).

| Kind | Textual form |
|------|--------------|
| `Br` | `br ^bb(%args…)` |
| `CondBr` | `condbr %cond, ^then(%a…), ^else(%b…)` |
| `Ret` | `ret <ty> %v` / `ret void` |
| `Switch` | `switch <ty> %v, default ^bb [ C ^bb, … ]` |
| `Unreachable` | `unreachable` |

**[deferred]** `Switch` prints and parses but ZirGen never emits it (M6 pattern
matching). A missing `return` lowers to `unreachable` (matching the legacy
backends' undefined behavior — PRD behavior inventory), not a synthesized
`ret 0`.

## Textual form

Round-trippable: `Printer` emits it, `TextParser` reads it back, and
`Printer → TextParser → Printer` is a fixed point (tested in
`tests/zir/roundtrip/`). This is what makes every pass unit-testable from a
`.zir` file instead of driving the whole frontend.

```zir
; hello.zz
module "hello.zz" target = "generic"

@.str0 = private constant [4 x i8] c"%d\0A\00"

declare i32 @printf(ptr, ...) variadic

fn @factorial(%n: i64) -> i64 {
^entry(%n: i64):
    %c1   = const i64 1
    %cmp  = icmp sle i64 %n, %c1
    condbr %cmp, ^base, ^rec

^base:
    ret i64 %c1

^rec:
    %sub  = sub i64 %n, %c1
    %rc   = call i64 @factorial(%sub)
    %mul  = mul i64 %n, %rc
    ret i64 %mul
}
```

This is post-`mem2reg` output. Straight out of ZirGen, `%n` would be an
`alloca` + `store` + `load`. A value's printed name is its source name if it has
one (`%n`), else a synthesized `%v<N>`.

Emit it with `zpiler --emit=zir -o out.zir prog.zz` (add `-O1`/`-O2`/`-O3` to
run the pipeline before printing). `--format` is still validated but unused on
this path — no backend runs, and `TargetLayout` uses its defaults (64-bit
pointers and size type).

## Global hoisting

ZirGen reproduces the legacy backends' global handling exactly (PRD behavior
inventory, `src/zirgen/ZirGen.cpp`):

1. Every top-level `VariableDeclaration` becomes a **zero-initialized**
   `GlobalVar`; its initializer is *not* a static initializer.
2. Top-level statements split three ways: `main` set aside; declarations,
   reassignments and `++`/`--` collected in source order; everything else
   (other functions, externs) emitted before `main`.
3. `@main`'s body is the collected list (source order) followed by `main`'s own
   statements — so global initializers run as stores at the top of `@main`.
4. `@main` gets a trailing `ret 0` only if its body doesn't already end in
   `ret` (ZIR forbids two terminators per block).

## Verifier

`Verifier::verify(const Module&)` returns a `vector<VerifierFailure>` — it never
throws or aborts; the caller decides how loudly to react. It runs after lowering
and after the optimization pipeline in `RegisterBackends.cpp`'s
`lowerOptimizeVerify` (both native and LLVM backends), and again in `main.cpp`
for `--emit=zir`.

| # | Check | Meaning |
|---|-------|---------|
| 1 | `Terminator` | the block's one terminator is well-formed for its kind, and every block it names is valid. **[divergence]** the spec's "exactly one terminator, nowhere else" is structurally unviolable here (`term_` is one field), so this checks internal well-formedness instead. |
| 2 | `Dominance` | every use is dominated by its definition; block parameters are defined at block entry. Computed independently of `DominatorTree` (this check predates the pass infra). |
| 3 | `OperandTypes` | operand types match the instruction's signature exactly — no implicit conversion. |
| 4 | `BranchArgs` | every `BlockRef`'s `args` match the target block's `params` in count and type. |
| 5 | `Predecessors` | no block is reachable only from outside the entry-rooted CFG. A block with **zero** predecessors is "unreferenced and removable" and allowed; a block that dangles off an equally-dead block is not. |
| 6 | `AllocaPlacement` | `alloca` appears only in the entry block (so it runs once, as frame setup — `tailcall` and `mem2reg` both depend on this). |
| 7 | `SingleDef` | every `ValueId` is defined exactly once. |
| 8 | `ReturnType` | every `ret` matches the function's declared return type. |

**Every pass must leave the module verifier-clean on its own**, not just the
pipeline as a whole — a pass that creates a transient dangling block has to
clear it itself rather than relying on a later `SimplifyCFG` round.

## Pass manager

```cpp
class Pass       { virtual bool run(Function&, AnalysisManager&) = 0; };
class ModulePass { virtual bool run(Module&,   AnalysisManager&) = 0; };
```

`run` returns **whether it changed anything**. `PassManager::run` drives a
two-level fixpoint:

```
repeat until a whole round changes nothing:
    for each module pass:  run it module-wide; on change, invalidate all analyses
    for each function:
        repeat until a round over this function changes nothing:
            for each function pass: run it; on change, invalidate this fn's analyses
```

So an always-`true` pass hangs the compiler — deliberately: that is a bug in the
pass, not something the manager papers over by capping iterations. A module pass
(inlining) that exposes new opportunities in an already-"done" function gets
picked back up because the outer loop restarts.

`AnalysisManager` caches one `DominatorTree` per `Function`. Invalidation is
coarse — the *whole* function entry is dropped the moment any pass reports a
change to it — because recomputing a dominator tree is cheap next to running a
pass, and per-pass "what do I preserve" bookkeeping is a bug farm.

`buildPipeline(level, module)` assembles the pass list. See `docs/ZIR-PASSES.md`
for the full pipeline and every pass.

| Level | Passes (all in one `PassManager`, iterated together to a fixpoint) |
|-------|-------------------------------------------------------------------|
| `-O0` | none — lowering + verify only |
| `-O1` | `mem2reg`, `constfold`, `simplifycfg`, `dce` |
| `-O2` | `-O1` + `sccp`, `gvn`, `instcombine`, `tailcall` |
| `-O3` | `-O2` + `inline`, `licm`, `loopunroll` |

**[deferred]** `--print-zir-after=<pass>`, `--print-zir-after-all`,
`--disable-pass=<name>`, `--time-passes` from the original spec are **not
implemented**. Today's debugging path is `--emit=zir -O<n>` plus the per-pass
unit tests.

## Interpreter

**[divergence]** The standalone ZIR interpreter from the original spec was
deliberately skipped. `llc` (via `clang -c`) on the `llvm-ir` backend is the
execution oracle instead: it validates the whole IR design on Linux CI before
any x86 instruction selection, and stands in for differential testing. The 40
golden `tests/runtime` cases running green through all three backends at every
opt level is the correctness contract.

## What backends see

By the time ZIR reaches a backend it is SSA, verified, optimized, and
target-neutral apart from type layout. A backend must:

- select instructions for ZIR ops (`X86InstSel` for native, direct 1:1 mapping
  for LLVM),
- allocate registers over live intervals (`LinearScan`),
- lay out the frame once, post-allocation (`FrameLayout`),
- apply the ABI — argument placement, callee-saved set, shadow space, red zone,
  variadic rules — all as data in a `TargetABI` value (`SysVAbi` / `Win64Abi`),
- emit text (`AsmWriterAtt` / `AsmWriterIntel`, or `ZirLlvmBackend`).

Nothing else. If a backend wants to "fix up" the IR, that fix-up is a missing
ZIR pass. See `docs/BACKENDS.md`.
