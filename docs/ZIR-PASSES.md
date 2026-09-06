# ZIR Optimization Passes

Reference for every pass in the ZIR pipeline: what it does, the exact shape it
recognizes, what it deliberately does *not* do, and why each rewrite is sound.

For the IR these passes operate on, see `docs/IR-DESIGN.md`. For the "what
should we optimize and how do we measure it" strategy, see
`docs/OPTIMIZATION.md`. For migration status, `docs/PRD-ZIR.md`.

Source: `include/zir/passes/`, `src/zir/passes/`. Unit tests:
`tests/unit/zir/passes/<Pass>Test.cpp`, driven from inline `.zir` text through
`TextParser`.

---

## The pipeline

`buildPipeline(level, module)` in `src/zir/passes/Pipeline.cpp` builds one
`PassManager` per optimization level. `level` outside `[0, 3]` is clamped.

| Level | Passes added (in this order) |
|-------|------------------------------|
| `-O0` | *(none — empty `PassManager`; lowering + verify only)* |
| `-O1` | `Mem2Reg`, `ConstFold`, `SimplifyCFG`, `DCE` |
| `-O2` | `-O1` + `SCCP`, `GVN`, `InstCombine`, `TailCall` |
| `-O3` | `-O2` + `Inline` (module pass), `LICM`, `LoopUnroll` |

Every pass at a level lives in the **same** `PassManager` and they iterate to a
combined fixpoint — there is no bolted-on "second cleanup round". `mem2reg`
exposes constant folding, which exposes foldable branches, which exposes dead
code, which exposes mergeable blocks, which exposes more `mem2reg` — the
fixpoint loop chases that chain to completion instead of a hand-tuned fixed
ordering. Adding `-O2`/`-O3` passes just adds more links to the same chain
(`instcombine` turning `x*1` into `x` feeds `gvn` a match; `inline` exposes
`mem2reg`/`licm` work in the caller).

### How the pass manager runs them

`PassManager::run` (`include/zir/PassManager.hpp`) is a two-level fixpoint:

```
repeat until a full round changes nothing anywhere:
    for each ModulePass:  run module-wide; on change → invalidate ALL analyses
    for each Function:
        repeat until a round over this function changes nothing:
            for each Pass: run on this function; on change → invalidate this fn's analyses
```

Consequences:

- **A pass that always returns `true` is an infinite loop.** This is by design —
  it is a bug in the pass, and the manager will not hide it by capping
  iterations. Every pass below returns `true` *only* when it actually mutated
  the function.
- Most passes do **one transformation per `run()` call** and return, letting the
  fixpoint re-invoke them against a freshly-consistent module. `ConstFold`,
  `DCE`, `InstCombine`, `SCCP`, `GVN` are the exceptions — they do a full sweep
  per call because each rewrite is independent and can't invalidate another's
  precondition.
- `Inline` is the only `ModulePass`. It runs in the same outer fixpoint, so a
  function it inlines gets re-optimized by every function pass afterward.

### Analyses

`AnalysisManager` caches one `DominatorTree` per `Function`; invalidation is
coarse (drop the whole function's entry on any change). Recomputing a dominator
tree is cheap next to running a pass.

`DominatorTree` (`src/zir/DominatorTree.cpp`) provides, for one function:
`isReachable`, `dominates(a, b)`, `immediateDominator`, `predecessors`,
`successors`, and `dominanceFrontier` (Cytron et al. — the merge-point set
`mem2reg` needs). It is built from the CFG implied by every block's terminator
targets. The Verifier computes its own dominance check independently (it predates
the pass infrastructure); the two are not yet unified.

`countUses(fn)` (`src/zir/Uses.cpp`) returns a per-`ValueId` use count over every
instruction operand and every terminator reference (branch args, condition,
return value). `DCE` and `Mem2Reg`'s shape check use it.

---

## `-O1` passes

### Mem2Reg — promote `alloca` to SSA

`src/zir/passes/Mem2Reg.cpp` · function pass · **the single biggest win** — every
local is an `alloca` out of ZirGen, and every other pass is far more effective
once they are SSA values.

**What it does.** Promotes an entry-block `alloca` out of memory entirely: its
loads become direct uses of the reaching value, its stores define that value,
and merge points get a new **block parameter** (ZIR's phi equivalent). Standard
dominance-frontier placement + dominator-tree-order rename (Cytron et al.).

**Recognized shape.**
1. `alloca` is in the entry block (all of them are, out of ZirGen).
2. *Every* use of the `alloca`'s pointer value is a `load`'s pointer operand or a
   `store`'s **destination** operand — never a `store`'s value, a `call`
   argument, a `gep` base, a cast, or a branch argument. (An "address never
   escapes" check. ZirGen never violates it; the check keeps the pass honest
   against future producers.)
3. Every `load` has a reaching `store` (or merge-point definition) along *every*
   dominator-tree path from entry, **and** every edge into one of this
   `alloca`'s own frontier blocks comes from a predecessor that already has a
   value to contribute.

**What it declines.** An `alloca` failing check 3 is left as `alloca`/`load`/
`store`. Promoting it would need an `undef` value ZIR does not have. This is the
known **block-scoping gap** (`CLAUDE.md`): `let` inside an untaken `if`/loop
branch leaks into the enclosing scope and can be read with whatever was on the
stack — undefined but not crashing. Leaving it in memory preserves exactly that
behavior.

**Output.** The `alloca`, its stores, and its loads are all removed; new block
parameters and branch arguments thread the value through merges. Nothing
`mem2reg`-related is left for `DCE`.

**Tests.** Straight-line promotion; two-arm merge (introduces a block param);
loop induction variable via a header param; declines an undominated load;
declines an address escaping into a call.

---

### ConstFold — fold constant-operand instructions

`src/zir/passes/ConstFold.cpp` · function pass · needs the `Module` (for the
`TypeTable`).

**What it does.** An instruction whose operands are *all* already `const`
becomes a `const` of the computed value, **in place** — same `ValueId`, same
type, so nothing referencing the result changes. Full sweep per `run()`; the
just-folded result is immediately available to fold the next instruction in the
same pass.

**Scope.** Every `binop`/`fbinop`, `icmp`/`fcmp`, `unop` (`neg`/`not`), and `cast`
(`trunc`, `zext`, `sext`, `fptrunc`, `fpext`, `fptosi`, `fptoui`, `sitofp`,
`uitofp`, `bitcast`). **Not** `select`, `gep`, `call`, `load`/`store`/`alloca`,
or `ptrtoint`/`inttoptr`.

**Semantics.** Integer arithmetic is masked to the result type's bit width;
signed ops sign-extend first. Floats are folded by `memcpy`-reinterpreting the
stored bit pattern to `float`/`double` and back — so `-O1` and runtime agree bit
for bit. **Declines** (leaves the instruction alone rather than inventing UB
semantics): `sdiv`/`udiv`/`srem`/`urem` by a constant zero; a shift amount `≥`
the type width.

**Tests.** `tests/unit/zir/passes/ConstFoldTest.cpp` — per-opcode folding, the
division-by-zero and over-wide-shift bail-outs, float bit-exactness.

---

### SimplifyCFG — clean up control flow

`src/zir/passes/SimplifyCFG.cpp` · function pass.

**One category of change per `run()` call**, then return — each category
invalidates the next one's precomputed CFG view (predecessor counts, which
blocks exist as separate entities, reachability), so the fixpoint loop must
re-run the pass between them:

1. **Constant condition.** A `condbr` whose condition is a still-in-place
   `const` becomes an unconditional `br` to whichever side is actually taken.
2. **Straight-line merge.** A block ending in an unconditional `br` to a target
   that has no *other* predecessor absorbs that target: the target's
   instructions are re-parented, its terminator moves up, and its block
   parameters are substituted with the branch's arguments. The target becomes an
   empty, unreferenced block (Verifier check 5 tolerates this). **Entry is never
   a merge target** — it has an invisible predecessor (the function's callers,
   and since `tailcall`, its own back-edge) that no internal CFG edge sees.
3. **Dead-block sweep.** Any block unreachable from entry is cleared to an inert
   `{ no instructions, unreachable }` stub. One reachability pass (BFS from
   entry) catches a whole dead subgraph at once, so a chain of blocks that lost
   their entry edge but still point at each other all get cleared together —
   which is what keeps each pass individually verifier-clean.

**Tests.** `tests/unit/zir/passes/SimplifyCFGTest.cpp`.

---

### DCE — dead code elimination

`src/zir/passes/DCE.cpp` · function pass.

**What it does.** Removes any result-producing instruction whose result has zero
uses (`countUses`) **and** that has no side effect. Full sweep per `run()`.

**Side-effect rule.** `Store` and `Call` are **never** removed regardless of use
count. From the PRD behavior inventory: *"DCE must treat calls as
side-effecting, or `-O1` deletes the `printf` every golden depends on."* A void
call has no result to be "unused"; a non-void call whose result nobody reads
must still survive. Everything else — arithmetic, casts, comparisons, `load`,
`alloca`, `globaladdr` — is pure with respect to the rest of the program and
safe to drop when dead.

**Note.** DCE does not remove dead *block parameters* or the branch arguments
feeding them (`SCCP` and `Mem2Reg` leave those); it only prunes the instruction
lists.

**Tests.** `tests/unit/zir/passes/DCETest.cpp`.

---

## `-O2` passes

### SCCP — sparse conditional constant propagation

`src/zir/passes/SCCP.cpp` · function pass.

**What it does.** Finds a non-entry block parameter whose incoming value, across
*every* predecessor edge, is the exact same constant. Rewrites every **use** of
that parameter to a fresh `const` prepended to the block.

**Scope.** Only what `ConstFold` and `SimplifyCFG`'s constant-condition handling
cannot already see: a merge that is constant. The common case is a
loop-invariant merge — a value nothing in the loop body actually changes —
collapsing to a plain constant.

**What it leaves.** The parameter declaration and its now-redundant branch
arguments stay in place (dead, for later cleanup) rather than renumbering every
other parameter at that block. Entry parameters are real function arguments and
are never touched.

**Tests.** `tests/unit/zir/passes/SCCPTest.cpp`.

---

### GVN — global value numbering

`src/zir/passes/GVN.cpp` · function pass.

**What it does.** Walks the dominator tree carrying an "already computed this"
table keyed by a cheap textual signature of each pure instruction (opcode, pred,
types, const bits, operand ids). A second instruction with a matching signature
has its result rewritten to point at the first, equivalent one. Because the walk
is dominator-tree order, the table built along one path is only visible to
blocks that path dominates — a single hash-map lookup stands in for a real
dominance check.

**Purity.** Same notion as `LICM`: everything *except* `Load`, `Store`,
`Alloca`, `Call`, `Select`, `Gep`. Those either have side effects or would need
real alias analysis to CSE safely (the pipeline has none).

**What it leaves.** Redundant instructions stay in place, unused, for `DCE` to
remove — GVN only rewrites *uses*. Substitution chains (a replacement that is
itself scheduled for replacement) are resolved before applying.

**Tests.** `tests/unit/zir/passes/GVNTest.cpp`.

---

### InstCombine — algebraic identities

`src/zir/passes/InstCombine.cpp` · function pass.

**What it does.** Peephole simplifications that need a constant on only *one*
side (or neither): `x + 0`, `0 + x`, `x - 0`, `x - x` → `0`, `x * 1`, `1 * x`,
`x * 0` → `0` (integer only), `x / 1`, `x ^ 0`, `x ^ x` → `0`, `x & x`, `x & 0`
→ `0`, `x | x`, `x | 0`. Full sweep per `run()`.

**Output shape.** "This instruction's value is just some other value" becomes a
same-type `Bitcast` of that value — a genuine no-op at the ZIR level and at
codegen (`ZirLlvmBackend` and `X86InstSel` both treat a same-width bitcast as a
relabel). "This is a constant" becomes a `Const`. Either way `DCE` removes the
now-dead original operands.

**Tests.** `tests/unit/zir/passes/InstCombineTest.cpp`.

---

### TailCall — self-recursive tail-call elimination

`src/zir/passes/TailCall.cpp` · function pass · needs the `Module` (to find its
own `FuncId`).

**What it does.** Turns `%r = call @f(args…); ret %r` (or `call void @f(args…);
ret void`) *inside `@f` itself* into a `br` back to the loop header with `args…`
as the new incoming values — eliminating the call and its stack frame.

**Entry split.** LLVM hard-requires a function's entry block to have no
predecessors, so a tail call cannot branch to `fn.entry()` directly. The pass
splits entry once: everything entry held moves to a new block `entry.tailrec`
(keeping every existing `ValueId`, so nothing else needs rewriting), and entry
becomes a one-time forwarder with fresh parameters that `br`s into the header.
Every tail call then targets the header, which may have as many predecessors as
it likes.

**Precondition.** Bails if the entry block contains any `alloca` — re-entering
entry would re-run one-time frame setup. `Mem2Reg` runs earlier in the same
pipeline and normally clears every `alloca`, so this is the common case by `-O2`,
not a rare one.

**Tests.** `tests/unit/zir/passes/TailCallTest.cpp` — includes "declines when
the entry block still has an alloca".

---

## `-O3` passes

### Inline — inline small simple callees

`src/zir/passes/Inline.cpp` · **module pass** (the only one) · `-O3`.

**What it does.** Inlines one call site per `run()` call. The callee's
instructions are copied into the caller at the call site, each getting a fresh
`ValueId`; callee parameters map to the call's actual arguments; the call's
result is substituted function-wide with the callee's returned value.

**Recognized callee.** Exactly one block ending in `ret`/`ret void`; not
`extern`; not variadic; no `alloca` (a callee `alloca` would land at the call
site, not the caller's entry — Verifier check 6); not in an immediate
two-function call cycle with the caller (a cheap guard against unbounded
mutual-recursion growth across the fixpoint — true self-recursion is `TailCall`'s
job and always has more than one block anyway).

**Why a module pass.** The callee's body lives in a different `Function` than the
one being rewritten — the original spec's own example of something that needs
whole-module knowledge. It runs in the same outer fixpoint, so an inlined
function is re-optimized by every function pass afterward (new `mem2reg`,
`constfold`, `licm` opportunities in the caller).

**Arena-hazard note.** `Function::addInst` reallocates both the instruction
arena and the block's `InstId` list, so the pass snapshots everything it needs
off the call instruction (`calleeId`, `callArgs`, `oldResult`) *by value* before
the copy loop and re-fetches the block list *after* it. Holding a reference
across `addInst` here caused a heap-use-after-free (fixed; the snapshot pattern
is the guard).

**Tests.** `tests/unit/zir/passes/InlineTest.cpp`.

---

### LICM — loop-invariant code motion

`src/zir/passes/LICM.cpp` · function pass · `-O3`.

**What it does.** Finds a natural loop from a back edge (`N → H` where `H`
dominates `N`), then hoists **one** pure, loop-invariant instruction per
`run()` call into the loop's preheader.

**Recognized shape.** `H` has exactly one predecessor outside the loop body, and
that predecessor ends in a plain `br` into `H` — exactly the shape ZirGen's
`for`/`while` lowering always produces, so this is the common case. That
predecessor is the preheader. An instruction is hoistable if it is pure (same
notion as `GVN`) and every operand is defined outside the loop body. The
fixpoint loop picks up anything the first hoist exposes.

**Tests.** `tests/unit/zir/passes/LICMTest.cpp`.

---

### LoopUnroll — fully unroll small counting loops

`src/zir/passes/LoopUnroll.cpp` · function pass · needs the `Module` · `-O3`.

**What it does.** Fully unrolls a counting loop whose trip count is known at
compile time: simulates the loop, emits one straight-line block per iteration
(each a copy of the latch's instructions with the induction variable replaced by
that iteration's concrete constant), wires them in sequence from the preheader
to the loop's exit, and clears the original header and latch.

**Recognized shape** (bails the moment any part isn't exactly met):
- exactly two blocks: header + latch (the shape after `-O1`'s `SimplifyCFG` has
  merged away a separate post-block — common by the time this runs in the same
  fixpoint);
- header has exactly one block parameter — the induction variable (a second
  loop-carried value isn't tracked);
- header ends in a `condbr` whose condition is an `icmp` of that parameter
  against a **constant** bound;
- latch ends in a plain `br` back to the header, advancing the same parameter by
  a **constant** step (`add` or `sub`);
- the preheader feeds a **constant** initial value;
- simulated trip count `≤ 64` (`kMaxUnrollIterations`).

Values a descendant block reads directly from the header (e.g. `^end: ret i64
%i` reading the loop counter in the canonical `for` shape) are replaced
function-wide with the concrete exit-time constant — not just along the exit
edge's argument list. The pass clears `header` and `latch` itself (they point
only at each other — "dangling", which Verifier check 5 rejects — not simply
dead).

**Tests.** `tests/unit/zir/passes/LoopUnrollTest.cpp`.

---

## Verification & testing contract

- **Verifier-clean after every pass.** Under `-DZUST_ASSERTIONS` / in the
  backends' `lowerOptimizeVerify`, the module is verified after lowering and
  after the whole pipeline. Each pass is additionally expected to be
  individually verifier-clean (hence the manual dead-block clearing in
  `SimplifyCFG` and `LoopUnroll`).
- **Golden suite.** All 40 `tests/runtime` cases produce byte-identical output
  at `-O0`, `-O1`, `-O2`, `-O3` on `llvm-ir`, `x86_64-linux`, and
  `x86_64-mswin` — a 40 × 3 × 4 = 480-cell matrix. `pytest` runs all four opt
  levels by default and the native target; `TARGET=linux,llvm,windows` and
  `OPT=0,2` narrow or widen each axis (CI covers the full matrix across jobs).
- **Per-pass unit tests.** `tests/unit/zir/passes/<Pass>Test.cpp`, each built on
  hand-written `.zir` fed through `TextParser` — the pass runs on a module the
  frontend never produced, so the test pins the transformation itself, not a
  lowering accident.
- **Round-trip.** `tests/zir/roundtrip/*.zir` — `Printer → TextParser → Printer`
  is a fixed point.

## Adding a pass

1. Add `include/zir/passes/Foo.hpp` + `src/zir/passes/Foo.cpp` with a
   `class FooPass : public Pass` (or `ModulePass`). `name()` returns the pass
   name; `run()` returns `true` **only** on real change.
2. Wire it into `buildPipeline` at the right level.
3. Add `tests/unit/zir/passes/FooTest.cpp` driven from `.zir` text.
4. Confirm the 480-cell golden matrix stays green and the module stays
   verifier-clean after your pass on every case.
5. Never fold a rewrite into a backend — if a backend "wants" the transform, it
   belongs here (`docs/BACKENDS.md`, invariant 2).
