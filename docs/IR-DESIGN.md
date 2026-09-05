# ZIR — zust Intermediate Representation

Status: **design**. Nothing here is implemented yet; this document is the
contract to implement against (ROADMAP M3).

## Why

Three backends today share zero lowering logic, so every language feature costs
three implementations and there is nowhere to put an optimization. ZIR is the one
place semantics get lowered and optimized; backends become emitters.

## Shape

```
Module
 ├── globals:   [GlobalVar]
 ├── types:     TypeTable (interned)
 └── functions: [Function]
                 ├── signature: (params: [TypeId], ret: TypeId, variadic: bool)
                 ├── entry: BlockId
                 └── blocks: [BasicBlock]
                              ├── params: [ValueId]      (block arguments, not phis)
                              ├── insts:  [InstId]
                              └── term:   Terminator
```

**Typed SSA.** Every value is defined exactly once and has a `TypeId`.

**Block arguments instead of phi nodes.** `br label(%a, %b)` passes values the way
a call passes arguments. This removes the "phi must be at the top and its operand
order must match predecessor order" class of bugs, and makes critical-edge
splitting a local rewrite.

**Arena-allocated, index-addressed.** `ValueId`, `BlockId`, `InstId`, `TypeId` are
all `uint32` indices into flat vectors. No pointer chasing, cache-friendly, cheap
to clone for speculative passes, trivially serializable, and stable across
`vector` growth.

**Locals start as `alloca`.** Lowering emits `alloca`/`load`/`store` for every
local — simple and obviously correct. `mem2reg` then promotes the ones without
address-taken uses into SSA registers. This is the standard LLVM approach and it
keeps the lowering code honest.

## Type system

```
Type := Void
      | Int   { bits: 1|8|16|32|64, signed: bool }
      | Float { bits: 32|64 }
      | Ptr   { pointee: TypeId }
      | Array { elem: TypeId, len: u64 }
      | Struct{ id: StructId }          // fields, layout computed per target
      | Fn    { params: [TypeId], ret: TypeId, variadic: bool }
```

Interned: identical structural types get the same `TypeId`, so type equality is
integer comparison. `bool` is `Int{1, false}`; `string` is initially
`Ptr(Int{8,false})` and becomes a real slice type at M6.

Sizes and alignment are **target-dependent** and come from the `TargetLayout`, not
baked into the type. The current `TypeInfo` hardcodes `size_t = 64` bits; ZIR must
not repeat that.

## Instructions

Values (produce a result):

| Instruction        | Form                                        |
|--------------------|---------------------------------------------|
| `const`            | `%v = const <ty> <literal>`                 |
| `alloca`           | `%p = alloca <ty>[, align N]`               |
| `load`             | `%v = load <ty>, %p`                        |
| `binop`            | `%v = add|sub|mul|sdiv|udiv|srem|urem|and|or|xor|shl|lshr|ashr <ty> %a, %b` |
| `fbinop`           | `%v = fadd|fsub|fmul|fdiv <ty> %a, %b`      |
| `icmp`             | `%v = icmp eq|ne|slt|sle|sgt|sge|ult|ule|ugt|uge <ty> %a, %b` |
| `fcmp`             | `%v = fcmp oeq|one|olt|ole|ogt|oge <ty> %a, %b` |
| `unop`             | `%v = neg|not <ty> %a`                      |
| `cast`             | `%v = trunc|zext|sext|fptrunc|fpext|fptosi|fptoui|sitofp|uitofp|ptrtoint|inttoptr|bitcast %a to <ty>` |
| `gep`              | `%p = gep <ty>, %base, %idx…`               |
| `call`             | `%v = call <ret> @f(%a, …)`                 |
| `select`           | `%v = select %cond, %a, %b`                 |

Non-value:

| Instruction        | Form                                        |
|--------------------|---------------------------------------------|
| `store`            | `store <ty> %v, %p`                         |
| `call` (void)      | `call void @f(%a, …)`                       |

Terminators (exactly one, at the end of every block):

| Terminator | Form                                              |
|------------|---------------------------------------------------|
| `br`       | `br ^bb(%args…)`                                  |
| `condbr`   | `condbr %cond, ^then(%a…), ^else(%b…)`            |
| `ret`      | `ret <ty> %v` / `ret void`                        |
| `switch`   | `switch <ty> %v, default ^bb [ C ^bb, … ]`        |
| `unreach`  | `unreachable`                                     |

`switch` exists for M6 pattern matching; lowering may emit only `condbr` at first.

## Textual form

Round-trippable — `--emit=zir` prints it, a `.zir` parser reads it back. This is
what makes passes unit-testable without driving the whole frontend.

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

Note there are no `alloca`s here — that is post-`mem2reg` output. Straight out of
the lowerer, `%n` would be an `alloca` + `store` + `load`.

## Verifier

Runs after lowering and after **every** pass when assertions are on. Checks:

1. Every block ends in exactly one terminator, and terminators appear nowhere else.
2. Every use is dominated by its definition (block arguments are defined at block entry).
3. Operand types match the instruction's signature exactly — no implicit conversion.
4. Branch argument lists match the target block's parameter list in count and type.
5. Every block except entry has a predecessor, or is unreferenced and removable.
6. `alloca` appears only in the entry block (canonical form after `mem2reg` prep).
7. `ValueId`s are defined exactly once.
8. Function returns match the declared return type on every `ret`.

A verifier failure is a compiler bug: `throw`, print the offending function, and abort.

## Pass manager

```cpp
class Pass          { virtual bool run(Function&, AnalysisManager&) = 0; };
class ModulePass    { virtual bool run(Module&,   AnalysisManager&) = 0; };
```

`run` returns whether it changed anything, which drives fixpoint iteration.
Analyses (`DominatorTree`, `LoopInfo`, `AliasAnalysis`) are cached in the
`AnalysisManager` and invalidated by passes that declare they break them.

Pipelines:

- `-O0`: lowering + verifier only. Fast, debuggable, 1:1 with source.
- `-O1`: mem2reg, constfold, simplifycfg, dce.
- `-O2`: `-O1` + sccp, gvn, instcombine, tailcall, then a second cleanup round.
- `-O3`: `-O2` + inline, licm, loop unrolling, more iterations.
- `-Os`: out of scope for the initial pass catalogue; revisit once `-O2`/`-O3`
  are proven (a size-biased inliner cost model on top of `-O2`).

(`docs/PRD-ZIR.md` tracks the implementation of this pipeline and is the
authoritative pass-to-level assignment; this section mirrors it.)

Debug flags: `--print-zir-after=<pass>`, `--print-zir-after-all`,
`--disable-pass=<name>`, `--time-passes`.

## Interpreter

A direct ZIR interpreter, used as the **test oracle**. For every `tests/runtime`
case, the interpreter's output must equal each backend's output. This is how we
find backend bugs without hand-reading assembly, and it is a prerequisite for the
differential fuzzing in M9.

It also lets us implement compile-time constant evaluation by reusing the same
evaluation core.

## What backends see

By the time ZIR reaches a backend it is: SSA, verified, optimized, and
target-neutral apart from type layout. A backend must:

- pick instructions for ZIR ops,
- allocate registers over live intervals,
- lay out the frame,
- apply the ABI (argument placement, callee-saved, shadow space, variadic rules),
- emit text.

Nothing else. If a backend wants to "fix up" the IR, that fix-up is a missing ZIR pass.

## Migration plan (incremental, always-green)

1. Land ZIR data structures + verifier + printer/parser, unused. No behavior change.
2. Land AST → ZIR lowering + interpreter. Add a `--emit=zir` flag and an
   interpreter-based test mode running alongside the existing backends.
3. Rewrite `CodeGenLLVM` to consume ZIR. Both paths coexist behind
   `--legacy-codegen`; goldens must match byte-for-byte before the switch.
4. Rewrite the native backends the same way, one at a time (Linux first, then Windows).
5. Delete the legacy path and `--legacy-codegen`.
6. Only then start adding optimization passes.

Each step keeps the full test suite green on all three backends.
