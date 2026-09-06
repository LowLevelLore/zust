# zpiler Architecture

> **Status (Wave 6 complete).** The five-stage ZIR pipeline described under
> *Target pipeline* below has landed for all three backends. Every backend now
> consumes ZIR: `ZirLlvmBackend` for `llvm-ir`, and the shared x86 machine
> layer (`X86InstSel → LinearScan → FrameLayout → AsmWriter{Att,Intel}`)
> against a `TargetABI` value (`SysVAbi` / `Win64Abi`) for the two native
> targets, at every optimization level `-O0`–`-O3`. The legacy AST-walking
> `CodeGen{Linux,Windows,LLVM}` emitters and `RegisterAllocator` have been
> deleted. What remains of the old coupling: the parser still builds the
> `ScopeContext` chain and the `FunctionScope` frame API / `NameMapper` are
> not yet removed (PRD-ZIR Wave 7.1). The diagram immediately below is kept
> for historical context.

## Original pipeline (pre-ZIR, for context)

```
                 .zz source text
                        │
                        ▼
   ┌────────────────────────────────────────┐
   │ Lexer  (src/lexer/Lexer.cpp)           │  on-demand, peek(n) lookahead
   │   → Token { kind, text, line, column } │  no token buffer, re-lexes on reset()
   └────────────────────────────────────────┘
                        │
                        ▼
   ┌────────────────────────────────────────┐
   │ Parser (src/parser/Parser.cpp)         │  recursive descent + precedence climbing
   │   builds ASTNode tree                  │  ALSO builds ScopeContext chain and
   │                                        │  defines vars/functions/types eagerly
   └────────────────────────────────────────┘
                        │
                        ▼
   ┌────────────────────────────────────────┐
   │ TypeChecker (src/typechecker/)         │  walks AST, returns type name per node
   │   promoteType() numeric promotion      │  sets shouldCodegen_ = false on error
   └────────────────────────────────────────┘
                        │
                        ▼
   ┌────────────────────────────────────────┐
   │ CodeGen::create(TargetTriple, ostream) │
   ├──────────────┬──────────────┬──────────┤
   │ CodeGenLinux │CodeGenWindows│CodeGenLLVM│
   │  GAS AT&T    │  MASM Intel  │  LLVM IR  │
   │  SysV ABI    │  Win64 ABI   │  (textual)│
   └──────────────┴──────────────┴──────────┘
```

### Consequences of this shape

The parser, symbol table, and code generator are coupled:

- `Parser` *is* the resolver. `ScopeContext::defineVariable` allocates stack slots
  during parsing, so stack layout is decided before types are fully known.
- `ASTNode` carries a `shared_ptr<ScopeContext>`, so the AST is not a pure tree —
  it holds live symbol-table state that codegen mutates.
- Each backend re-implements expression evaluation, calling conventions, spilling,
  and casts. `CodeGenLinux.cpp` (1359 lines), `CodeGenWindows.cpp` (1661), and
  `CodeGenLLVM.cpp` (1048) share **no** lowering logic. Every new language feature
  costs three implementations and diverges silently.
- There is no place to put an optimization. Constant folding today would have to be
  written three times.

That is the motivation for ZIR.

## The pipeline (current, post-Wave-6)

Reference docs: `docs/IR-DESIGN.md` (the IR), `docs/ZIR-PASSES.md` (the
optimization passes), `docs/BACKENDS.md` (the emitters).

```
   Lexer ──▶ Parser ──▶ AST (+spans, no symbols)
                          │
                          ▼
                   ┌──────────────┐
                   │  Sema        │  Resolver: names → SymbolId
                   │              │  TypeCheck: SymbolId → Type
                   └──────────────┘  produces a fully annotated AST
                          │
                          ▼
                   ┌──────────────┐
                   │  ZIR Builder │  AST → typed SSA, alloca-based locals
                   └──────────────┘
                          │
                          ▼
                   ┌──────────────┐
                   │  Pass Manager│  mem2reg, sccp, dce, gvn, simplifycfg,
                   │  (-O0..-O3)  │  inline, licm, tailcall
                   └──────────────┘
                          │
        ┌─────────────────┼─────────────────┐
        ▼                 ▼                 ▼
  LLVM IR emit      x86_64 SysV        x86_64 Win64
  (ZirLlvmBackend)  ┌────────────────────────────┐
                    │ shared: X86InstSel,        │
                    │ LiveIntervals, LinearScan, │
                    │ FrameLayout, AsmWriter     │
                    │ ABI differences = a        │
                    │ TargetABI value            │
                    └────────────────────────────┘
```

The native backends are **one** code generator parameterized by a `TargetABI`
value (argument registers, callee-saved set, shared vs independent arg slots,
shadow space, red zone, variadic rule, assembler syntax) — `SysVAbi` and
`Win64Abi` — rather than two forks. The standalone ZIR interpreter from the
original plan was skipped; `llc` on the LLVM backend is the execution oracle
(`docs/IR-DESIGN.md` § Interpreter).

## Key data structures

### `ASTNode` (`include/ast/ASTNode.hpp`)
A single struct with a `NodeType` tag, a `std::string value`, and
`std::vector<std::unique_ptr<ASTNode>> children`. Positional children — e.g. a
function node's children are `[paramList, returnType, body]`, accessed via
`getFunctionParamList()` etc.

*Debt:* untyped `value` strings, no source span, positional children are easy to
get wrong. See ROADMAP M1.

### `ScopeContext` (`include/parser/ScopeContext.hpp`)
Lexical scope chain. Three kinds:

| Kind             | Stack allocation                              |
|------------------|-----------------------------------------------|
| `NamespaceScope` | throws — globals go in `.data`                |
| `FunctionScope`  | owns `stackOffset_`, spill slots, stack canary |
| `BlockScope`     | delegates to the enclosing `FunctionScope`     |

Also holds `variable_name_mappings` — the mangled-name table used to keep shadowed
variables distinct in the emitted code. `NameMapper` (a **global** in `all.hpp`)
generates those mangled names with a monotonic counter.

*Debt:* a process-global `NameMapper` makes the compiler non-reentrant and output
order-dependent. See ROADMAP M2.

### `TypeInfo`
`{bits, align, isFloat, isSigned, isString, isBoolean, isPointer, isUserDefined,
isFunction, name}`. A flat bag of booleans rather than a tagged union — adding
arrays, slices, or structs will require restructuring this into a proper type
table with interned `TypeId`s. See ROADMAP M2/M6.

### Register allocation (`src/codegen/machine/LinearScan.cpp`)
The legacy per-emission `RegisterAllocator` is **deleted** (Wave 7.1). Native
codegen now runs `LinearScan` over block-local `LiveIntervals` on the machine IR
`X86InstSel` produces, with the allocatable pool set to exactly the callee-saved
registers so every vreg is call-safe unconditionally. See `docs/BACKENDS.md` and
`docs/PRD-ZIR.md` Wave 5.

## Where the platform differences actually are

| Concern              | Linux (SysV)                  | Windows (Win64)                        |
|----------------------|-------------------------------|----------------------------------------|
| Integer arg regs     | rdi rsi rdx rcx r8 r9         | rcx rdx r8 r9                          |
| Float arg regs       | xmm0–xmm7                     | xmm0–xmm3 (paired with the GPR slot)   |
| Callee-saved GPR     | rbx r12–r15 (+rbp)            | rbx rdi rsi r12–r15 (+rbp)             |
| Callee-saved XMM     | none                          | xmm6–xmm15                             |
| Shadow space         | none                          | 32 bytes reserved by caller            |
| Red zone             | 128 bytes                     | none                                   |
| Variadic float count | `al` = # of vector args       | float args also duplicated into GPRs   |
| Assembler            | GNU `as`, AT&T                | `ml64`, Intel/MASM                     |
| Name mangling        | plain symbol                  | plain symbol (C linkage)               |

These are the only things a backend should branch on. Anything else branching on
target is a bug waiting to happen.
