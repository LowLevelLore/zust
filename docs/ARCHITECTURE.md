# zpiler Architecture

## Current pipeline (as of today)

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

## Target pipeline

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
                          ├──────────────▶ ZIR interpreter (test oracle)
                          │
        ┌─────────────────┼─────────────────┐
        ▼                 ▼                 ▼
  LLVM IR emit      x86_64 SysV        x86_64 Win64
  (thin, ~400 loc)  ┌────────────────────────────┐
                    │ shared: isel, live ranges, │
                    │ linear-scan regalloc,      │
                    │ frame layout, peephole     │
                    │ ABI differences = data     │
                    └────────────────────────────┘
```

The native backends become **one** code generator parameterized by an ABI
description (argument registers, callee-saved set, shadow space, red zone,
assembler syntax), rather than two forks.

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

### `RegisterAllocator` (`include/codegen/RegisterAllocator.hpp`)
Per-target free lists (`forSysV()` / `forMSVC()`) plus an LRU victim picker and a
spill-slot table. It allocates *during* emission with no knowledge of live ranges,
so it spills far more than necessary and cannot keep a value in a register across
a statement boundary. Replacement is a ZIR-level linear-scan allocator (M5).

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
