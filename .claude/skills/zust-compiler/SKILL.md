---
name: zust-compiler
description: "Architecture and orientation for the zust/zpiler compiler. Use when: getting oriented in this codebase, tracing how a .zz program becomes assembly, deciding which compiler phase owns a problem, or answering 'where does X happen'."
---

# zust Compiler — Orientation

Load this before any nontrivial work in this repo. For deeper task-specific
guidance, load one of: `zust-language-feature`, `zust-ir`, `zust-backend-abi`,
`zust-testing`, `zust-debug-codegen`.

## The pipeline

```
.zz ──▶ Lexer ──▶ Parser (+ScopeContext) ──▶ TypeChecker ──▶ CodeGen{Linux,Windows,LLVM}
```

`main.cpp` orchestrates all of it, in that order, with an early `return 1` between
each stage if the previous one failed. Target selection happens at the bottom of
`main.cpp` via `CodeGen::create(TargetTriple, ostream)`.

We are migrating to `… ──▶ Sema ──▶ ZIR ──▶ passes ──▶ backends`. See
`docs/IR-DESIGN.md` and `docs/ROADMAP.md`. New optimization work goes in ZIR, not
in a backend.

## Which phase owns the problem

| Symptom | Owner | File |
|---|---|---|
| Token not recognized, bad literal, comment eats code | Lexer | `src/lexer/Lexer.cpp` |
| "Expected X at line N", wrong precedence, missing construct | Parser | `src/parser/Parser.cpp` |
| Variable not found, wrong scope, shadowing broken | ScopeContext (built by Parser) | `src/parser/ScopeContext.cpp` |
| Wrong type accepted/rejected, bad promotion | TypeChecker | `src/typechecker/TypeChecker.cpp` |
| Compiles but runs wrong / crashes / wrong number printed | CodeGen | `src/codegen/CodeGen{Linux,Windows,LLVM}.cpp` |
| Works on Linux, breaks on Windows (or vice versa) | ABI | see `zust-backend-abi` skill |
| Wrong register, corrupted value after a call | RegisterAllocator | `src/codegen/RegisterAllocator.cpp` |

Bisect with the flags: `--print-ast` shows whether the parser got it right; if the
AST is correct the bug is downstream.

## Reading the code — what will surprise you

- **The parser is also the resolver.** `Parser::parseVariableDeclaration` calls
  `ScopeContext::defineVariable`, which calls `allocateStack` — stack layout is
  decided during parsing, before type checking runs.
- **AST nodes hold a `shared_ptr<ScopeContext>`.** The AST is not a pure tree; it
  points into live symbol-table state that codegen reads and mutates.
- **Children are positional.** A `Function` node's children are
  `[paramList, returnType, body]`. Always use the named accessors
  (`getFunctionParamList()`, `getBodyForLoop()`, …), never `children[2]`.
- **`GLOBAL_NAME_MAPPER` is a process-global** declared in `include/all.hpp`. It
  mangles `name` → `Scope___name___vN`. Shadowing depends on it; so does output
  determinism.
- **Codegen consumes the AST.** `generateStatement(std::unique_ptr<ASTNode>, …)`
  takes ownership — nodes are destroyed as they are emitted. You cannot walk the
  tree twice without restructuring.
- **The three backends share nothing** below `CodeGen`'s helpers. A fix in
  `CodeGenLinux.cpp` almost always needs the same fix in the other two, written
  differently. Always check all three.

## Types

`TypeInfo` is a flat struct of booleans (`isFloat`, `isSigned`, `isString`,
`isPointer`, …) plus `bits`/`align`/`name`. Built-in types are registered in the
`Parser` constructor as literal `TypeInfo` initializers. To add a builtin type you
add it there, add its name to `numeric_types`/`integral_types` in
`TypeChecker.hpp` if applicable, and handle its size in every backend's move-size
switch.

`TypeChecker::promoteType` implements binary-operation promotion: floats win over
ints, wider wins over narrower, signed wins on ties. It throws on strings,
pointers, and user-defined types.

## Scopes

Three kinds, all deriving `ScopeContext`:

- `NamespaceScope` — the global scope. `allocateStack` throws; globals go to `.data`.
- `FunctionScope` — owns the frame: `stackOffset_`, spill slots, the stack canary.
- `BlockScope` — delegates `allocateStack` to its enclosing `FunctionScope`.

`lookupVariable` walks up the parent chain and throws if not found.
`getMapping`/`setMapping` carry the mangled-name table.

**Currently broken:** `if`/`for`/`while` bodies do not push a `BlockScope`
(see ROADMAP M0-1). Do not build on the current behavior.

## Build, run, test

```bash
cmake -S . -B build -DCMAKE_BUILD_TYPE=RelWithDebInfo && cmake --build build -j
python3 -m pytest -q                      # native target
TARGET=linux,llvm python3 -m pytest -q    # both Linux backends
./build/zpiler --print-ast prog.zz        # inspect the AST
```

Full end-to-end invocations for each backend are in `CLAUDE.md`.

## Rules

- Backends emit; they do not optimize. Optimizations go in ZIR.
- Never `exit()` from `src/**` — return a status, let `main.cpp` decide.
- Every language-visible change gets a `.zz` test and a reviewed golden diff.
- `-Werror` is on. Fix the warning; do not cast it away.
