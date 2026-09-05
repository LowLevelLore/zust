# CLAUDE.md — zust / zpiler

Context for AI agents working in this repository. Read this first; then load the
relevant skill from `.claude/skills/`.

## What this is

**zust** is a statically typed, compiled language. **zpiler** is its compiler
(C++20). Source files use the `.zz` extension.

Today the compiler is a **four-stage, AST-walking** pipeline:

```
.zz ──▶ Lexer ──▶ Parser (+ScopeContext) ──▶ TypeChecker ──▶ CodeGen{Linux,Windows,LLVM}
```

We are migrating to a **five-stage** pipeline with a real intermediate
representation, so optimizations live in one place and backends stay thin:

```
.zz ──▶ Lexer ──▶ Parser ──▶ Sema (resolve + typecheck) ──▶ ZIR (typed SSA)
                                                             │
                                          ┌──────────────────┼──────────────────┐
                                          ▼                  ▼                  ▼
                                    LLVM IR (.ll)   x86_64 SysV (.s)   x86_64 Win64 (.asm)
```

`docs/IR-DESIGN.md` is the spec for ZIR. `docs/ROADMAP.md` is the milestone plan.
**Do not add new optimizations to the backends.** They belong in ZIR passes.

## Non-negotiable invariants

1. **Linux x86_64 and Windows x86_64 are both first-class.** A change that only
   works on one is not done. Every backend-visible change needs green CI on both.
2. **Backends are dumb.** Lowering decisions (constant folding, strength
   reduction, dead-code removal) happen in ZIR passes, never in an emitter.
   Backends are also *pluggable*: no layer outside `src/codegen/` may branch on
   target. See `docs/BACKENDS.md`.
3. **The compiler never crashes on bad input.** Malformed programs produce
   diagnostics and a nonzero exit code, not an uncaught exception or a segfault.
4. **Golden tests are the contract.** Behavior changes must come with updated
   `tests/expected/**` in the same commit, blessed deliberately (never blanket
   `--bless` a whole run without reading the diff).
5. **No generated artifacts in git.** Build output belongs in `build/`.

## Build

```bash
cmake -S . -B build -DCMAKE_BUILD_TYPE=RelWithDebInfo
cmake --build build -j
```

Useful options (see `CMakeLists.txt`): `-DZUST_SANITIZE=address,undefined`,
`-DZUST_ENABLE_LTO=ON`, `-DZUST_WERROR=OFF`, `-DZUST_BUILD_TESTS=ON`.

The binary lands at `build/zpiler` (`build/zpiler.exe` on Windows).

## Test

```bash
python3 -m pytest -q                 # native target, auto-detected
TARGET=linux,llvm python3 -m pytest -q
TARGET=windows  python -m pytest -q
python3 -m pytest -q -k shadowing    # one case
```

Three suites, all directory-driven (see `.claude/skills/zust-testing/`):

| Directory             | Contract                                                  |
|-----------------------|-----------------------------------------------------------|
| `tests/runtime`       | compiles, runs, exact stdout/stderr/exit code              |
| `tests/runtime_fail`  | compiles, runs, exact output + **nonzero** exit            |
| `tests/compile_fail`  | fails to compile with expected exit + stderr substrings    |

Expectations mirror the source path under `tests/expected/<mode>/`. A
`<name>.<target>.stdout` overrides the shared `<name>.stdout` for one backend.

## Run a program end to end

```bash
./build/zpiler --format x86_64-linux -o out.s prog.zz && as out.s -o out.o && gcc out.o -o prog && ./prog
./build/zpiler --format llvm-ir      -o out.ll prog.zz && llc -filetype=obj out.ll -o out.o && gcc out.o -o prog -no-pie && ./prog
./build/zpiler --format x86_64-mswin -o out.asm prog.zz && ml64 /nologo /c out.asm && gcc out.obj -o prog.exe && ./prog.exe
```

## Layout

| Path                       | What lives there                                        |
|----------------------------|---------------------------------------------------------|
| `include/`, `src/`         | compiler, mirrored directory structure                  |
| `src/lexer/`               | tokenization                                            |
| `src/parser/`              | recursive-descent parser, `ScopeContext` symbol tables   |
| `src/ast/`                 | `ASTNode` — one struct, `NodeType` tag, `children` vector |
| `src/typechecker/`         | type validation, promotion rules                        |
| `src/codegen/`             | three emitters + `RegisterAllocator`                    |
| `src/common/`, `src/support/` | diagnostics, logging, strings, CLI, file I/O          |
| `tests/`                   | pytest harness + `.zz` cases + goldens                  |
| `docs/`                    | architecture, conventions, roadmap, IR spec             |
| `.claude/skills/`          | task-specific playbooks — load one before deep work     |

## Conventions

Full rules in `docs/CONVENTIONS.md`. The short version:

- C++20. 4-space indent, `.clang-format` is authoritative — run it before commit.
- `PascalCase` types, `camelCase` functions/locals, `member_` trailing underscore
  for private data, `SCREAMING_CASE` constants.
- Everything in `namespace zust`.
- `std::unique_ptr` owns AST nodes; pass raw `T*` or `const T&` for non-owning views.
- Errors go through `zust::Error`/`logError`. Do not `exit()` from library code —
  return a status and let `main.cpp` decide.
- New warnings are errors (`-Werror`). Do not silence a warning with a cast; fix it.

## Known open problems

- **Block scoping is currently disabled.** `parseConditionals`/`parseForLoop`/
  `parseWhileLoop` no longer push a `BlockScope`, so `let` inside an `if`/loop body
  leaks into the enclosing function scope and cannot shadow. `tests/runtime/variables/shadowing.zz`
  was rewritten to dodge this. This is tracked as **M0-1** in `docs/ROADMAP.md`
  and must be fixed properly (scope push + unique mangled names), not worked around.
- AST nodes carry no source spans, so diagnostics can only cite the current token.
- `TypeChecker` and name resolution are entangled with parsing; declaration order matters.
- The register allocator is a per-expression LRU with ad-hoc spilling, not a real
  live-range allocator. Replace it at the ZIR stage, not before.
- **Adding a backend costs 13 edits across 9 files**, because target knowledge has
  leaked into the parser (`ScopeContext::allocateSpillSlot`) and the register
  allocator (`emitSpillRestore`), and two parallel enums (`CodegenOutputFormat`,
  `TargetTriple`) are mapped by hand in `main.cpp`. Tracked as M2.5; plan in
  `docs/BACKENDS.md`.

## Working agreements for agents

- Prefer the smallest change that satisfies the contract; this codebase already
  has enough half-migrated surface.
- When touching codegen, always check **all three** emitters. They do not share code
  and diverge silently.
- Add a `.zz` test for every language-visible change. Read the golden diff before blessing.
- If you find a second bug while fixing the first, report it — do not fold unrelated
  fixes into one commit.
