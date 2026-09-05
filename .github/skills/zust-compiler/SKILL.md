---
name: zust-compiler
description: "Domain knowledge for the zust compiler project. Use when: understanding the compiler architecture, working with lexer/parser/typechecker/codegen components, analyzing the compilation pipeline, debugging assembly generation, or modifying language features."
---

# Zust Compiler

This is a pointer. The maintained documentation lives in the repository root:

- **`CLAUDE.md`** — project context, build/test commands, invariants, open problems
- **`AGENTS.md`** — the short version of the rules
- **`docs/ARCHITECTURE.md`** — pipeline structure and known design debt
- **`docs/CONVENTIONS.md`** — coding standards
- **`docs/ROADMAP.md`** — milestones M0–M9
- **`docs/IR-DESIGN.md`** — ZIR, the typed SSA IR (design stage)
- **`docs/BACKENDS.md`** — adding a code generator; the pluggable-backend plan
- **`docs/OPTIMIZATION.md`** — performance guidance
- **`.claude/skills/`** — task playbooks (language features, IR, ABI, testing, codegen debugging)

Read `CLAUDE.md` first. Do not duplicate its content here — a second copy drifts
out of date and misleads.

## Ten-second orientation

```
.zz ──▶ Lexer ──▶ Parser (+ScopeContext) ──▶ TypeChecker ──▶ CodeGen{Linux,Windows,LLVM}
```

Migrating to `… ──▶ Sema ──▶ ZIR ──▶ passes ──▶ backends`.

```bash
cmake -S . -B build -DCMAKE_BUILD_TYPE=RelWithDebInfo && cmake --build build -j
TARGET=linux,llvm python3 -m pytest -q
```

The three code generators share no lowering logic. A change to one almost always
needs the equivalent change to the other two.
