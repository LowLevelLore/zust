# AGENTS.md

Instructions for AI coding agents working in this repository.

**The full context lives in [`CLAUDE.md`](CLAUDE.md). Read it first.**

Quick orientation:

| Document | Contents |
|---|---|
| `CLAUDE.md` | project context, build/test commands, invariants, known open problems |
| `docs/ARCHITECTURE.md` | how the compiler is structured today and where it's going |
| `docs/CONVENTIONS.md` | naming, formatting, ownership, error handling, commit rules |
| `docs/ROADMAP.md` | milestones M0–M9 toward a full-fledged optimizing compiler |
| `docs/IR-DESIGN.md` | ZIR — the typed SSA IR spec (design stage) |
| `docs/BACKENDS.md` | how to add a code generator; the backend registry plan |
| `docs/OPTIMIZATION.md` | generated-code and compiler performance guidance |
| `.claude/skills/*/SKILL.md` | task playbooks: architecture, language features, IR, ABI, testing, codegen debugging |

## The short version

```bash
cmake -S . -B build -DCMAKE_BUILD_TYPE=RelWithDebInfo && cmake --build build -j
TARGET=linux,llvm python3 -m pytest -q
```

Rules that matter most:

1. **Linux x86_64 and Windows x86_64 are both first-class.** Check all three
   backends (`CodeGenLinux`, `CodeGenWindows`, `CodeGenLLVM`) — they share no code.
2. **Backends emit; they do not optimize.** Optimizations belong in ZIR (`docs/IR-DESIGN.md`).
3. **Every language-visible change ships with a `.zz` test.** Read the golden diff
   before blessing; never blanket-`--bless`.
4. **`-Werror` is on.** Fix the warning, don't cast it away.
5. **No generated files in git.**
