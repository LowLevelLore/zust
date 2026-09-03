---
description: Report progress against the ZIR migration milestones in docs/ROADMAP.md
---

Assess where the ZIR migration actually stands versus `docs/ROADMAP.md`.

Check the tree for what exists:
- Is there an `include/ir/` or `src/ir/` directory? What's in it?
- Does `--emit=zir` exist in `src/support/CommandLine.cpp`?
- Is there a verifier, printer, parser, interpreter, pass manager?
- Do any backends consume ZIR, or are they all still AST-walking?
- Are there `tests/zir/` cases?

Then report, concisely:
1. Which roadmap milestone we are actually on (M0–M9), with evidence.
2. What the exit criterion for that milestone is, and what remains to satisfy it.
3. The single next concrete piece of work, sized to one commit.

Do not start implementing. This command reports status.
