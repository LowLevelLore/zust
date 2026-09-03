---
description: Build zpiler and run the golden test suite; optionally filter with -k
---

Build, then run the tests. If arguments are given, pass them to pytest (e.g. a
`-k` filter or a path).

```bash
cmake --build build -j && TARGET=linux,llvm python3 -m pytest -q $ARGUMENTS
```

If tests fail:

1. Report which cases failed and at which stage (compile / assemble / link / output mismatch).
2. For an output mismatch, cross-check the `linux` and `llvm` targets — if they
   disagree, exactly one backend is wrong and that tells you where to look.
3. Do **not** run `--bless` to make a failure go away. A golden changes only when
   the behavior change was intended and the diff has been read.

Load the `zust-testing` skill for triage detail, or `zust-debug-codegen` if a
program compiles but produces wrong output.
