---
description: Bless golden expectations for specific test cases, showing the diff for review
---

Bless goldens for the cases named in $ARGUMENTS (a pytest `-k` filter). Never
blanket-bless the whole suite.

```bash
cmake --build build -j
git diff --stat tests/expected/
TARGET=linux,llvm python3 -m pytest -q -k "$ARGUMENTS" --bless
git diff tests/expected/
```

Then:

1. **Show the user the full golden diff** and explain what changed and why the new
   output is correct.
2. Re-run without `--bless` on all targets to confirm the suite is green.
3. If a golden changed for a test you did not intend to touch, stop — that is a
   regression, not a blessing.
