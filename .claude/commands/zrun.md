---
description: Compile and run a .zz file end to end on the native backend
---

Compile and run the `.zz` file given in $ARGUMENTS through the full pipeline:

```bash
cmake --build build -j
./build/zpiler --format x86_64-linux -o /tmp/zrun.s "$ARGUMENTS" \
  && as /tmp/zrun.s -o /tmp/zrun.o \
  && gcc /tmp/zrun.o -o /tmp/zrun \
  && /tmp/zrun; echo "exit=$?"
```

Report the program's stdout, stderr, and exit code.

If it fails, say which stage failed. Useful follow-ups:
- `./build/zpiler --print-ast <file>` — did the parser get it right?
- `cat /tmp/zrun.s` — is the generated assembly sane?
- Compare against `--format llvm-ir` to see whether the backends disagree.

Exit code 69 means the stack canary tripped; 70 means division by zero.
