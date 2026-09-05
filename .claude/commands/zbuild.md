---
description: Configure and build zpiler, reporting any warnings or errors
---

Build the compiler:

```bash
cmake -S . -B build -DCMAKE_BUILD_TYPE=RelWithDebInfo && cmake --build build -j
```

If configuration or compilation fails, read the first error (not the last — later
errors are usually cascades), fix it, and rebuild. `-Werror` is on, so warnings
are failures; fix the underlying issue rather than suppressing it.

Report: whether it succeeded, and the first real error if not.

$ARGUMENTS
