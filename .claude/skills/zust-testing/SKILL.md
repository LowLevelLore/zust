---
name: zust-testing
description: "The zust golden-output test harness — adding cases, blessing expectations, per-target overrides, and debugging failures. Use when: adding or fixing tests, a pytest case fails, blessing goldens, or setting up test coverage for a new feature."
---

# Testing zust

Directory-driven golden tests via pytest. `tests/conftest.py` handles targets and
`--bless`; `tests/test_pipeline.py` runs compile → assemble → link → execute and
compares bytes.

## The three suites

| Directory | Contract |
|---|---|
| `tests/runtime/**/*.zz` | must compile, run, and match exact stdout, stderr, and exit code |
| `tests/runtime_fail/**/*.zz` | must compile and run, match exact output, and exit **nonzero** |
| `tests/compile_fail/**/*.zz` | must **fail** to compile with the expected exit code and required stderr substrings |

Expectations mirror the source path under `tests/expected/<mode>/`:

```
tests/runtime/loops/nested.zz
  → tests/expected/runtime/loops/nested.stdout
    tests/expected/runtime/loops/nested.stderr
    tests/expected/runtime/loops/nested.exitcode
```

**Per-target override:** `nested.linux.stdout` wins over `nested.stdout` when
`target=linux`. Use this only when output genuinely must differ per backend
(pointer widths, float formatting) — a divergence you did not intend is a bug, not
a reason to add an override file.

Line endings are normalized (`\r\n` → `\n`) before comparison, so Windows and
Linux share goldens by default.

## Targets

```bash
python3 -m pytest -q                       # auto-detect the native target
TARGET=linux,llvm python3 -m pytest -q     # both Linux-hostable backends
TARGET=windows  python -m pytest -q        # Windows
python3 -m pytest -q -k shadowing          # one case by name
python3 -m pytest -q -x                    # stop at first failure
```

Each target needs its toolchain: `as`+`gcc` (linux), `llc`+`gcc` (llvm),
`ml64`+`gcc` (windows). A missing tool shows up as an "Assemble failed" error, not
a skip.

## Adding a runtime test

1. Write the smallest `.zz` program that exercises one thing and prints an
   unambiguous result:

```zust
// tests/runtime/operations/shift.zz
extern fn printf(fmt: string, ...) -> int32_t;

fn main() {
    let a: int32_t = 1;
    printf("%d\n", a);
}
```

2. **Predict the output by hand.** If you cannot, the test is too big.
3. Run without `--bless` — it fails with "Missing expected file".
4. Bless just that case and read what got written:

```bash
python3 -m pytest -q -k shift --bless
cat tests/expected/runtime/operations/shift.stdout
```

5. Confirm the blessed value matches your prediction. If it does not, you found a
   bug — fix it; do not enshrine it in a golden.
6. Re-run without `--bless` on **all** targets to confirm agreement.

## Adding a compile_fail test

Needs the program plus a `.stderr.contains` file — one required substring per
line, `#` comments allowed:

```
# tests/expected/compile_fail/type/bad_assignment.stderr.contains
Type
bad_assignment
```

Match the **stable** part of the message (the error kind, the identifier), not the
full sentence with line numbers — otherwise every diagnostic reword breaks the test.

`.exitcode` is optional and defaults to `1`.

## Blessing — the rules

`--bless` rewrites goldens to whatever the compiler currently produces. That is a
loaded gun.

- **Never** `pytest --bless` across the whole suite to make a red run green.
- Bless one case at a time, with `-k`.
- Always `git diff tests/expected/` and read every changed byte before committing.
- A golden change in a test you did not intend to touch means you broke something.

## Debugging a failure

The harness prints the failing command and both streams. To reproduce by hand:

```bash
./build/zpiler --format x86_64-linux -o /tmp/t.s tests/runtime/<path>.zz
as /tmp/t.s -o /tmp/t.o && gcc /tmp/t.o -o /tmp/t && /tmp/t; echo "exit=$?"
```

Triage order:

| Failure stage | Meaning | Next step |
|---|---|---|
| Compile failed | frontend rejected valid code, or crashed | `--print-ast`; is the parse right? |
| Assemble failed | we emitted invalid assembly | read `/tmp/t.s` around the reported line |
| Link failed | missing symbol, or `main` not global | check `.globl main` and extern decls |
| stdout mismatch | miscompilation | compare against the `llvm` target — if LLVM is right, the native backend is wrong |
| exit 69 | stack canary tripped — something wrote past a local | check frame size and `allocateStack` |
| exit 70 | division by zero at runtime | intended, or a codegen bug in the divisor |
| Segfault | usually a bad stack frame or ABI violation | `zust-backend-abi` skill |

**Cross-backend disagreement is the highest-signal bug report available.** If
`TARGET=linux` and `TARGET=llvm` produce different output for the same program,
exactly one of them is wrong and the diff tells you where to look.

## Coverage gaps worth filling

The suite currently covers variables, operations, conditionals, loops, functions,
strings, and types. Missing, in rough priority order:

- deep recursion / stack growth
- integer overflow and wraparound at each width
- float edge cases (NaN, inf, -0.0, denormals, float↔int conversion boundaries)
- many-argument calls (>6 args on Linux, >4 on Windows — forces stack passing)
- variadic calls mixing ints and doubles
- deeply nested scopes and shadowing at every level (blocked on ROADMAP M0-1)
- empty function bodies, empty loop bodies, unreachable code after `return`
- very long expressions that exhaust the register pool and force spilling
- `compile_fail` coverage for every diagnostic the compiler can emit

## Harness maintenance

`test_runner.py` and `generate_expected_outputs.py` in the repo root are the older
harness, superseded by pytest. They are scheduled for deletion (ROADMAP M0-6).
Add new tests to the pytest harness only.
