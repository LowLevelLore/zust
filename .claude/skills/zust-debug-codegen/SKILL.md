---
name: zust-debug-codegen
description: "Systematic debugging for zust miscompilations — the program compiles but produces wrong output, crashes, or behaves differently across backends. Use when: a runtime test fails, generated assembly looks wrong, a segfault or canary trip occurs, or two backends disagree."
---

# Debugging a Miscompilation

The program compiled, so the frontend accepted it. The question is which of the
five things below went wrong. Work them in order — each step is cheaper than the
next.

## Step 0 — Shrink the program

A miscompilation in a 40-line test is nearly impossible to read in assembly.
Delete statements until it stops reproducing, then put the last one back. Aim for
under 10 lines. Everything below is much easier on a small case.

## Step 1 — Is the AST right?

```bash
./build/zpiler --print-ast /tmp/min.zz
```

Look for wrong operator nesting (precedence bug), a missing node, or a node in the
wrong scope. **If the AST is wrong, stop** — the bug is in the lexer or parser and
nothing in this skill applies.

## Step 2 — Cross-check the backends

The highest-signal test available:

```bash
./build/zpiler --format llvm-ir -o /tmp/m.ll /tmp/min.zz
llc -filetype=obj /tmp/m.ll -o /tmp/ml.o && gcc /tmp/ml.o -o /tmp/ml -no-pie && /tmp/ml

./build/zpiler --format x86_64-linux -o /tmp/m.s /tmp/min.zz
as /tmp/m.s -o /tmp/m.o && gcc /tmp/m.o -o /tmp/m && /tmp/m
```

| Result | Conclusion |
|---|---|
| Both wrong, same way | Bug is upstream of codegen — TypeChecker or the shared AST/scope state |
| LLVM right, native wrong | Native emitter bug. Read the assembly (step 3) |
| Native right, LLVM wrong | LLVM emitter bug — usually a name-mangling or type-string error; `llc` often reports it |
| Both wrong, differently | Two independent bugs, or shared-state corruption in `ScopeContext` |

## Step 3 — Read the assembly

```bash
./build/zpiler --format x86_64-linux -o /tmp/m.s /tmp/min.zz && cat /tmp/m.s
objdump -d --no-show-raw-insn /tmp/m | less
```

The emitted comments (`# load canary`, `# save callee-saved GPR`) map instructions
back to the emitter that produced them — grep the comment string in
`src/codegen/CodeGenLinux.cpp` to find the code.

What to look for, in order of likelihood:

1. **Wrong operand width.** `movl` where `movq` was needed, or `%eax` where `%rax`
   was needed. Cause: `TypeInfo::bits` not threaded through, or a missing
   `adjustReg(reg, bits)`. Symptom: correct small values, garbage above 2^32, or a
   value that loses its sign.

2. **Missing sign extension.** `movl` into a 32-bit register zeroes the upper half;
   a signed narrow value needs `movslq`/`movsx`. Symptom: negative numbers become
   huge positives.

3. **Register clobbered across a call.** A value in a caller-saved register
   (`rax rcx rdx rsi rdi r8-r11` on Linux) that survives a `call` is gone.
   Symptom: a variable changes value right after a function call. See `zust-backend-abi`.

4. **Register freed twice or leaked.** Look for the same register being written by
   two live values. Symptom: two variables share a value.

5. **Wrong stack offset.** Check the `-N(%rbp)` against `FunctionScope::allocateStack`.
   Remember `[rbp-8]` is the canary and locals start at `-16`. Symptom: reading a
   neighbouring variable, or a canary trip.

6. **Missing spill restore.** A value spilled by `allocateOrSpill` and used
   without `restoreIfSpilled`. Symptom: stale value, intermittent by expression complexity.

7. **Wrong comparison mnemonic.** Signed `setl`/`setg` used on unsigned types (or
   vice versa) — `assembly_comparison_operations` vs
   `unsigned_assembly_comparison_operations` in `CodeGen.hpp`. Symptom: comparisons
   flip near the sign boundary only.

## Step 4 — Exit-code shortcuts

| Exit | Meaning | Look at |
|---|---|---|
| 69 | Stack canary mismatch — something wrote past a local | frame size, `allocateStack`, spill slot overlap |
| 70 | Division by zero at runtime | is the divisor register actually holding the divisor? `idiv` needs `rdx:rax` set up |
| 139 / SIGSEGV | Bad stack frame or ABI violation | prologue/epilogue balance, shadow space on Windows, stack alignment at `call` |
| 132 / SIGILL | Emitted garbage bytes | usually a label emitted into `.text` where an instruction belongs |

## Step 5 — Run it under a debugger

```bash
gdb -q /tmp/m \
  -ex 'break main' -ex run \
  -ex 'layout asm' -ex 'info registers'
```

Useful once you have a suspect instruction:

```
(gdb) x/16gx $rbp-64      # dump the local area
(gdb) watch *(long*)($rbp-8)   # catch the canary being overwritten
(gdb) stepi
```

The canary watchpoint is the fastest way to find which store is out of bounds.

## Step 6 — Check stack alignment

`rsp` must be 16-byte aligned at the point of a `call` (so 8 mod 16 on entry,
after the return address is pushed). The prologue's `push %r15  # just for
alignment` exists for this. An odd number of pushes, or a `sub $N, %rsp` where N
is not a multiple of 16, breaks any callee using SSE — glibc's `printf` will
segfault on a movaps.

Check: count pushes in the prologue, add the `sub` amount, verify the total is
even/aligned.

## Step 7 — Isolate shared state

`ScopeContext`, `regType`, and the process-global `GLOBAL_NAME_MAPPER` persist
across the whole compilation. A bug that only appears when a test has **two**
functions, or only in the second of two similar statements, is almost always
stale shared state — a `noteType` never overwritten, a mapping reused, a register
not freed at the end of the previous statement.

Reproduce by duplicating the failing statement; if the second copy behaves
differently from the first, it is state.

## When you find it

- Add the minimized program as a permanent test case (`zust-testing` skill).
- Check whether the same bug exists in the other two backends — it usually does,
  in a different form.
- If the root cause is structural (register allocator, no live ranges, no IR),
  note it against the relevant milestone in `docs/ROADMAP.md` rather than patching
  around it repeatedly.
