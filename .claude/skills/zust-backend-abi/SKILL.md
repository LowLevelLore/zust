---
name: zust-backend-abi
description: "x86_64 calling conventions and backend emission details for zust — SysV (Linux) vs Win64 (Windows), register allocation, stack frames, variadics, and LLVM IR emission. Use when: writing or fixing code generation, debugging a bug that appears on only one OS, touching RegisterAllocator, or handling function calls, arguments, or stack layout."
---

# Backends & ABI

Three emitters, no shared lowering. If you change one, check the other two.

**Adding a *new* backend? Read `docs/BACKENDS.md` first** — it is the plan for the
registry and for where the plug point lives (AST today, ZIR after M3). This skill
covers working on the backends that exist.

| File | Target | Syntax | Assembler |
|---|---|---|---|
| `src/codegen/CodeGenLinux.cpp` | x86_64 SysV | AT&T (`movq %rax, -8(%rbp)`) | GNU `as` |
| `src/codegen/CodeGenWindows.cpp` | x86_64 Win64 | Intel/MASM (`mov QWORD PTR [rbp-8], rax`) | `ml64` |
| `src/codegen/CodeGenLLVM.cpp` | LLVM IR | textual `.ll` | `llc` |

## The ABI table — this is the whole difference

| Concern | SysV (Linux) | Win64 (Windows) |
|---|---|---|
| Integer args | `rdi rsi rdx rcx r8 r9` | `rcx rdx r8 r9` |
| Float args | `xmm0`–`xmm7` | `xmm0`–`xmm3` |
| Arg slot sharing | GPR and XMM counters are **independent** | slot N is *either* GPR N or XMM N — a float in slot 2 consumes `r8`'s slot |
| Return (int / float) | `rax` / `xmm0` | `rax` / `xmm0` |
| Callee-saved GPR | `rbx r12 r13 r14 r15` (+`rbp`) | `rbx rdi rsi r12 r13 r14 r15` (+`rbp`) |
| Callee-saved XMM | **none** | `xmm6`–`xmm15` |
| Caller-saved GPR | `rax rcx rdx rsi rdi r8 r9 r10 r11` | `rax rcx rdx r8 r9 r10 r11` |
| Shadow space | none | **32 bytes** the caller must reserve before every call |
| Red zone | 128 bytes below `rsp` | none |
| Stack alignment at call | `rsp` ≡ 16 (so ≡ 8 on entry after the return address) | same |
| Variadic floats | `al` = number of vector registers used | float args passed in **both** the XMM and the corresponding GPR |
| Stack args | pushed right to left, above the return address | same, above the 32-byte shadow space |

These constants live in `include/codegen/RegisterAllocator.hpp` as
`ARG_GPR_LINUX` / `ARG_GPR_MSVC` etc. **Any target-conditional behavior belongs
in that table.** An `#ifdef _WIN64` inside lowering logic is a bug — the compiler
must be able to cross-compile (ROADMAP M8).

Two places currently violate that rule and should not be copied:
`ScopeContext::allocateSpillSlot` and `RegisterAllocator::emitSpillRestore` both
switch on `CodegenOutputFormat` to format assembly, which is why the parser and
the register allocator must be edited to add a target. M2.5 removes both — do not
add a third.

### The three that actually bite

1. **`rdi`/`rsi` are callee-saved on Windows, caller-saved on Linux.** Code that
   scratches `rsi` on Linux corrupts the caller on Windows.
2. **Shadow space.** Forgetting the 32-byte reservation corrupts the first four
   stack slots. The symptom is a callee stomping the caller's locals — usually
   looks like an unrelated variable changing value.
3. **Variadic `printf`.** On Linux, `al` must hold the vector-register count or
   glibc's `printf` reads garbage for `%f`. On Windows, a `double` passed to a
   variadic must go in *both* `xmm` and the paired GPR.

## Register allocation (current)

`RegisterAllocator` is per-target (`forSysV()` / `forMSVC()`) and works during
emission with no live-range knowledge.

```cpp
std::string reg = allocateOrSpill(/*isXMM=*/false, scope, out);  // may spill an LRU victim
noteType(reg, typeInfo);                                          // record type for width/signedness
// ... emit ...
alloc.free(reg);                                                  // caller of an expression frees it
```

Rules:

- **Whoever consumes an expression's result frees the register.** `emitExpression`
  returns a live register; leaking it exhausts the pool.
- `allocateOrSpill` may spill; `restoreIfSpilled(reg, scope, out)` brings it back.
  Anything you hold across a nested `emitExpression` call may have been spilled.
- Use `adjustReg(r64, bits)` to name the right sub-register (`rax`→`eax`→`ax`→`al`)
  and `getCorrectMove(bytes, isFloat)` for the right mnemonic.
- Argument registers are allocated separately (`allocateArgument(position)`) so
  they are not stolen mid-call-setup.

This design is a known limitation (see `docs/OPTIMIZATION.md`) — it cannot keep a
value in a register across a statement boundary. It is replaced by a linear-scan
allocator over ZIR live intervals at ROADMAP M5. **Do not invest in improving it**
beyond correctness fixes.

## Stack frame (current)

```asm
push   %rbp
mov    %rsp, %rbp
movabs $0x<canary>, %r10        # per-function random canary
movq   %r10, -8(%rbp)           # stored at [rbp-8]
sub    $<frame>, %rsp           # locals + spill slots
push   %rbx / %r12 / %r13 / %r14 / %r15
push   %r15                     # alignment filler
```

- `[rbp-8]` is always the canary; locals start at `[rbp-16]`. `FunctionScope::allocateStack`
  enforces this.
- `BlockScope::allocateStack` delegates upward — block locals live in the enclosing
  function's frame, so a frame is sized once for the whole function.
- Epilogue reloads the canary, compares, and jumps to `__stack_smash_detected`
  (exit code 69) on mismatch. `__division_by_zero` (exit 70) is the other runtime stub.
- **Known inefficiency:** callee-saved registers are pushed unconditionally, even
  in leaf functions that touch none. See `docs/OPTIMIZATION.md`.

## LLVM backend notes

- Emits textual LLVM IR; `llc -filetype=obj` then `gcc -no-pie`. The `-no-pie` is
  required because emitted globals assume absolute addressing.
- Locals are `alloca` + `load`/`store`; LLVM's own `mem2reg` cleans that up, which
  is why the LLVM backend often produces better code than the native ones today.
- String literals are interned in `stringLiterals` — do this in the native
  backends too (they currently emit a duplicate `.Lstr` per use site).
- SSA names come from `ScopeContext::getMapping`, so a name-mangling bug shows up
  here as an LLVM verifier error rather than silently wrong code. Useful signal.

## Verifying a backend change

```bash
cmake --build build -j
./build/zpiler --format x86_64-linux -o /tmp/t.s prog.zz && cat /tmp/t.s
as /tmp/t.s -o /tmp/t.o && gcc /tmp/t.o -o /tmp/t && /tmp/t; echo "exit=$?"

# cross-check against the LLVM backend — if they disagree, one of them is wrong
./build/zpiler --format llvm-ir -o /tmp/t.ll prog.zz
llc -filetype=obj /tmp/t.ll -o /tmp/tl.o && gcc /tmp/tl.o -o /tmp/tl -no-pie && /tmp/tl

TARGET=linux,llvm python3 -m pytest -q
```

Disagreement between the native and LLVM backends on the same program is the
fastest bug-finder available until the ZIR interpreter lands (ROADMAP M3).

For Windows without a Windows machine: CI is the check. Do not guess at MASM
syntax — read the existing emission in `CodeGenWindows.cpp` and mirror it exactly.

## Reading generated assembly

`gdb` on the generated binary, or:

```bash
objdump -d --no-show-raw-insn /tmp/t | less     # what actually got assembled
gdb -q /tmp/t -ex 'break main' -ex run -ex 'layout asm'
```

Trace stack corruption by watching `[rbp-8]` — if the canary check fires
(exit 69), something wrote past a local.
