# Pluggable Backends

How a new code generator gets added to zpiler — today (AST-level), and after ZIR
lands (IR-level). The goal is that adding a backend means **writing one backend
and registering it in one line**, with the CLI, the driver, `--formats`, and the
test harness picking it up automatically.

Related: `docs/ARCHITECTURE.md`, `docs/IR-DESIGN.md`, `docs/ROADMAP.md`.

---

## 1. What it costs today

Adding, say, `aarch64-linux` right now requires edits in **13 places across 9
files in 4 layers**:

| # | File | What you must add |
|---|------|-------------------|
| 1 | `include/support/CommandLine.hpp` | `CodegenOutputFormat::AARCH64_LINUX` |
| 2 | `src/support/CommandLine.cpp` | string → enum in `parseArgs` |
| 3 | `src/support/CommandLine.cpp` | a line in `printFormats` |
| 4 | `include/codegen/CodeGen.hpp` | `TargetTriple::AARCH64_LINUX` |
| 5 | `include/codegen/CodeGen.hpp` | the whole `class CodeGenAArch64` declaration |
| 6 | `src/codegen/CodeGen.cpp` | a case in `CodeGen::create` |
| 7 | `main.cpp` | a case mapping `CodegenOutputFormat` → `TargetTriple` |
| 8 | `include/codegen/RegisterAllocator.hpp` | register set constants |
| 9 | `src/codegen/RegisterAllocator.cpp` | a `forAArch64()` factory |
| 10 | `src/codegen/RegisterAllocator.cpp` | a case in `emitSpillRestore` |
| 11 | `src/parser/ScopeContext.cpp` | a case in `allocateSpillSlot` |
| 12 | `tests/conftest.py` | an entry in `TARGETS` |
| 13 | `tests/test_pipeline.py` | branches in `assemble_source` and `link_object` |

### The four structural faults behind that number

**(a) Two enums for one concept.** `CodegenOutputFormat` (CLI-facing) and
`TargetTriple` (codegen-facing) describe the same thing and are mapped by hand in
`main.cpp`. Nothing checks they stay in sync.

**(b) `--formats` is already wrong.** It omitted `llvm-ir` for as long as that
backend has existed — a supported, tested target the CLI denied having. That is
not carelessness, it is the predictable result of a hand-maintained list that
lives three files away from the thing it describes.

**(c) Layering inversions — non-codegen code knows about targets.**

```cpp
// src/parser/ScopeContext.cpp — the SYMBOL TABLE formats assembly operands
std::string FunctionScope::allocateSpillSlot(std::int64_t size, CodegenOutputFormat format) {
    case CodegenOutputFormat::X86_64_LINUX: return "-" + std::to_string(offset) + "(%rbp)";
    case CodegenOutputFormat::X86_64_MSWIN: return "[rbp - " + ... + "]";
}
```

`RegisterAllocator::emitSpillRestore` does the same. So a new target requires
edits to the parser and the register allocator — layers that have no business
knowing a target exists. This is the single biggest obstacle to plugging a
backend in.

**(d) Every backend is declared in the shared header.** `CodeGen.hpp` declares
`CodeGenLinux`, `CodeGenWindows`, and `CodeGenLLVM` in full. It reaches every
translation unit through `all.hpp`, so each new backend grows a header everything
depends on, and touching any backend's private interface rebuilds the world.

---

## 2. Phase A — pluggable at the AST boundary (do now)

Target state: **a backend is one directory plus one registration line.** No CLI
edit, no `main.cpp` edit, no test-harness edit.

### A.1 Prerequisite: evict target knowledge from non-codegen layers

Nothing else works until this is done.

- `FunctionScope::allocateSpillSlot(size)` returns a plain `std::int64_t` frame
  offset. The **backend** formats it (`-24(%rbp)` vs `[rbp-24]`). Delete the
  `CodegenOutputFormat` parameter.
- `RegisterAllocator` stops emitting assembly. `unSpill`/`emitSpillRestore` return
  the slot and let the backend write the instruction. `RegisterAllocator` becomes
  pure bookkeeping over register names.
- After this, `grep -rn CodegenOutputFormat src/parser src/codegen/RegisterAllocator.cpp`
  is empty. Make that a CI check.

This is a pure refactor: behavior identical, goldens unchanged. Land it alone.

### A.2 One target identity

Delete `TargetTriple` and the `CodegenOutputFormat` target values. A target is
identified by its **name string**, which is also the `--format` value. The CLI
stops knowing the set of targets; it passes the string to the registry and
reports the registry's error if it is unknown.

```cpp
enum class AsmSyntax { ATT, Intel, None };   // None = a textual IR, not assembly

struct TargetInfo {
    std::string name;         // "x86_64-linux" — the --format value
    std::string description;  // shown by --formats
    std::string asmExt;       // ".s" / ".asm" / ".ll"
    AsmSyntax   syntax;
    bool        isNative;     // native asm vs. IR-for-another-toolchain

    // How to turn the emitted file into an object and then an executable.
    // "$IN"/"$OUT" are substituted. Consumed by the driver AND the test harness,
    // so the two can never disagree about how to build for a target.
    std::vector<std::string> assembleCmd;  // {"as", "$IN", "-o", "$OUT"}
    std::vector<std::string> linkCmd;      // {"gcc", "$IN", "-o", "$OUT"}
};
```

### A.3 The interface and the registry

```cpp
// include/codegen/Backend.hpp
class Backend {
public:
    virtual ~Backend() = default;
    virtual const TargetInfo &info() const = 0;
    virtual void emit(std::unique_ptr<ASTNode> program, std::ostream &out) = 0;
};

class BackendRegistry {
public:
    using Factory = std::function<std::unique_ptr<Backend>()>;

    static BackendRegistry &instance();

    void registerBackend(TargetInfo info, Factory factory);

    // nullptr when unknown — the CLI turns that into a diagnostic that lists
    // the available names, so a typo is self-correcting.
    std::unique_ptr<Backend> create(std::string_view name) const;

    const TargetInfo *find(std::string_view name) const;
    std::vector<const TargetInfo *> list() const;   // drives --formats
    static std::string hostDefaultName();           // resolves "default"
};
```

`main.cpp` collapses to:

```cpp
auto backend = BackendRegistry::instance().create(cli.getFormat());
if (!backend) { /* diagnostic listing registry.list() */ return 1; }
backend->emit(std::move(program), *outstream);
```

The `switch` over targets, the `#ifdef _WIN64` host detection, and the
`"This should not happen, ACP Pradhyumn..."` fallthrough all disappear.

### A.4 Registration

```cpp
// src/codegen/x86_64/LinuxBackend.cpp
void registerX86_64LinuxBackend(BackendRegistry &r) {
    r.registerBackend(
        TargetInfo{ .name = "x86_64-linux", .description = "x86_64 Linux (System V), GNU as",
                    .asmExt = ".s", .syntax = AsmSyntax::ATT, .isNative = true,
                    .assembleCmd = {"as", "$IN", "-o", "$OUT"},
                    .linkCmd     = {"gcc", "$IN", "-o", "$OUT"} },
        [] { return std::make_unique<LinuxBackend>(); });
}
```

collected in exactly one place:

```cpp
// src/codegen/RegisterBackends.cpp — the ONE line a new backend adds
void registerBuiltinBackends(BackendRegistry &r) {
    registerX86_64LinuxBackend(r);
    registerX86_64WindowsBackend(r);
    registerLLVMIRBackend(r);
}
```

> **Do not use static-initializer self-registration.** It is the obvious design
> and it silently breaks here: `zust_core` is a static library, and a linker drops
> any object file no symbol references, taking its registration with it. The
> backend then vanishes in release builds only. An explicit `registerBuiltinBackends`
> is deterministic, debuggable, and has no static-init-order hazard. (If
> self-registration is ever wanted, the object must be forced in with
> `--whole-archive` / `/WHOLEARCHIVE` or by making `zust_core` an OBJECT library —
> pay that cost knowingly.)

### A.5 Make the test harness read the registry

Today `tests/conftest.py` hardcodes `TARGETS` and `test_pipeline.py` hardcodes the
assemble/link commands per target. Replace both with a query:

```bash
zpiler --formats --json
# [{"name":"x86_64-linux","asmExt":".s","assembleCmd":["as","$IN","-o","$OUT"], ...}]
```

`conftest.py` parses that once per session and builds `TARGETS` from it;
`test_pipeline.py` substitutes `$IN`/`$OUT` instead of branching on target name.
A new backend then gets tested **with no test-harness edit at all**, and the
driver and the tests are guaranteed to build the same way.

### A.6 Split the header

`include/codegen/Backend.hpp` holds `Backend`, `TargetInfo`, `BackendRegistry`.
Each backend's class declaration moves next to its implementation
(`src/codegen/x86_64/LinuxBackend.hpp`) and leaves the shared header. Nothing
outside a backend's own directory should be able to name its class.

### Phase A exit criterion

Adding a backend touches exactly: a new `src/codegen/<target>/` directory, and one
line in `registerBuiltinBackends`. Verified by actually doing it — see §4.
`grep -rn "CodegenOutputFormat\|TargetTriple" src/parser src/support` returns
nothing, and `--formats` is generated from the registry.

---

## 3. Phase B — pluggable at the IR boundary (after ZIR)

Once ZIR exists (`docs/IR-DESIGN.md`, ROADMAP M3), the plug point moves one stage
later. **The registry, the CLI, the driver, and the test harness do not change.**
That is the whole return on doing Phase A first: Phase B is a signature change,
not a re-plumb.

```cpp
class Backend {
public:
    virtual const TargetInfo &info() const = 0;
    virtual void emit(const zir::Module &module, std::ostream &out) = 0;   // was: unique_ptr<ASTNode>
};
```

Note `const zir::Module &` — backends no longer consume (destroy) their input, so
the same module can be emitted to several targets in one run, and a backend can
make more than one pass over it. Both are impossible today, because
`generateStatement(std::unique_ptr<ASTNode>, …)` destroys the AST as it walks.

### B.1 Migration without a red suite

The two interfaces coexist behind a base class during the transition:

1. Add `emit(const zir::Module&, std::ostream&)` as a second virtual with a
   default implementation that throws "backend has not been migrated".
2. The driver prefers the ZIR path when the backend advertises `consumesZIR()`,
   else falls back to the AST path.
3. Migrate LLVM first — ZIR → LLVM IR is nearly 1:1 once ZIR is SSA — and require
   **byte-identical goldens** before switching its default.
4. Then Linux, then Windows.
5. Delete the AST-consuming overload and `consumesZIR()`.

### B.2 Two kinds of backend emerge

| Kind | Consumes | Examples | Effort per new one |
|---|---|---|---|
| **IR-translating** | ZIR directly | LLVM IR, C, WASM text, Cranelift | small — a structural translation |
| **Native** | ZIR + shared machine layer | x86_64 SysV, x86_64 Win64, AArch64 | mostly *data* |

Native backends stop being independent emitters. They share one machine layer —
instruction selection, live-interval construction, linear-scan register
allocation, frame layout, peephole — and differ by a `TargetABI` description:

```cpp
struct TargetABI {
    std::vector<Reg> intArgRegs, floatArgRegs;
    std::vector<Reg> calleeSaved, callerSaved;
    Reg  intReturn, floatReturn;
    int  shadowSpaceBytes;      // 32 on Win64, 0 on SysV
    int  redZoneBytes;          // 128 on SysV, 0 on Win64
    int  stackAlignAtCall;      // 16
    bool argSlotsShared;        // Win64: slot N is GPR-or-XMM, not both
    VariadicFloatRule variadicFloats;  // SysV: AL = vector count; Win64: duplicate into GPR
    AsmSyntax syntax;
};
```

This is exactly the table in `docs/ARCHITECTURE.md` promoted from prose to code.
It is what turns "Linux and Windows are two 1,400-line forks that drift" into "one
backend and two structs", and it is what makes AArch64 tractable — a new ABI
struct and an instruction table rather than a third fork.

### Phase B exit criterion

`CodeGenLinux.cpp` and `CodeGenWindows.cpp` no longer exist as separate emitters;
a native target is a `TargetABI` plus an instruction-selection table. Adding an
IR-translating backend requires no knowledge of registers or stack frames at all.

---

## 4. Prove it with a real backend

A plan for pluggability that is never exercised will not be pluggable. Each phase
ends by adding a genuinely new backend:

- **End of Phase A — a C backend** (`--format c`). It shares nothing with the
  existing three: no registers, no stack frames, no assembler. If the registry can
  host it without special cases, the abstraction is real. It is also independently
  useful (bootstrapping on any platform with a C compiler) and it is cheap.
- **End of Phase B — AArch64 Linux.** Different register file, different
  instruction encoding, same ABI-as-data machinery. This is the real test of the
  `TargetABI` split, and it is where the payoff shows up: it should be
  overwhelmingly a data exercise.

Neither is on the critical path for the language itself. Both are the check that
the abstraction holds, and each should be attempted *only* after its phase's
refactor, so the friction is informative.

## 5. Rules for backend authors

1. A backend emits. It does not optimize, does not rewrite the IR, and does not
   "fix up" what it was handed. A needed fix-up is a missing ZIR pass.
2. A backend owns its target's syntax entirely. No other layer may branch on
   target — no `#ifdef _WIN64` outside host-default detection, no target enum in
   the parser, the symbol table, or the register allocator.
3. A backend declares its toolchain in `TargetInfo`. It does not assume the driver
   knows how to assemble or link for it.
4. A backend's class is private to its own directory.
5. A new backend adds no `if` to any existing file except the one registration line.
6. A backend is not done until it passes the full golden suite. Cross-backend
   disagreement means one of them is wrong — and historically in this project it
   was usually the native one (see `docs/ROADMAP.md` M0-7).
