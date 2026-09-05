# Coding Conventions — zpiler

These are enforced by `.clang-format`, `.clang-tidy`, `-Werror`, and review.
When an existing file disagrees with a rule here, match the file locally and fix
the file separately — do not mix a reformat into a behavior change.

## Language & build

- **C++20**, no compiler extensions (`CMAKE_CXX_EXTENSIONS OFF`).
- No exceptions for control flow. `throw` is for "the compiler has a bug"
  (`std::runtime_error` with a message naming the invariant that broke), not for
  user program errors.
- No RTTI-dependent designs; prefer a `kind()`/tag enum, which the code already uses.
- Headers use `#pragma once`. Include what you use; do not lean on `all.hpp`
  outside of `main.cpp` and `.cpp` translation units.

## Naming

| Kind                      | Style              | Example                     |
|---------------------------|--------------------|-----------------------------|
| Type / class / enum       | `PascalCase`       | `ScopeContext`, `TypeInfo`  |
| Enum value                | `PascalCase`       | `NodeType::BinaryOp`        |
| Function / method         | `camelCase`        | `parseExpression`           |
| Local / parameter         | `camelCase`        | `currentScope`              |
| Private data member       | `camelCase_`       | `stackOffset_`              |
| Public data member        | `camelCase`        | `returnType`                |
| Constant / file-static    | `SCREAMING_SNAKE`  | `ARG_GPR_LINUX`             |
| Namespace                 | lowercase          | `zust`                      |
| File                      | `PascalCase.{hpp,cpp}` | `RegisterAllocator.cpp` |

Everything lives in `namespace zust`. `src/` mirrors `include/` exactly.

## Formatting

4-space indent, no tabs. 120-column soft limit. Braces on the same line for
functions and control flow (the majority style in this tree — `.clang-format`
encodes it). Run before committing:

```bash
clang-format -i $(git diff --name-only --diff-filter=ACM | grep -E '\.(cpp|hpp)$')
```

## Ownership & lifetime

- `std::unique_ptr<ASTNode>` **owns**. A function that takes ownership takes it by
  value and the caller `std::move`s.
- Non-owning views are raw `T*` (nullable) or `const T&` (not nullable). Never a
  `shared_ptr` "just in case".
- `std::shared_ptr<ScopeContext>` is the one legitimate shared owner, because the
  scope chain is a DAG referenced from many AST nodes. Do not introduce more.
- Prefer `std::string_view` for read-only string parameters that do not outlive
  the call.

## Errors and diagnostics

- User-facing problems: build a `zust::Error{ErrorType::…, message}` and route it
  through `logError`. Set the phase's `shouldCodegen_`/`shouldTypecheck` flag and
  **keep going** so we report more than one error per run.
- Every message must name a source location (`line`, `column`) and quote what was
  expected. `"Expected ';' after expression at line 4, column 12."` — not `"parse error"`.
- Library code (`src/**`) must never call `exit()` or `std::abort()`. Return a
  status; only `main.cpp` chooses a process exit code.
- Exit codes: `0` success, `1` compilation failure. Reserve other codes for the
  driver, not for phases.

## Adding to the AST

Positional children are the current design; when you add a node type:

1. Add the `NodeType` enum value.
2. Add a `static std::unique_ptr<ASTNode> makeXNode(...)` factory — never build a
   node by hand at a call site.
3. Add named accessors (`getConditionForLoop()`-style) rather than `children[2]`
   at use sites.
4. Extend `ASTNode::print` so `--print-ast` stays complete.
5. Handle it in `TypeChecker::checkNode` **and all three** codegen `switch`es.
   A missing case must be a compile error, so `switch` over `NodeType` without a
   `default:` where practical.

## Backends

- A backend emits; it does not decide. No folding, no reassociation, no dead-store
  elimination in an emitter.
- Anything conditional on target must go through the `TargetABI` data struct
  (see `docs/PRD-ZIR.md` / `docs/BACKENDS.md`) — never `#ifdef _WIN64` inside
  lowering logic. (`main.cpp` selecting a default target is the one exception.)
  `RegisterAllocator` is being replaced by a linear-scan allocator over ZIR and
  should not gain new target knowledge in the meantime.
- Emit through the provided `std::ostringstream&`, not to `std::cout`.
- Keep the assembly commented. The existing `# load canary` style comments are
  what makes generated code debuggable; preserve them.

## Tests

- Every language-visible change ships with a `.zz` case. Put it in the directory
  that matches the feature (`tests/runtime/<feature>/<case>.zz`).
- Keep cases small and single-purpose. A case that tests loops *and* floats *and*
  shadowing tells you nothing when it fails.
- Test programs must be deterministic: no time, no addresses, no uninitialized reads.
- Bless goldens deliberately: run without `--bless`, read the diff, then bless.
- Compile failures need a `.stderr.contains` file listing substrings that must
  appear. Match on the stable part of the message, not the whole line.

## Commits

- One logical change per commit. Formatting, renames, and behavior changes are
  separate commits.
- Message: imperative subject under 72 chars, then *why* in the body. The
  interesting part is never "what" — the diff says that.
- Never commit build output (`build/`, `*.o`, `*.asm`, `*.ll`, `program`, `__pycache__`).

## Performance rules for the compiler itself

- Don't `std::string` in hot paths where a `string_view` or an interned id works.
  The AST's `std::string value` field is a known cost; new code should not add more.
- Prefer `reserve()` on vectors whose final size is known.
- Pass `const T&`, return by value and let NRVO work; don't return `const T`.
- Measure before optimizing the compiler. `docs/OPTIMIZATION.md` describes the
  benchmark harness; a claimed speedup without a number is not a speedup.
