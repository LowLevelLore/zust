---
name: zust-language-feature
description: "End-to-end checklist for adding or changing a zust language feature — new syntax, operator, statement, type, or construct. Use when: implementing a language feature, adding a keyword or operator, changing grammar, or when a feature works in one backend but not the others."
---

# Adding a Language Feature to zust

A feature touches every phase. Skipping one produces a compiler that accepts the
syntax and then miscompiles it silently — the worst failure mode. Work the
checklist top to bottom.

## Before you write code

1. **Write the test first.** Create `tests/runtime/<area>/<feature>.zz` with the
   smallest program that exercises it and prints an unambiguous result. You cannot
   bless a golden you cannot predict by hand.
2. **Write the failure test.** `tests/compile_fail/<area>/<bad_case>.zz` plus a
   `.stderr.contains` file — what should the error say when someone gets it wrong?
3. **Decide the semantics in writing.** Evaluation order, type rules, and what is
   an error. Put it in a comment on the test if there is no doc yet.

## The checklist

### 1. Lexer — `src/lexer/Lexer.cpp`, `include/lexer/Lexer.hpp`

- Add the `Token::Kind` enum value.
- Add the case to the private `kindToString` — this is what debug output prints,
  and a missing case silently prints "Invalid".
- Keyword? Add it to the keyword table in `scanIdentifierOrKeywordOrConditional`.
  Operator? Add it to `scanSymbol`, **longest match first** (`>=` before `>`,
  `...` before `.`).

### 2. AST — `include/ast/ASTNode.hpp`, `src/ast/ASTNode.cpp`

- Add the `NodeType` value.
- Add a `static makeXNode(...)` factory. Never construct nodes inline at a call site.
- Add named accessors for the positional children. Follow
  `getConditionForLoop()`/`getBodyForLoop()` — no `children[N]` at use sites.
- Extend `ASTNode::print` so `--print-ast` stays complete. This is your primary
  debugging tool for the next step.

### 3. Parser — `src/parser/Parser.cpp`

- Add the parse function, and dispatch to it from `parseStatement` (statements) or
  `parsePrimary`/`parseUnary` (expressions).
- Binary operator? Add it to `getPrecedence` — precedence climbing in
  `parseBinaryRHS` reads it from there.
- On error: `expect(kind, "Expected X at line …, column …")`, set
  `shouldTypecheck = false`, and **keep parsing** so we report more than one error.
- Introducing a scope? `enterScope(name, isFunction)` / `exitScope()` around the
  body. Bodies of `if`/`for`/`while` **should** do this (currently they do not —
  see ROADMAP M0-1).
- Declaring a name? `currentScope->defineVariable/defineFunction/defineType`, and
  `setMapping` with a `GLOBAL_NAME_MAPPER` mangled name so shadowing works.

### 4. TypeChecker — `src/typechecker/TypeChecker.cpp`

- Add the `case` in `checkNode`. Return the resulting type name, or `""` after
  reporting an error.
- On a type error: `logError(Error(ErrorType::Type, …))` **and** set
  `shouldCodegen_ = false`. Do not throw — throwing skips the rest of the file's
  diagnostics.
- New numeric type? Add its name to `numeric_types` / `integral_types` in
  `include/typechecker/TypeChecker.hpp` and make sure `promoteType` handles it.

### 5. CodeGen — **all three**, they share nothing

- `src/codegen/CodeGenLinux.cpp` — GAS, AT&T syntax, SysV ABI
- `src/codegen/CodeGenWindows.cpp` — MASM, Intel syntax, Win64 ABI
- `src/codegen/CodeGenLLVM.cpp` — textual LLVM IR

For each: add the virtual to `include/codegen/CodeGen.hpp` if the feature needs a
new entry point, then add the `case` to `generateStatement` (statements) or
`emitExpression` (expressions).

Emitter rules:

- Statements return `void`; expressions return the register (or LLVM SSA name)
  holding the result, and the **caller** frees it.
- Allocate via `allocateOrSpill(isXMM, scope, out)`, free via `alloc.free(reg)`.
  Leaking a register exhausts the pool and produces a confusing "no registers"
  failure far from the cause.
- Record the type of anything you put in a register: `noteType(reg, typeInfo)`.
  Later code reads `regType` to pick the right move width and signedness.
- Around a call, caller-saved registers must be saved. See `zust-backend-abi`.
- Comment your emitted assembly (`# what this does`) — matching the existing style.
- Do not optimize in the emitter. Constant folding belongs in ZIR.

### 6. Tests and docs

- Run without `--bless`, **read the diff**, then bless:
  `TARGET=linux,llvm python3 -m pytest -q -k <feature>` then add `--bless`.
- Verify on Windows too, or state explicitly in the PR that it is unverified.
- Update `README.md`'s feature list and the grammar/spec if the syntax is new.

## Verification loop

```bash
cmake --build build -j
./build/zpiler --print-ast tests/runtime/<area>/<feature>.zz     # is the AST right?
./build/zpiler --format x86_64-linux -o /tmp/t.s tests/runtime/<area>/<feature>.zz
cat /tmp/t.s                                                     # is the asm sane?
as /tmp/t.s -o /tmp/t.o && gcc /tmp/t.o -o /tmp/t && /tmp/t; echo "exit=$?"
TARGET=linux,llvm python3 -m pytest -q -k <feature>
```

If the AST is right and the program is wrong, the bug is in codegen — switch to
the `zust-debug-codegen` skill.

## Common mistakes in this codebase

- **Fixing one backend and forgetting the other two.** The Linux one is easiest to
  test locally, so Windows silently rots. Grep for the sibling function name in all
  three files before declaring done.
- **Forgetting `kindToString`** — debug output lies about the token forever after.
- **Forgetting `ASTNode::print`** — `--print-ast` silently omits your node, and
  then you cannot debug it.
- **Freeing a register twice, or not at all.** Both are silent until an unrelated
  test fails.
- **Assuming 64-bit.** The move width comes from `TypeInfo::bits`; use
  `getCorrectMove(bits/8, isFloat)` and `adjustReg(reg, bits)`.
- **Adding the `NodeType` but not the codegen `case`.** Falls through to a default
  that either throws at runtime or emits nothing. Prefer `switch` with no
  `default:` so the compiler tells you.
