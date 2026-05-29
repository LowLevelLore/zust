---
name: zust-compiler
description: "Domain knowledge for the zust compiler project. Use when: understanding the compiler architecture, working with lexer/parser/typechecker/codegen components, analyzing the compilation pipeline, debugging assembly generation, or modifying language features."
---

# Zust Compiler Project Guide

## Project Overview

**Zust** is a lightweight statically typed programming language that compiles to **x86_64 Linux assembly**. It supports custom data types, conditionals, arithmetic operations, type-safe operations, and control flow. The language is designed for educational and experimental purposes with emphasis on low-level control and code generation.

## Compilation Pipeline

The compiler transforms `.zz` source files through the following stages:

```
.zz source file
    ↓
[Lexer] - Tokenization
    ↓
[Parser] - Syntax tree building (AST)
    ↓
[TypeChecker] - Type validation & symbol resolution
    ↓
[CodeGen] - x86_64 assembly generation
    ↓
.elf executable (ELF format)
```

## Project Structure

### Core Compiler Components

| Component | Files | Purpose |
|-----------|-------|---------|
| **Lexer** | `lexer/Lexer.hpp/.cpp` | Tokenizes source code into meaningful tokens |
| **Parser** | `parser/Parser.hpp/.cpp` | Builds Abstract Syntax Tree (AST) from tokens, handles scope via `ScopeContext` |
| **TypeChecker** | `typechecker/TypeChecker.hpp/.cpp` | Validates types, resolves symbols, manages symbol tables |
| **CodeGen** | `codegen/CodeGen*.hpp/.cpp` | Generates x86_64 assembly code; platform-specific variants for Linux/Windows |
| **RegisterAllocator** | `codegen/RegisterAllocator.hpp/.cpp` | Manages x86_64 register allocation and spilling |
| **AST** | `ast/ASTNode.hpp/.cpp` | Abstract Syntax Tree node definitions |

### Support Modules

| Module | Purpose |
|--------|---------|
| `common/Logging` | Debug output and logging utilities |
| `common/StringUtils` | String manipulation helpers |
| `common/Colors` | Terminal color output for diagnostics |
| `common/Errors` | Error handling and reporting |
| `support/CommandLine` | CLI argument parsing |
| `support/File` | File I/O operations |

## Key Concepts

### Language Features

- **Variable declarations** with explicit type annotations
- **Custom data types** (numeric and non-numeric)
- **Expressions**: arithmetic, comparison, logic, unary operations
- **String literals** and numeric constants
- **Control flow**: `if`, `elif`, `else` statements
- **Functions** with recursion support
- **Typed scopes** for variable tracking
- **Assembly generation** to ELF-compatible x86_64

### Type System

- Strong static typing (type-checked before code generation)
- Custom numeric types (supported types defined in TypeChecker)
- Type validation happens in the TypeChecker phase
- Symbol table management per scope

### Code Generation

- **Target**: x86_64 assembly (ELF format for Linux)
- **Memory Layout**: Stack-based with `.rodata` section for constants
- **Register Management**: Register allocator handles allocation and spilling
- **Canaries**: Security features for stack protection (`Canaries.hpp`)

## Common Workflows

### Adding a New Language Feature

1. **Update Lexer** (`lexer/Lexer.cpp`) - Add token type if needed
2. **Update Parser** (`parser/Parser.cpp`) - Add grammar rule for new construct
3. **Update AST** (`ast/ASTNode.hpp`) - Add AST node type
4. **Update TypeChecker** (`typechecker/TypeChecker.cpp`) - Add type validation rules
5. **Update CodeGen** (`codegen/CodeGen.cpp`) - Add assembly generation
6. **Add Tests** - Create `.zz` test files in `tests/zz/`

### Debugging Compilation Issues

- **Lexer errors** → Check tokenization in `Lexer.cpp`, verify token patterns
- **Parser errors** → Verify grammar in `Parser.cpp`, check AST node construction
- **Type errors** → Check `TypeChecker.cpp` for type rules and symbol resolution
- **Assembly generation** → Check `CodeGen.cpp` and `RegisterAllocator.cpp` for code patterns
- **Runtime issues** → Review generated assembly, check register allocation and stack layout

### Testing

- Test files use `.zz` extension
- Located in `tests/zz/` organized by feature
- Run tests with `python test_runner.py`
- Example `.zz` files in `examples/` directory

## Entry Point

- **Main file**: `main.cpp` - Orchestrates the compilation pipeline
- **CLI Interface**: `CommandLine` class handles program arguments
- **Input format**: `.zz` files (e.g., `program.zz`)

## Build System

### Building the Compiler

- **Build tool**: CMake (see `CMakeLists.txt`)
- **Build directory**: `build/` (contains generated build files)
- **Compiler output**: `zpiler` executable

**Linux/macOS:**
```bash
mkdir build && cd build
cmake ..
make -j
```

**Windows (Visual Studio):**
```bash
mkdir build && cd build
cmake ..
cmake --build . --config Debug
```

### Complete Compilation Pipeline

After generating assembly with `zpiler`, the workflow depends on the target format:

#### Linux (x86_64-linux)
1. **Generate assembly**: `./zpiler --format x86_64-linux -o out.asm program.zz`
2. **Assemble**: `as out.asm -o out.o` (GNU Assembler)
3. **Link**: `gcc out.o -o program` (GCC linker)
4. **Execute**: `./program`

**Required tools:** GNU Binutils (as), GCC

#### Windows (x86_64-mswin)
1. **Generate assembly**: `./zpiler --format x86_64-mswin -o out.asm program.zz`
2. **Assemble**: `ml64 /nologo /c out.asm` (Microsoft Macro Assembler)
3. **Link**: `gcc out.obj -o program.exe` (GCC linker)
4. **Execute**: `./program.exe`

**Required tools:** ml64 (from Visual Studio/Windows SDK), GCC

#### LLVM (llvm-ir)
1. **Generate LLVM IR**: `./zpiler --format llvm-ir -o out.ll program.zz`
2. **Compile to object**: `llc -filetype=obj out.ll -o out.o` (LLVM compiler)
3. **Link**: `gcc out.o -o program -no-pie` (GCC linker)
4. **Execute**: `./program`

**Required tools:** LLVM toolchain (llc), GCC

## Platform-Specific Code Generation

The project includes platform-specific code generators:

- `CodeGenLinux.cpp` - Linux x86_64 specifics
- `CodeGenWindows.cpp` - Windows x86_64 specifics (if applicable)
- `CodeGenLLVM.cpp` - LLVM backend (if applicable)

Choose appropriate backend based on target platform.

## Variable Scoping

- **ScopeContext** manages variable scopes during parsing and type checking
- Each function/block creates a new scope
- Variable lookup follows scope hierarchy (innermost to outermost)
- Used by both Parser and TypeChecker for consistency

## After Assembly Generation: What's Next

Once `zpiler` generates assembly files, there are two main paths:

### Path 1: Native Assembly (Linux/Windows)

**For `.asm` or `.s` files:**
- Use platform-specific assembler to create object files
- Link object files with GCC to create executable
- Run the executable directly

**Assemblers:**
- Linux: GNU `as` (gas)
- Windows: Microsoft `ml64` (masm64)

### Path 2: LLVM IR (Cross-platform)

**For `.ll` files:**
- Use `llc` (LLVM compiler) to generate optimized object files
- Link object files with GCC to create executable
- Run the executable directly

### Testing Workflow

The `test_runner.py` script automates the entire pipeline:
1. For each `.zz` test file
2. Compile to assembly/IR
3. Assemble to object file
4. Link to executable
5. Execute and verify
6. Report pass/fail

**Run tests:**
```bash
python test_runner.py                    # Auto-detect platform
TARGET=linux python test_runner.py       # Linux only
TARGET=windows python test_runner.py     # Windows only
TARGET=llvm python test_runner.py        # LLVM only
```
