# zlang

A lightweight statically typed programming language that compiles to **x86_64 Linux assembly**, supporting custom data types, conditionals, arithmetic, and type-safe operations. The language was designed for educational and experimental purposes with an emphasis on low-level control and code generation.

---

## ✨ Features

- Basic variable declarations and assignments
- Strong type-checking with custom-defined numeric and non-numeric types
- Expressions: arithmetic, comparison, logic, and unary operations
- String literals and numeric constants
- Control flow: `if`, `elif`, `else`
- Typed AST with scoped variable tracking
- Code generation to ELF-compatible x86_64 assembly
- Symbol table management and register allocation
- Stack-based memory layout and `.rodata` section usage

---

## 📦 Project Structure

```
zlang/
├── include/
│   ├── common/
│   │   ├── Colors.hpp
│   │   ├── Errors.hpp
│   │   ├── Logging.hpp
│   │   └── StringUtils.hpp
│   ├── support/
│   │   ├── CommandLine.hpp
│   │   └── File.hpp
│   ├── ast/
│   │   └── ASTNode.hpp
│   ├── lexer/
│   │   └── Lexer.hpp
│   ├── parser/
│   │   ├── Parser.hpp
│   │   └── ScopeContext.hpp
│   ├── typechecker/
│   │   └── TypeChecker.hpp
│   └── codegen/
│       ├── CodeGen.hpp  
│       ├── Canaries.hpp
│       └── RegisterAllocator.hpp
├── src/
│   ├── common/
│   │   ├── Logging.cpp
│   │   └── StringUtils.cpp
│   ├── support/
│   │   ├── CommandLine.cpp
│   │   └── File.cpp
│   ├── ast/
│   │   └── ASTNode.cpp
│   ├── lexer/
│   │   └── Lexer.cpp
│   ├── parser/
│   │   ├── ScopeContext.cpp
│   │   └── Parser.cpp
│   ├── typechecker/
│   │   └── TypeChecker.cpp
│   └── codegen/
│       ├── CodeGenWindows.cpp
│       ├── CodeGenLinux.cpp
│       ├── CodeGenLLVM.cpp
│       └── RegisterAllocator.cpp
├── tests/
│   ├── zz/
│   |   ├── [Various test categorized in folders]
├── examples/
│   ├── [current example we are working on]
├── main.cpp
├── CMakeLists.txt
├── test_runner.py
├── LICENSE
└── README.md

```

---

## 🚀 Getting Started

### Prerequisites

- Linux (x86_64)
- `gnu` (linux)
- `ml64.exe` (windows)
- `llc & clang` (LLVM)
- `g++` or `clang++` compiler
- `cmake`

### Build Instructions

```bash
# Clone the repository
git clone https://github.com/your-username/zlang.git
cd zlang

# Create build directory
mkdir build && cd build

# Generate and build
make
```

---

## 🔧 Usage

### On Linux when --format is x86_64-linux

- **Compile**

```bash
./zpiler --format x86_64-linux -o out.asm <source_file.zz> 
```

    This will:

    1. Parse the input`.zz` file.
      2. Typecheck the AST.
      3. Generate `out.asm` containing x86_64 assembly.

- **Assemble, Link and Run**

```bash
as out.asm -o out.o
gcc out.o -o out
./out
```

### On Linux when --format is llvm-ir

- **Compile**

```bash
./zpiler --format llvm-ir -o out.ll <source_file.zz> 
```

    This will:

    1. Parse the input`.zz` file.
      2. Typecheck the AST.
      3. Generate `out.ll` containing LLVM IR.

- **Assemble, Link and Run**

```bash
llc -filetype=obj -o out.ll <llvm_out_path>
gcc out.o -o out -no-pie
./out
```

### On Windows when --format is x86_64-windows

- **Compile**

```bash
./zpiler --format x86_64-windows -o out.asm <source_file.zz> 
```

    This will:

    1. Parse the input`.zz` file.
      2. Typecheck the AST.
      3. Generate `out.asm` containing x86_64 assembly.

- **Assemble, Link and Run**

```bash
ml64 /nologo /c .\out.asm
gcc out.o -o out
./out
```

---

## 🧠 Example Program (zlang)

```zlang
extern fn printf(fmt: string, ...) -> int32_t;

fn factorial(x: uint64_t) -> uint64_t{
    fn multiply(x: uint64_t, y: uint64_t) -> uint64_t{
        return x * y;
    }
    if(x <= 1){
        return 1;
    }else{
        return multiply(x, factorial(x - 1));
    }
}

fn main() {
    printf("Factorial of 10: %d\n", factorial(10));
}
```

---

## ⚙️ Design Highlights

- Type safety enforced during compile-time using a dedicated `TypeChecker`.
- Register allocator for both general-purpose (`%r`) and floating-point (`%xmm`) registers.
- Automatic handling of `.rodata` and `.text` segments during code generation.
- Explicit handling of stack operations for expression evaluation.

---

## 🧪 Testing

You can add `.zz` programs inside a `tests/` folder and run them individually through the pipeline. Type errors, parsing errors, and codegen output are printed to the terminal.

---

## 📜 License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

---

## 👨‍💻 Author

**Mihir Patel**

Feel free to reach out for contributions, discussions, or collaborations!
