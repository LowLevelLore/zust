# zlang

A lightweight statically typed programming language that compiles to **x86_64 Linux assembly**, supporting custom data types, conditionals, arithmetic, and type-safe operations. The language was designed for educational and experimental purposes with an emphasis on low-level control and code generation.

---

## ✨ Features

- Basic variable declarations and assignments
- Strong type-checking with custom-defined numeric and non-numeric types
- Expressions: arithmetic, comparison, logic, and unary operations
- String literals and numeric constants
- Control flow: `if`, `else if`, `else`
- Typed AST with scoped variable tracking
- Code generation to ELF-compatible x86_64 assembly
- Symbol table management and register allocation
- Stack-based memory layout and `.rodata` section usage

---

## 📦 Project Structure

```
zlang/
├── include/
│   ├── parser/
│   │   ├── ASTNode.hpp
│   │   ├── Parser.hpp
│   │   ├── ScopeContext.hpp
│   │   └── Token.hpp
│   ├── codegen/
│   │   ├── CodegenLinux.hpp
│   │   └── RegisterAllocator.hpp
│   └── typechecker/
│       └── TypeChecker.hpp
├── src/
│   ├── parser/
│   │   └── Parser.cpp
│   ├── codegen/
│   │   └── CodegenLinux.cpp
│   └── typechecker/
│       └── TypeChecker.cpp
├── main.cpp
├── Makefile
└── README.md
```

---

## 🚀 Getting Started

### Prerequisites

- Linux (x86_64)
- `gnu` assembler
- `g++` or `clang++` compiler
- `make` or `cmake` (for build automation)

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

```bash
./zpiler <source_file.zz>
```

This will:

1. Parse the input `.zz` file.
2. Typecheck the AST.
3. Generate `out.asm` containing x86_64 assembly.
4. Assemble and link the code to produce an ELF executable.

```bash
as out.asm -o out.o
ld out.o -o out
./out
```

---

## 🧠 Example Program (zlang)

```zlang
let x: integer = 10;
let y: integer;

y = 13;
x = 11;

let msg: string = "Hello World";
let pi: double = 3.1415;
let precision: float = 0.0001f;

let a: uint32_t = 123;
let b: float32_t = 12.3f;

if (a > b + 10) {
    // do something
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

```

```
