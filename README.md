# zust

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
git clone https://github.com/your-username/zust.git
cd zust
# Create build directory
mkdir build && cd build
# Generate and build
make
```

---

## 🔧 Usage

### Complete Build Pipeline

The compilation process follows this pipeline:

1. **Compile** (zpiler): `.zz` source → assembly/IR
2. **Assemble**: assembly/IR → object file
3. **Link**: object file → executable
4. **Execute**: run the executable

### Linux (x86_64-linux)

**Step 1: Compile to Assembly**
```bash
./zpiler --format x86_64-linux -o out.asm program.zz
```
Generates: `out.asm` (x86_64 assembly)

**Step 2: Assemble**
```bash
as out.asm -o out.o
```
Generates: `out.o` (object file)

**Step 3: Link**
```bash
gcc out.o -o program
```
Generates: `program` (executable)

**Step 4: Execute**
```bash
./program
```

**Combined (one-liner):**
```bash
./zpiler --format x86_64-linux -o out.asm program.zz && as out.asm -o out.o && gcc out.o -o program && ./program
```

### Windows (x86_64-mswin)

**Step 1: Compile to Assembly**
```bash
./zpiler --format x86_64-mswin -o out.asm program.zz
```
Generates: `out.asm` (x86_64 assembly for Windows)

**Step 2: Assemble**
```bash
ml64 /nologo /c out.asm
```
Generates: `out.obj` (object file)

**Step 3: Link**
```bash
gcc out.obj -o program.exe
```
Generates: `program.exe` (executable)

**Step 4: Execute**
```bash
./program.exe
```

**Note:** Requires Microsoft Macro Assembler (ml64) installed. Available in Visual Studio or Windows SDK.

### LLVM IR Backend (llvm-ir)

**Step 1: Compile to LLVM IR**
```bash
./zpiler --format llvm-ir -o out.ll program.zz
```
Generates: `out.ll` (LLVM intermediate representation)

**Step 2: Compile to Object File**
```bash
llc -filetype=obj out.ll -o out.o
```
Generates: `out.o` (object file)

**Step 3: Link**
```bash
gcc out.o -o program -no-pie
```
Generates: `program` (executable)

**Step 4: Execute**
```bash
./program
```

**Combined (one-liner):**
```bash
./zpiler --format llvm-ir -o out.ll program.zz && llc -filetype=obj out.ll -o out.o && gcc out.o -o program -no-pie && ./program
```

**Note:** Requires LLVM toolchain (llc) installed.

### Automated Testing

Install test dependencies:
```bash
python3 -m pip install -r requirements-dev.txt
```

Run strict golden-output tests with `pytest`:
```bash
# Auto-detect native target when TARGET is not set
pytest -q

# Linux + LLVM
TARGET=linux,llvm pytest -q

# Windows
TARGET=windows pytest -q

# Update/create runtime goldens explicitly
TARGET=linux pytest -q --bless
```

---

## 🧠 Example Program (zust)

```zust
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

Test suites are directory-based:

- `tests/runtime`: program must compile, run, and match exact `stdout`/`stderr`/exit code
- `tests/runtime_fail`: program must compile, run, and match exact failing output + nonzero exit code
- `tests/compile_fail`: program must fail compilation with expected exit code and required stderr substrings

Expected files live under `tests/expected/<mode>/...` with mirrored paths.

---

## 📜 License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

---

## 👨‍💻 Author

**Mihir Patel**

Feel free to reach out for contributions, discussions, or collaborations!
