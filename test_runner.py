#!/usr/bin/env python3
import subprocess
import sys
from pathlib import Path
import shutil

# Your compiler driver:
ZPILER = "zpiler"  

# Per‐target invocation info:
TARGETS = {
    "linux": {
        "zpiler_flag": "--target X86_64_LINUX",
        "asm_ext": ".s",
        # assemble with 'as', link with 'ld'
        "assemble": lambda asm, obj: ["as", asm, "-o", obj],
        "link":    lambda obj, exe: ["ld", obj, "-o", exe]
    },
    "windows": {
        "zpiler_flag": "--target X86_64_WINDOWS",
        "asm_ext": ".asm",  # MASM‐style source
        # assemble with ml64, link with link.exe
        "assemble": lambda asm, obj: ["ml64", "/nologo", "/c", asm, "/Fo" + obj],
        "link":    lambda obj, exe: ["link", "/nologo", "/SUBSYSTEM:CONSOLE",
                                     "/OUT:" + exe, obj]
    },
    "llvm": {
        "zpiler_flag": "--target LLVM_IR",
        "asm_ext": ".ll",
        "assemble": lambda ir, obj: ["llc", "-filetype=obj", ir, "-o", obj],
        "link":    lambda obj, exe: ["clang", obj, "-o", exe]
    }
}

ROOT = Path(__file__).parent.resolve()
TEST_ZZ = ROOT / "test" / "zz"
ASM_DIR  = ROOT / "test" / "asm"
OBJ_DIR  = ROOT / "test" / "object"
EXE_DIR  = ROOT / "test" / "executable"

def run(cmd):
    print("> " + " ".join(cmd))
    proc = subprocess.run(cmd, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    if proc.returncode != 0:
        sys.stderr.write(proc.stderr.decode())
        sys.exit(proc.returncode)
    return proc.stdout

def main():
    # Clean outputs
    for d in (ASM_DIR, OBJ_DIR, EXE_DIR):
        if d.exists():
            shutil.rmtree(d)
        d.mkdir(parents=True)

    zz_files = list(TEST_ZZ.rglob("*.zz"))
    if not zz_files:
        print("No .zz tests found.")
        sys.exit(1)

    for target, cfg in TARGETS.items():
        print(f"\n=== Testing {target} ===")
        for zz in zz_files:
            rel = zz.relative_to(TEST_ZZ)
            asm_out = ASM_DIR  / target / rel.with_suffix(cfg["asm_ext"])
            obj_out = OBJ_DIR  / target / rel.with_suffix(".obj")
            exe_out = EXE_DIR  / target / rel.with_suffix(".exe")

            asm_out.parent.mkdir(parents=True, exist_ok=True)
            obj_out.parent.mkdir(parents=True, exist_ok=True)
            exe_out.parent.mkdir(parents=True, exist_ok=True)

            # 1) Emit assembly/IR
            run([ZPILER] + cfg["zpiler_flag"].split()
                + ["-o", str(asm_out), str(zz)])

            # 2) Assemble → object
            run(cfg["assemble"](str(asm_out), str(obj_out)))

            # 3) Link → executable
            run(cfg["link"](str(obj_out), str(exe_out)))

            # 4) Run it
            print(f"Running {exe_out} …")
            res = subprocess.run([str(exe_out)])
            if res.returncode != 0:
                sys.stderr.write(f"FAIL: {rel} on {target} returned {res.returncode}\n")
                sys.exit(res.returncode)
            else:
                print(f"PASS: {rel} ✓")

    # Cleanup executables
    print("\nCleaning up executables…")
    shutil.rmtree(EXE_DIR)
    print("All tests passed!")

if __name__ == "__main__":
    main()
