#!/usr/bin/env python3
import subprocess
import sys
import os
from pathlib import Path
import shutil
import platform

# ANSI colors for not having eye cancer
RED = "\033[91m"
GREEN = "\033[92m"
YELLOW = "\033[93m"
CYAN = "\033[96m"
BOLD = "\033[1m"
RESET = "\033[0m"

ZPILER = "build/zpiler" if platform.system() != "Windows" else "build/Debug/zpiler.exe"

# Per‑target invocation info:
TARGETS = {
    "linux": {
        "zpiler_flag": "--format x86_64-linux",
        "asm_ext": ".s",
        "assemble": lambda asm, obj: ["as", asm, "-o", obj],
        "link": lambda obj, exe: ["gcc", obj, "-o", exe],
    },
    "windows": {
        "zpiler_flag": "--format x86_64-mswin",
        "asm_ext": ".asm",
        "assemble": lambda asm, obj: ["ml64", "/nologo", "/c", asm],
        "link": lambda obj, exe: [
            "link",
            "/nologo",
            "/SUBSYSTEM:CONSOLE",
            "/ENTRY:main",
            "/OUT:" + exe,
            obj,
        ],
    },
    "llvm": {
        "zpiler_flag": "--format llvm-ir",
        "asm_ext": ".ll",
        "assemble": lambda ir, obj: ["llc", "-filetype=obj", ir, "-o", obj],
        "link": lambda obj, exe: ["gcc", obj, "-o", exe, "-no-pie"],
    },
}

ROOT = Path(__file__).parent.resolve()
TEST_ZZ = ROOT / "tests" / "zz"
ASM_DIR = ROOT / "tests" / "asm"
OBJ_DIR = ROOT / "tests" / "object"
EXE_DIR = ROOT / "tests" / "executable"


def run(cmd):
    print(f"{CYAN}> {' '.join(cmd)}{RESET}")
    proc = subprocess.run(cmd, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    stdout = proc.stdout.decode()
    stderr = proc.stderr.decode()

    if stdout.strip():
        print(f"{stdout}")
    if stderr.strip():
        sys.stderr.write(f"{YELLOW}{stderr}{RESET}")

    if proc.returncode != 0:
        sys.stderr.write(f"{RED}Command failed: {' '.join(cmd)}{RESET}\n")
        sys.exit(proc.returncode)

    return stdout


def detect_native_target():
    plat = sys.platform
    if plat.startswith("linux"):
        return "linux"
    if plat.startswith("win32") or plat.startswith("cygwin"):
        return "windows"
    return "llvm"


def main():
    env = os.getenv("TARGET")
    if env:
        wanted = [t.strip() for t in env.split(",") if t.strip() in TARGETS]
        if not wanted:
            print(
                f"{RED}ERROR: TARGET={env} contains no known backends{RESET}",
                file=sys.stderr,
            )
            sys.exit(1)
    else:
        wanted = [detect_native_target()]

    print(f"{BOLD}Running tests for targets: {YELLOW}{wanted}{RESET}")

    for d in (ASM_DIR, OBJ_DIR, EXE_DIR):
        if d.exists():
            shutil.rmtree(d)
        d.mkdir(parents=True)

    zz_files = list(TEST_ZZ.rglob("*.zz"))
    if not zz_files:
        print(f"{YELLOW}No .zz tests found.{RESET}")
        sys.exit(1)

    for target in wanted:
        cfg = TARGETS[target]
        print(f"\n{BOLD}=== Testing {YELLOW}{target}{RESET} ===")
        for zz in zz_files:
            rel = zz.relative_to(TEST_ZZ)
            asm_out = ASM_DIR / target / rel.with_suffix(cfg["asm_ext"])
            obj_out = OBJ_DIR / target / rel.with_suffix(".obj")
            exe_out = EXE_DIR / target / rel.with_suffix(".exe")

            for p in (asm_out.parent, obj_out.parent, exe_out.parent):
                p.mkdir(parents=True, exist_ok=True)

            run([ZPILER] + cfg["zpiler_flag"].split() + ["-o", str(asm_out), str(zz)])

            if target == "windows":
                # Run ml64, obj will be produced in cwd
                obj_temp = asm_out.stem + ".obj"
                run(cfg["assemble"](str(asm_out), None))
                if not Path(obj_temp).exists():
                    print(f"{RED}Expected obj file {obj_temp} not found{RESET}")
                    sys.exit(1)
                shutil.move(obj_temp, obj_out)
            else:
                run(cfg["assemble"](str(asm_out), str(obj_out)))

            run(cfg["link"](str(obj_out), str(exe_out)))

            print(f"Running {exe_out} …")
            res = subprocess.run([str(exe_out)])
            if res.returncode != 0:
                sys.stderr.write(
                    f"{RED}FAIL: {rel} on {target} returned {res.returncode}{RESET}\n"
                )
                sys.exit(res.returncode)
            else:
                print(f"{GREEN}PASS: {rel} {RESET}")

    print(f"\n{CYAN}Cleaning up trash{RESET}")
    shutil.rmtree(EXE_DIR)
    shutil.rmtree(OBJ_DIR)
    shutil.rmtree(ASM_DIR)
    print(f"{GREEN}All tests passed!{RESET}")


if __name__ == "__main__":
    main()
