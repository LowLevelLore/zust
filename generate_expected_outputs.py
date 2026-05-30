#!/usr/bin/env python3
import subprocess
import sys
from pathlib import Path
import tempfile
import shutil

ZPILER = Path("/home/xzist/zust/build/zpiler")
TESTS_DIR = Path("/home/xzist/zust/tests")
RUNTIME_DIR = TESTS_DIR / "runtime"
EXPECTED_DIR = TESTS_DIR / "expected" / "runtime"

def run_command(cmd, check=True):
    result = subprocess.run(cmd, capture_output=True, text=False)
    if check and result.returncode != 0:
        print(f"Command failed: {' '.join(cmd)}")
        print(f"stdout: {result.stdout.decode()}")
        print(f"stderr: {result.stderr.decode()}")
        sys.exit(1)
    return result

def generate_outputs_for_test(test_file):
    """Generate expected outputs for a single test file"""
    rel_path = test_file.relative_to(RUNTIME_DIR)
    expected_dir = EXPECTED_DIR / rel_path.parent
    expected_dir.mkdir(parents=True, exist_ok=True)
    
    test_name = test_file.stem
    
    # Compile to assembly
    with tempfile.TemporaryDirectory() as tmpdir:
        tmpdir = Path(tmpdir)
        asm_file = tmpdir / f"{test_name}.s"
        obj_file = tmpdir / f"{test_name}.o"
        exe_file = tmpdir / f"{test_name}"
        
        # Compile
        print(f"Compiling {rel_path}...", end=" ")
        result = run_command([str(ZPILER), "--format", "x86_64-linux", "-o", str(asm_file), str(test_file)], check=False)
        if result.returncode != 0:
            print(f"FAILED to compile")
            return False
        
        # Assemble
        result = run_command(["as", str(asm_file), "-o", str(obj_file)], check=False)
        if result.returncode != 0:
            print(f"FAILED to assemble")
            return False
        
        # Link
        result = run_command(["gcc", str(obj_file), "-o", str(exe_file)], check=False)
        if result.returncode != 0:
            print(f"FAILED to link")
            return False
        
        # Run
        result = run_command([str(exe_file)], check=False)
        
        # Save outputs
        stdout_file = expected_dir / f"{test_name}.stdout"
        stderr_file = expected_dir / f"{test_name}.stderr"
        exitcode_file = expected_dir / f"{test_name}.exitcode"
        
        stdout_file.write_bytes(result.stdout)
        stderr_file.write_bytes(result.stderr)
        exitcode_file.write_bytes(str(result.returncode).encode())
        
        print(f"OK (exit code: {result.returncode})")
        return True

def main():
    # Find all .zz files in runtime directory
    test_files = sorted(RUNTIME_DIR.rglob("*.zz"))
    
    success_count = 0
    fail_count = 0
    
    for test_file in test_files:
        if generate_outputs_for_test(test_file):
            success_count += 1
        else:
            fail_count += 1
    
    print(f"\nGenerated outputs for {success_count} tests, {fail_count} failed")

if __name__ == "__main__":
    main()
