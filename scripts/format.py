#!/usr/bin/env python3
"""Formats every tracked .cpp/.hpp file with clang-format -i.

Run this before committing -- CI's `clang-format --dry-run --Werror` job
(.github/workflows/test.yml) only checks files changed relative to the PR
base, so this is broader than what CI enforces on any one push, but running
it on everything is the simplest way to never be surprised by that check.

Uses `git ls-files` (not a directory walk) so it only ever touches files
already tracked by git -- never build/ output, and never something you added
but haven't `git add`ed yet without noticing.
"""

from __future__ import annotations

import shutil
import subprocess
import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parent.parent


def find_clang_format() -> str:
    exe = shutil.which("clang-format")
    if exe:
        return exe
    raise SystemExit(
        "clang-format not found on PATH. Install it (e.g. `pip install "
        "clang-format`, or via LLVM/your package manager) and try again."
    )


def tracked_cpp_files() -> list[str]:
    result = subprocess.run(
        ["git", "ls-files", "--", "*.cpp", "*.hpp"],
        cwd=ROOT,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        text=True,
        check=True,
    )
    return [line for line in result.stdout.splitlines() if line.strip()]


def main() -> int:
    clang_format = find_clang_format()
    files = tracked_cpp_files()
    if not files:
        print("No tracked .cpp/.hpp files found.")
        return 0

    print(f"Formatting {len(files)} file(s) with {clang_format} ...")
    # Chunk the invocation: a very large file list can exceed the command
    # line length limit on Windows.
    chunk_size = 100
    for i in range(0, len(files), chunk_size):
        chunk = files[i : i + chunk_size]
        subprocess.run([clang_format, "-i"] + chunk, cwd=ROOT, check=True)

    print("Done.")
    return 0


if __name__ == "__main__":
    sys.exit(main())
