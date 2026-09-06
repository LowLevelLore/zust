from __future__ import annotations

import difflib
import os
import shlex
import shutil
import subprocess
from pathlib import Path

import pytest


TESTS_DIR = Path(__file__).resolve().parent
RUNTIME_DIR = TESTS_DIR / "runtime"
RUNTIME_FAIL_DIR = TESTS_DIR / "runtime_fail"
COMPILE_FAIL_DIR = TESTS_DIR / "compile_fail"
EXPECTED_DIR = TESTS_DIR / "expected"


def discover_cases(root: Path) -> list[Path]:
    if not root.exists():
        return []
    return sorted(path.relative_to(root) for path in root.rglob("*.zz"))


RUNTIME_CASES = discover_cases(RUNTIME_DIR)
RUNTIME_FAIL_CASES = discover_cases(RUNTIME_FAIL_DIR)
COMPILE_FAIL_CASES = discover_cases(COMPILE_FAIL_DIR)


def normalize_line_endings(data: bytes) -> bytes:
    return data.replace(b"\r\n", b"\n").replace(b"\r", b"\n")


def decode_text(data: bytes) -> str:
    return data.decode("utf-8", errors="replace")


def diff_text(expected: bytes, actual: bytes, label: str) -> str:
    expected_text = decode_text(expected)
    actual_text = decode_text(actual)
    diff = "".join(
        difflib.unified_diff(
            expected_text.splitlines(keepends=True),
            actual_text.splitlines(keepends=True),
            fromfile=f"expected/{label}",
            tofile=f"actual/{label}",
        )
    )
    if diff:
        return diff
    return (
        f"Byte mismatch for {label}, but textual diff is empty.\n"
        f"expected={expected!r}\nactual={actual!r}\n"
    )


def run_process(
    cmd: list[str], *, cwd: Path | None = None
) -> subprocess.CompletedProcess[bytes]:
    return subprocess.run(
        cmd,
        cwd=str(cwd) if cwd is not None else None,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
    )


def format_process_result(
    step: str, cmd: list[str], proc: subprocess.CompletedProcess[bytes]
) -> str:
    stdout = decode_text(normalize_line_endings(proc.stdout))
    stderr = decode_text(normalize_line_endings(proc.stderr))
    return (
        f"{step} failed.\n"
        f"Command: {' '.join(cmd)}\n"
        f"Exit code: {proc.returncode}\n"
        f"--- stdout ---\n{stdout}\n"
        f"--- stderr ---\n{stderr}"
    )


def compile_source(
    *,
    source: Path,
    asm_out: Path,
    target_config,
    zpiler_path: Path,
) -> tuple[list[str], subprocess.CompletedProcess[bytes]]:
    # ZPILER_EXTRA_ARGS is not a permanent per-target setting -- it's how a
    # PRD wave verifies a codegen path that's still behind a flag before it
    # becomes the default (docs/PRD-ZIR.md Wave 4.1 used it for
    # `--zir-codegen`, e.g. `ZPILER_EXTRA_ARGS=--zir-codegen TARGET=llvm
    # pytest -q`, before Wave 4.2 flipped that flag's behavior on
    # permanently and deleted it). Applied to every target compiled in the
    # run, same as TARGET itself.
    extra_args = shlex.split(os.environ.get("ZPILER_EXTRA_ARGS", ""))
    cmd = [
        str(zpiler_path),
        "--format",
        target_config.zpiler_format,
        *extra_args,
        "-o",
        str(asm_out),
        str(source),
    ]
    return cmd, run_process(cmd)


def substitute_cmd(template: tuple[str, ...], *, IN: str, OUT: str) -> list[str]:
    return [part.replace("$IN", IN).replace("$OUT", OUT) for part in template]


def assemble_source(
    *,
    target_name: str,
    target_config,
    asm_out: Path,
    obj_out: Path,
    work_dir: Path,
) -> tuple[list[str], subprocess.CompletedProcess[bytes]]:
    cmd = substitute_cmd(target_config.assemble_cmd, IN=str(asm_out), OUT=str(obj_out))

    if target_name == "windows":
        # ml64 only accepts one input and always writes <stem>.obj next to it
        # -- $OUT can't be substituted into the command itself, so run it in
        # work_dir and move the result into place afterward.
        proc = run_process(cmd, cwd=work_dir)
        if proc.returncode != 0:
            return cmd, proc

        obj_temp = work_dir / f"{asm_out.stem}.obj"
        if not obj_temp.exists():
            return (
                cmd,
                subprocess.CompletedProcess(
                    cmd,
                    returncode=1,
                    stdout=proc.stdout,
                    stderr=proc.stderr
                    + f"\nExpected object file not found: {obj_temp}\n".encode(),
                ),
            )

        obj_out.parent.mkdir(parents=True, exist_ok=True)
        shutil.move(str(obj_temp), str(obj_out))
        return cmd, proc

    return cmd, run_process(cmd)


def link_object(
    *,
    target_config,
    obj_out: Path,
    exe_out: Path,
) -> tuple[list[str], subprocess.CompletedProcess[bytes]]:
    cmd = substitute_cmd(target_config.link_cmd, IN=str(obj_out), OUT=str(exe_out))
    return cmd, run_process(cmd)


def run_executable(exe_out: Path, work_dir: Path) -> subprocess.CompletedProcess[bytes]:
    return run_process([str(exe_out)], cwd=work_dir)


def expectation_candidates(base: Path, target_name: str, ext: str) -> tuple[Path, Path]:
    specific = Path(f"{base}.{target_name}.{ext}")
    shared = Path(f"{base}.{ext}")
    return specific, shared


def resolve_existing_expectation(base: Path, target_name: str, ext: str) -> Path:
    specific, shared = expectation_candidates(base, target_name, ext)
    if specific.exists():
        return specific
    return shared


def resolve_write_expectation(base: Path, target_name: str, ext: str) -> Path:
    specific, shared = expectation_candidates(base, target_name, ext)
    if specific.exists():
        return specific
    if shared.exists():
        return shared
    return shared


def expected_base(mode: str, rel_path: Path) -> Path:
    return EXPECTED_DIR / mode / rel_path.with_suffix("")


def parse_exit_code(path: Path) -> int:
    text = decode_text(normalize_line_endings(path.read_bytes())).strip()
    if not text:
        pytest.fail(f"Expected exit code file is empty: {path}", pytrace=False)
    try:
        return int(text)
    except ValueError as exc:
        pytest.fail(f"Invalid exit code in {path}: {text!r}", pytrace=False)
        raise exc


def load_or_bless_output(
    *,
    base: Path,
    target_name: str,
    ext: str,
    actual: bytes,
    bless: bool,
) -> tuple[bytes, Path]:
    actual_norm = normalize_line_endings(actual)
    read_path = resolve_existing_expectation(base, target_name, ext)

    if bless:
        write_path = resolve_write_expectation(base, target_name, ext)
        write_path.parent.mkdir(parents=True, exist_ok=True)
        if (not write_path.exists()) or (
            normalize_line_endings(write_path.read_bytes()) != actual_norm
        ):
            write_path.write_bytes(actual_norm)
        return actual_norm, write_path

    if not read_path.exists():
        pytest.fail(
            f"Missing expected file: {read_path}\n"
            "Re-run with --bless to create runtime expectations.",
            pytrace=False,
        )

    return normalize_line_endings(read_path.read_bytes()), read_path


def load_or_bless_exit_code(
    *,
    base: Path,
    target_name: str,
    actual: int,
    bless: bool,
) -> tuple[int, Path]:
    read_path = resolve_existing_expectation(base, target_name, "exitcode")

    if bless:
        write_path = resolve_write_expectation(base, target_name, "exitcode")
        write_path.parent.mkdir(parents=True, exist_ok=True)
        rendered = f"{actual}\n".encode()
        if (not write_path.exists()) or (write_path.read_bytes() != rendered):
            write_path.write_bytes(rendered)
        return actual, write_path

    if not read_path.exists():
        pytest.fail(
            f"Missing expected file: {read_path}\n"
            "Re-run with --bless to create runtime expectations.",
            pytrace=False,
        )
    return parse_exit_code(read_path), read_path


def assert_bytes_equal(
    *,
    label: str,
    expected: bytes,
    actual: bytes,
    expected_path: Path,
    source: Path,
    target_name: str,
) -> None:
    if expected == actual:
        return
    diff = diff_text(expected, actual, label)
    raise AssertionError(
        f"{source} on target={target_name} mismatched {label}.\n"
        f"Expectation file: {expected_path}\n{diff}"
    )


def build_and_run(
    *,
    mode: str,
    rel_path: Path,
    source_root: Path,
    target_name: str,
    target_config,
    zpiler_path: Path,
    artifacts_root: Path,
) -> subprocess.CompletedProcess[bytes]:
    source = source_root / rel_path
    case_root = artifacts_root / mode / target_name / rel_path.with_suffix("")
    case_root.mkdir(parents=True, exist_ok=True)

    asm_out = case_root / f"{source.stem}{target_config.asm_ext}"
    obj_out = case_root / f"{source.stem}.obj"
    exe_out = case_root / f"{source.stem}.exe"

    compile_cmd, compile_proc = compile_source(
        source=source,
        asm_out=asm_out,
        target_config=target_config,
        zpiler_path=zpiler_path,
    )
    if compile_proc.returncode != 0:
        pytest.fail(
            format_process_result("Compile", compile_cmd, compile_proc), pytrace=False
        )

    assemble_cmd, assemble_proc = assemble_source(
        target_name=target_name,
        target_config=target_config,
        asm_out=asm_out,
        obj_out=obj_out,
        work_dir=case_root,
    )
    if assemble_proc.returncode != 0:
        pytest.fail(
            format_process_result("Assemble", assemble_cmd, assemble_proc), pytrace=False
        )

    link_cmd, link_proc = link_object(
        target_config=target_config,
        obj_out=obj_out,
        exe_out=exe_out,
    )
    if link_proc.returncode != 0:
        pytest.fail(format_process_result("Link", link_cmd, link_proc), pytrace=False)

    return run_executable(exe_out, case_root)


def run_compile_only(
    *,
    rel_path: Path,
    source_root: Path,
    target_config,
    zpiler_path: Path,
    artifacts_root: Path,
    target_name: str,
) -> subprocess.CompletedProcess[bytes]:
    source = source_root / rel_path
    case_root = artifacts_root / "compile_fail" / target_name / rel_path.with_suffix("")
    case_root.mkdir(parents=True, exist_ok=True)
    asm_out = case_root / f"{source.stem}{target_config.asm_ext}"
    _, compile_proc = compile_source(
        source=source,
        asm_out=asm_out,
        target_config=target_config,
        zpiler_path=zpiler_path,
    )
    return compile_proc


def load_required_substrings(base: Path, target_name: str) -> tuple[list[str], Path]:
    contains_path = resolve_existing_expectation(base, target_name, "stderr.contains")
    if not contains_path.exists():
        pytest.fail(
            f"Missing required compile_fail expectation: {contains_path}", pytrace=False
        )

    lines = decode_text(normalize_line_endings(contains_path.read_bytes())).splitlines()
    required = []
    for line in lines:
        stripped = line.strip()
        if not stripped or stripped.startswith("#"):
            continue
        required.append(stripped)

    if not required:
        pytest.fail(
            f"No required substrings found in {contains_path}. "
            "Add at least one non-comment line.",
            pytrace=False,
        )
    return required, contains_path


def load_compile_fail_expected_exit(base: Path, target_name: str) -> tuple[int, Path | None]:
    exit_path = resolve_existing_expectation(base, target_name, "exitcode")
    if not exit_path.exists():
        return 1, None
    return parse_exit_code(exit_path), exit_path


@pytest.mark.parametrize("rel_path", RUNTIME_CASES, ids=lambda p: p.as_posix())
def test_runtime(
    rel_path: Path,
    target_name: str,
    target_config,
    project_root: Path,
    zpiler_path: Path,
    artifacts_root: Path,
    bless: bool,
) -> None:
    proc = build_and_run(
        mode="runtime",
        rel_path=rel_path,
        source_root=project_root / "tests" / "runtime",
        target_name=target_name,
        target_config=target_config,
        zpiler_path=zpiler_path,
        artifacts_root=artifacts_root,
    )

    base = expected_base("runtime", rel_path)

    expected_stdout, stdout_path = load_or_bless_output(
        base=base,
        target_name=target_name,
        ext="stdout",
        actual=proc.stdout,
        bless=bless,
    )
    expected_stderr, stderr_path = load_or_bless_output(
        base=base,
        target_name=target_name,
        ext="stderr",
        actual=proc.stderr,
        bless=bless,
    )
    expected_exit, exit_path = load_or_bless_exit_code(
        base=base,
        target_name=target_name,
        actual=proc.returncode,
        bless=bless,
    )

    actual_stdout = normalize_line_endings(proc.stdout)
    actual_stderr = normalize_line_endings(proc.stderr)
    assert_bytes_equal(
        label="stdout",
        expected=expected_stdout,
        actual=actual_stdout,
        expected_path=stdout_path,
        source=rel_path,
        target_name=target_name,
    )
    assert_bytes_equal(
        label="stderr",
        expected=expected_stderr,
        actual=actual_stderr,
        expected_path=stderr_path,
        source=rel_path,
        target_name=target_name,
    )
    if proc.returncode != expected_exit:
        raise AssertionError(
            f"{rel_path} on target={target_name} mismatched exit code.\n"
            f"Expectation file: {exit_path}\n"
            f"Expected: {expected_exit}\nActual: {proc.returncode}"
        )


@pytest.mark.parametrize("rel_path", RUNTIME_FAIL_CASES, ids=lambda p: p.as_posix())
def test_runtime_fail(
    rel_path: Path,
    target_name: str,
    target_config,
    project_root: Path,
    zpiler_path: Path,
    artifacts_root: Path,
    bless: bool,
) -> None:
    proc = build_and_run(
        mode="runtime_fail",
        rel_path=rel_path,
        source_root=project_root / "tests" / "runtime_fail",
        target_name=target_name,
        target_config=target_config,
        zpiler_path=zpiler_path,
        artifacts_root=artifacts_root,
    )

    base = expected_base("runtime_fail", rel_path)

    expected_stdout, stdout_path = load_or_bless_output(
        base=base,
        target_name=target_name,
        ext="stdout",
        actual=proc.stdout,
        bless=bless,
    )
    expected_stderr, stderr_path = load_or_bless_output(
        base=base,
        target_name=target_name,
        ext="stderr",
        actual=proc.stderr,
        bless=bless,
    )
    expected_exit, exit_path = load_or_bless_exit_code(
        base=base,
        target_name=target_name,
        actual=proc.returncode,
        bless=bless,
    )

    if expected_exit == 0:
        pytest.fail(
            f"runtime_fail expectation cannot use exit code 0: {exit_path}",
            pytrace=False,
        )

    actual_stdout = normalize_line_endings(proc.stdout)
    actual_stderr = normalize_line_endings(proc.stderr)
    assert_bytes_equal(
        label="stdout",
        expected=expected_stdout,
        actual=actual_stdout,
        expected_path=stdout_path,
        source=rel_path,
        target_name=target_name,
    )
    assert_bytes_equal(
        label="stderr",
        expected=expected_stderr,
        actual=actual_stderr,
        expected_path=stderr_path,
        source=rel_path,
        target_name=target_name,
    )
    if proc.returncode != expected_exit:
        raise AssertionError(
            f"{rel_path} on target={target_name} mismatched exit code.\n"
            f"Expectation file: {exit_path}\n"
            f"Expected: {expected_exit}\nActual: {proc.returncode}"
        )


@pytest.mark.parametrize("rel_path", COMPILE_FAIL_CASES, ids=lambda p: p.as_posix())
def test_compile_fail(
    rel_path: Path,
    target_name: str,
    target_config,
    project_root: Path,
    zpiler_path: Path,
    artifacts_root: Path,
) -> None:
    proc = run_compile_only(
        rel_path=rel_path,
        source_root=project_root / "tests" / "compile_fail",
        target_config=target_config,
        zpiler_path=zpiler_path,
        artifacts_root=artifacts_root,
        target_name=target_name,
    )

    base = expected_base("compile_fail", rel_path)
    expected_exit, exit_path = load_compile_fail_expected_exit(base, target_name)
    if proc.returncode != expected_exit:
        exit_source = str(exit_path) if exit_path is not None else "default(1)"
        pytest.fail(
            "compile_fail exit code mismatch.\n"
            f"Source: {rel_path}\n"
            f"Target: {target_name}\n"
            f"Expected ({exit_source}): {expected_exit}\n"
            f"Actual: {proc.returncode}\n"
            f"--- stdout ---\n{decode_text(normalize_line_endings(proc.stdout))}\n"
            f"--- stderr ---\n{decode_text(normalize_line_endings(proc.stderr))}",
            pytrace=False,
        )

    if proc.returncode == 0:
        pytest.fail(
            "compile_fail test unexpectedly succeeded.\n"
            f"Source: {rel_path}\nTarget: {target_name}",
            pytrace=False,
        )

    required_substrings, contains_path = load_required_substrings(base, target_name)
    stderr_text = decode_text(normalize_line_endings(proc.stderr))
    missing = [needle for needle in required_substrings if needle not in stderr_text]
    if missing:
        joined = "\n".join(f"- {needle}" for needle in missing)
        pytest.fail(
            "compile_fail stderr missing required substrings.\n"
            f"Source: {rel_path}\n"
            f"Target: {target_name}\n"
            f"Expectation file: {contains_path}\n"
            f"Missing:\n{joined}\n"
            f"--- stderr ---\n{stderr_text}",
            pytrace=False,
        )
