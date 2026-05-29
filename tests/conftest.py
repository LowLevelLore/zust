from __future__ import annotations

import os
import sys
from dataclasses import dataclass
from pathlib import Path

import pytest


@dataclass(frozen=True)
class TargetConfig:
    name: str
    zpiler_format: str
    asm_ext: str


TARGETS: dict[str, TargetConfig] = {
    "linux": TargetConfig(name="linux", zpiler_format="x86_64-linux", asm_ext=".s"),
    "windows": TargetConfig(
        name="windows", zpiler_format="x86_64-mswin", asm_ext=".asm"
    ),
    "llvm": TargetConfig(name="llvm", zpiler_format="llvm-ir", asm_ext=".ll"),
}


def detect_native_target() -> str:
    plat = sys.platform
    if plat.startswith("linux"):
        return "linux"
    if plat.startswith("win32") or plat.startswith("cygwin"):
        return "windows"
    return "llvm"


def parse_target_env() -> list[str]:
    env = os.getenv("TARGET")
    if env:
        wanted = [t.strip() for t in env.split(",") if t.strip() in TARGETS]
        if not wanted:
            raise pytest.UsageError(
                f"TARGET={env} contains no known backends. "
                f"Known targets: {', '.join(TARGETS)}"
            )
        return wanted
    return [detect_native_target()]


def pytest_addoption(parser: pytest.Parser) -> None:
    parser.addoption(
        "--bless",
        action="store_true",
        default=False,
        help="Create or update runtime golden output files.",
    )


def pytest_generate_tests(metafunc: pytest.Metafunc) -> None:
    if "target_name" in metafunc.fixturenames:
        targets = parse_target_env()
        metafunc.parametrize(
            "target_name", targets, ids=[f"target={target}" for target in targets]
        )


@pytest.fixture(scope="session")
def bless(request: pytest.FixtureRequest) -> bool:
    return bool(request.config.getoption("--bless"))


@pytest.fixture(scope="session")
def project_root() -> Path:
    return Path(__file__).resolve().parents[1]


@pytest.fixture(scope="session")
def zpiler_path(project_root: Path) -> Path:
    candidates = [
        project_root / "build" / "zpiler",
        project_root / "build" / "Debug" / "zpiler.exe",
        project_root / "build" / "zpiler.exe",
    ]
    for candidate in candidates:
        if candidate.exists():
            return candidate

    candidate_text = "\n".join(str(path) for path in candidates)
    pytest.exit(
        "Could not find zpiler executable. Build project first.\n"
        f"Searched:\n{candidate_text}",
        returncode=2,
    )


@pytest.fixture(scope="session")
def artifacts_root(tmp_path_factory: pytest.TempPathFactory) -> Path:
    return tmp_path_factory.mktemp("pipeline_artifacts")


@pytest.fixture
def target_config(target_name: str) -> TargetConfig:
    return TARGETS[target_name]
