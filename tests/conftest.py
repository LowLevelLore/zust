from __future__ import annotations

import json
import os
import subprocess
import sys
from dataclasses import dataclass, field
from pathlib import Path

import pytest


@dataclass(frozen=True)
class TargetConfig:
    name: str  # the harness's short id ("linux") -- baked into golden filenames,
    # e.g. tests/expected/runtime/variables.linux.stdout, so it can never change.
    zpiler_format: str  # the --format value / BackendRegistry target name
    asm_ext: str
    assemble_cmd: tuple[str, ...] = field(default_factory=tuple)  # "$IN"/"$OUT" template
    link_cmd: tuple[str, ...] = field(default_factory=tuple)


# The harness's short target ids predate the backend registry and are frozen
# by golden filenames (tests/expected/**/*.{linux,windows,llvm}.*). Everything
# else about a target -- its --format value, file extension, and how to
# assemble/link it -- comes from `zpiler --formats --json`, so the driver and
# this harness cannot disagree about how to build a target.
_SHORT_NAME_TO_REGISTRY_NAME: dict[str, str] = {
    "linux": "x86_64-linux",
    "windows": "x86_64-mswin",
    "llvm": "llvm-ir",
}

_zpiler_path_candidates_cache: list[Path] | None = None
_targets_cache: dict[str, TargetConfig] | None = None


def _zpiler_path_candidates(project_root: Path) -> list[Path]:
    return [
        project_root / "build" / "zpiler",
        project_root / "build" / "Debug" / "zpiler.exe",
        project_root / "build" / "zpiler.exe",
    ]


def _find_zpiler(project_root: Path) -> Path | None:
    for candidate in _zpiler_path_candidates(project_root):
        if candidate.exists():
            return candidate
    return None


def discover_targets() -> dict[str, TargetConfig]:
    """Build TARGETS by asking the compiled zpiler what it supports.

    Runs at collection time (pytest_generate_tests needs it before any
    fixture is set up), so it duplicates the zpiler_path fixture's search
    rather than depending on it. Cached: this only needs to run once per
    session, and the compiler's answer does not change mid-run.
    """
    global _targets_cache
    if _targets_cache is not None:
        return _targets_cache

    project_root = Path(__file__).resolve().parents[1]
    zpiler_path = _find_zpiler(project_root)
    if zpiler_path is None:
        # No binary yet (e.g. -k collect-only before a build). Fall back to
        # the short-name list so collection doesn't explode; zpiler_path's own
        # pytest.exit(returncode=2) is what actually reports the real error
        # once a test tries to run.
        _targets_cache = {
            short: TargetConfig(name=short, zpiler_format=registry_name, asm_ext="")
            for short, registry_name in _SHORT_NAME_TO_REGISTRY_NAME.items()
        }
        return _targets_cache

    proc = subprocess.run(
        [str(zpiler_path), "--formats", "--json"],
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
    )
    data = json.loads(proc.stdout.decode("utf-8"))
    by_registry_name = {t["name"]: t for t in data["targets"]}

    targets: dict[str, TargetConfig] = {}
    for short, registry_name in _SHORT_NAME_TO_REGISTRY_NAME.items():
        info = by_registry_name.get(registry_name)
        if info is None:
            # The registry dropped a target the harness still expects a short
            # name for -- a real configuration error, not a missing binary.
            raise pytest.UsageError(
                f"zpiler --formats --json has no target named '{registry_name}' "
                f"(needed for harness target '{short}')"
            )
        targets[short] = TargetConfig(
            name=short,
            zpiler_format=info["name"],
            asm_ext=info["asmExt"],
            assemble_cmd=tuple(info["assembleCmd"]),
            link_cmd=tuple(info["linkCmd"]),
        )
    _targets_cache = targets
    return targets


def detect_native_target() -> str:
    plat = sys.platform
    if plat.startswith("linux"):
        return "linux"
    if plat.startswith("win32") or plat.startswith("cygwin"):
        return "windows"
    return "llvm"


def parse_target_env() -> list[str]:
    targets = discover_targets()
    env = os.getenv("TARGET")
    if env:
        wanted = [t.strip() for t in env.split(",") if t.strip() in targets]
        if not wanted:
            raise pytest.UsageError(
                f"TARGET={env} contains no known backends. "
                f"Known targets: {', '.join(targets)}"
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
    found = _find_zpiler(project_root)
    if found is not None:
        return found

    candidate_text = "\n".join(str(path) for path in _zpiler_path_candidates(project_root))
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
    return discover_targets()[target_name]
