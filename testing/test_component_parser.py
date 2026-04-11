#!/usr/bin/env python3
"""Test the extracted OCaml component parser against .wat components."""

import subprocess
import shutil
import tempfile
from pathlib import Path
from tqdm import tqdm


REPO_ROOT = Path(__file__).parent.parent
WIT_COMPONENT_TESTS = (
    REPO_ROOT
    / "submodules"
    / "wasm-tools"
    / "crates"
    / "wit-component"
    / "tests"
    / "components"
)
BINARY = "wit_parser_bin"
WASM_TOOLS = "wasm-tools"
PARSER = REPO_ROOT / "src" / BINARY


def is_component_wat(path: Path) -> bool:
    return path.read_text(errors="ignore").lstrip().startswith("(component")


def find_component_tests() -> list[Path]:
    files: list[Path] = []
    for path in sorted(WIT_COMPONENT_TESTS.rglob("*.wat")):
        if is_component_wat(path):
            files.append(path)
    return files


def to_temp_wasm(path: Path, temp_dir: Path) -> Path:
    rel = path.relative_to(WIT_COMPONENT_TESTS)
    rel_name = rel.with_suffix("").as_posix().replace("/", "__")
    return temp_dir / f"{rel_name}.wasm"


def main():
    if shutil.which(BINARY):
        parser_cmd = BINARY
    elif PARSER.exists():
        parser_cmd = str(PARSER)
    else:
        print(f"Error: parser binary not found at {PARSER}")
        print(f"Run `make src/{BINARY}` first.")
        print(f"Alternatively, ensure {BINARY} is in your PATH")
        exit(1)

    if not shutil.which(WASM_TOOLS):
        print(f"Error: {WASM_TOOLS} not found in PATH")
        print("Install wasm-tools and make it available on PATH.")
        exit(1)

    print(f"Using parser: {parser_cmd}")

    component_files = find_component_tests()
    if not component_files:
        print("Error: no component .wat files found under configured test root")
        exit(1)

    test_plan: list[tuple[Path, bool]] = []
    for wat_file in component_files:
        parts = wat_file.parts
        if "parse-fail" in parts:
            pf_idx = parts.index("parse-fail")
            below = parts[pf_idx + 1 :]
            if len(below) > 1 and wat_file.name != "root.wat":
                continue
        expect_fail = "parse-fail" in parts
        test_plan.append((wat_file, expect_fail))

    correct = []
    incorrect = []

    with tempfile.TemporaryDirectory() as tmpdir:
        tmp_dir = Path(tmpdir)

        for wat_file, expect_fail in tqdm(test_plan):
            wasm_file = to_temp_wasm(wat_file, tmp_dir)
            parse_result = subprocess.run(
                [WASM_TOOLS, "parse", "-o", str(wasm_file), str(wat_file)],
                capture_output=True,
            )
            if parse_result.returncode != 0:
                ok = False
            else:
                result = subprocess.run(
                    [parser_cmd, "--component", str(wasm_file)], capture_output=True
                )
                ok = result.returncode == 0

            if ok != expect_fail:
                correct.append((wat_file, ok, expect_fail))
            else:
                incorrect.append((wat_file, ok, expect_fail))


    total = len(test_plan)
    pct = 100 * len(correct) / total

    print(f"Results: {len(correct)}/{total} correct ({pct:.1f}%)\n")

    if incorrect:
        false_positives = [(f, ok, ef) for f, ok, ef in incorrect if ok and ef]
        false_negatives = [(f, ok, ef) for f, ok, ef in incorrect if not ok and not ef]

        if false_positives:
            print("Parsed but expected to fail:")
            for f, ok, ef in false_positives:
                print(f"  {f.relative_to(REPO_ROOT)}")
            print()

        if false_negatives:
            print("Failed to parse but expected to parse:")
            for f, ok, ef in false_negatives:
                print(f"  {f.relative_to(REPO_ROOT)}")


if __name__ == "__main__":
    main()
