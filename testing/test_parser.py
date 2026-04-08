#!/usr/bin/env python3
"""Test the extracted Rocq WIT parser against the wasm-tools test suite."""

import subprocess
from pathlib import Path

REPO_ROOT = Path(__file__).parent.parent
PARSER = REPO_ROOT / "src" / "wit_parser_bin"
WIT_TESTS = REPO_ROOT / "submodules" / "wasm-tools" / "crates" / "wit-parser" / "tests"


def main():
    if not PARSER.exists():
        print(f"Error: parser binary not found at {PARSER}")
        print("Run `make src/wit_parser_bin` first.")
        exit(1)

    wit_files = sorted(WIT_TESTS.rglob("*.wit"))
    if not wit_files:
        print(f"Error: no .wit files found under {WIT_TESTS}")
        exit(1)

    correct = []
    incorrect = []

    for wit in wit_files:
        expect_fail = "parse-fail" in wit.parts
        result = subprocess.run([str(PARSER), str(wit)], capture_output=True)
        ok = result.returncode == 0
        if ok != expect_fail:
            correct.append((wit, ok, expect_fail))
        else:
            incorrect.append((wit, ok, expect_fail))

    total = len(wit_files)
    pct = 100 * len(correct) / total

    print(f"Results: {len(correct)}/{total} correct ({pct:.1f}%)\n")

    if incorrect:
        print("Incorrect files:")
        for f, ok, expect_fail in incorrect:
            outcome = "parsed" if ok else "failed to parse"
            expected = "expected to fail" if expect_fail else "expected to parse"
            print(f"  {f.relative_to(WIT_TESTS)}  ({outcome}, {expected})")


if __name__ == "__main__":
    main()
