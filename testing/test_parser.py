#!/usr/bin/env python3
"""Test the extracted Rocq WIT parser against the wasm-tools test suite."""

import subprocess
from tqdm import tqdm
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

    for wit in tqdm(wit_files):
        # Dependency files inside parse-fail scenarios are only meaningful in
        # multi-file context; skip them to avoid false expectations.
        parts = wit.parts
        if "parse-fail" in parts and "deps" in parts:
            continue
        expect_fail = "parse-fail" in parts
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
        false_positives = [(f, ok, ef) for f, ok, ef in incorrect if ok and ef]
        false_negatives = [(f, ok, ef) for f, ok, ef in incorrect if not ok and not ef]

        if false_positives:
            print("Parsed but expected to fail:")
            for f, ok, ef in false_positives:
                print(f"  {f.relative_to(WIT_TESTS)}")
            print()

        if false_negatives:
            print("Failed to parse but expected to parse:")
            for f, ok, ef in false_negatives:
                print(f"  {f.relative_to(WIT_TESTS)}")


if __name__ == "__main__":
    main()
