#!/usr/bin/env python3
"""Test the extracted Rocq WIT parser against the wasm-tools test suite."""

import subprocess
import shutil
from tqdm import tqdm
from pathlib import Path

REPO_ROOT = Path(__file__).parent.parent
WIT_TESTS = REPO_ROOT / "submodules" / "wasm-tools" / "crates" / "wit-parser" / "tests"
BINARY = "wc-tools"
PARSER = REPO_ROOT / "src" / BINARY


def main():
    # Try to use native wit_parser_test binary from PATH first
    if shutil.which(BINARY):
        parser_cmd = BINARY
    elif PARSER.exists():
        parser_cmd = str(PARSER)
    else:
        print(f"Error: parser binary not found at {PARSER}")
        print(f"Run `make src/{BINARY}` first.")
        print("Alternatively, ensure {BINARY} is in your PATH")
        exit(1)

    print(f"Using parser: {parser_cmd}")

    wit_files = sorted(WIT_TESTS.rglob("*.wit"))
    if not wit_files:
        print(f"Error: no .wit files found under {WIT_TESTS}")
        exit(1)

    correct = []
    incorrect = []

    for wit in tqdm(wit_files):
        # Files in subdirectories of parse-fail are multi-file test scenarios.
        # Only test the root file of each scenario; skip everything else.
        parts = wit.parts
        if "parse-fail" in parts:
            pf_idx = parts.index("parse-fail")
            below = parts[pf_idx + 1 :]
            if len(below) > 1 and wit.name != "root.wit":
                continue
        expect_fail = "parse-fail" in parts
        result = subprocess.run([parser_cmd, str(wit)], capture_output=True)
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
