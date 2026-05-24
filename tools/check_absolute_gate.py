#!/usr/bin/env python3
from __future__ import annotations

import hashlib
import os
import subprocess
import sys
import tempfile
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
COVERAGE_DIR = ROOT / "_coverage_phase13"
COVERAGE_PREFIX = COVERAGE_DIR / "bisect"
COVERAGE_XML = COVERAGE_DIR / "rfc6570-phase13.coverage.xml"

GENERATED_NAMES = [
    "abnf-corpus.tsv",
    "abnf-corpus.summary",
    "operator-matrix.tsv",
    "operator-matrix.summary",
]
BUILD_GENERATED_DIR = ROOT / "_build" / "default" / "test" / "generated"

STALE_PATTERNS = [
    "missing_generated_evidence",
    "covered_current_needs_generated_evidence",
    "missing_docs",
    "Needs",
    "Still needs",
    "Classify",
    "Generated matrix must",
    "add generated",
    "Add generated",
]

TEXT_SUFFIXES = {".md", ".ml", ".mli", ".opam", ".tsv", ".summary", ".py"}


def sha256(path: Path) -> str:
    return hashlib.sha256(path.read_bytes()).hexdigest()


def run(args: list[str], *, env: dict[str, str] | None = None) -> str:
    print("$ " + " ".join(args), flush=True)
    merged_env = os.environ.copy()
    if env:
        merged_env.update(env)
    completed = subprocess.run(
        args,
        cwd=ROOT,
        env=merged_env,
        text=True,
        stdout=subprocess.PIPE,
        stderr=subprocess.STDOUT,
    )
    if completed.stdout:
        print(completed.stdout, end="")
    if completed.returncode != 0:
        raise SystemExit(completed.returncode)
    return completed.stdout


def check_generated_reproducible() -> None:
    with tempfile.TemporaryDirectory(prefix="rfc6570-generated-") as tmp:
        tmp_dir = Path(tmp)
        run(["python3", "tools/gen_abnf_corpus.py", "--out-dir", str(tmp_dir)])
        run(["python3", "tools/gen_operator_matrix.py", "--out-dir", str(tmp_dir)])
        run(
            [
                "opam",
                "exec",
                "--switch=guarded-thought",
                "--",
                "dune",
                "build",
                *(f"test/generated/{name}" for name in GENERATED_NAMES),
            ]
        )
        mismatches = [
            name
            for name in GENERATED_NAMES
            if sha256(tmp_dir / name) != sha256(BUILD_GENERATED_DIR / name)
        ]
    if mismatches:
        for name in mismatches:
            print(f"generated artifact mismatch: test/generated/{name}", file=sys.stderr)
        raise SystemExit(1)
    print("generated_reproducible\tOK")


def check_wip_tsv_field_counts() -> None:
    for path in sorted(ROOT.glob("*.wip.tsv")):
        rows = path.read_text(encoding="utf-8").splitlines()
        if not rows:
            raise SystemExit(f"empty TSV: {path.name}")
        width = len(rows[0].split("\t"))
        for number, line in enumerate(rows[1:], 2):
            got = len(line.split("\t"))
            if got != width:
                raise SystemExit(
                    f"bad TSV width in {path.name}:{number}: expected {width}, got {got}"
                )
    print("wip_tsv_field_counts\tOK")


def summary_declared_rows(path: Path) -> int:
    for line in path.read_text(encoding="utf-8").splitlines():
        if line.startswith("rows\t"):
            return int(line.split("\t", 1)[1])
    raise SystemExit(f"missing rows entry in {path.relative_to(ROOT)}")


def check_generated_summary_counts() -> None:
    for stem in ["abnf-corpus", "operator-matrix"]:
        tsv = BUILD_GENERATED_DIR / f"{stem}.tsv"
        summary = BUILD_GENERATED_DIR / f"{stem}.summary"
        actual = max(0, len(tsv.read_text(encoding="utf-8").splitlines()) - 1)
        declared = summary_declared_rows(summary)
        if actual != declared:
            raise SystemExit(f"{stem}: TSV rows {actual} != summary rows {declared}")
        print(f"{stem}\trows={actual}\tOK")


def check_normative_statuses() -> None:
    path = ROOT / "RFC6570_NORMATIVE_LEDGER.wip.tsv"
    rows = path.read_text(encoding="utf-8").splitlines()
    header = rows[0].split("\t")
    status_index = header.index("status")
    counts: dict[str, int] = {}
    disallowed: list[tuple[int, str, str]] = []
    for number, line in enumerate(rows[1:], 2):
        fields = line.split("\t")
        status = fields[status_index]
        counts[status] = counts.get(status, 0) + 1
        if status not in {"covered_current", "non_normative_guidance"}:
            disallowed.append((number, fields[0], status))
    if disallowed:
        for number, row_id, status in disallowed:
            print(f"open normative status: {path.name}:{number} {row_id} {status}", file=sys.stderr)
        raise SystemExit(1)
    print("normative_statuses\t" + ",".join(f"{k}={counts[k]}" for k in sorted(counts)))


def text_files() -> list[Path]:
    result: list[Path] = []
    for path in ROOT.rglob("*"):
        if not path.is_file():
            continue
        rel = path.relative_to(ROOT)
        if rel.parts[0] in {"_build", "_coverage_phase13", "test/uritemplate-test"}:
            continue
        if rel.as_posix() == "tools/check_absolute_gate.py":
            continue
        if path.suffix in TEXT_SUFFIXES or path.name in {"README.md", ".ocamlformat", ".gitignore"}:
            result.append(path)
    return result


def check_stale_markers() -> None:
    matches: list[str] = []
    for path in text_files():
        text = path.read_text(encoding="utf-8", errors="replace")
        for line_no, line in enumerate(text.splitlines(), 1):
            for pattern in STALE_PATTERNS:
                if pattern in line:
                    matches.append(f"{path.relative_to(ROOT)}:{line_no}: {pattern}")
    if matches:
        for match in matches:
            print(match, file=sys.stderr)
        raise SystemExit(1)
    print("stale_markers\tOK")


def clean_coverage() -> None:
    COVERAGE_DIR.mkdir(exist_ok=True)
    for pattern in ["*.coverage", "*.xml"]:
        for path in COVERAGE_DIR.glob(pattern):
            path.unlink()


def check_coverage() -> None:
    clean_coverage()
    run(
        [
            "opam",
            "exec",
            "--switch=guarded-thought",
            "--",
            "dune",
            "runtest",
            "--instrument-with",
            "bisect_ppx",
            "--force",
        ],
        env={"BISECT_ENABLE": "YES", "BISECT_FILE": str(COVERAGE_PREFIX)},
    )
    coverage_files = sorted(str(path.relative_to(ROOT)) for path in COVERAGE_DIR.glob("*.coverage"))
    if not coverage_files:
        raise SystemExit("no coverage files produced")
    summary = run(
        [
            "opam",
            "exec",
            "--switch=guarded-thought",
            "--",
            "bisect-ppx-report",
            "summary",
            "--per-file",
            *coverage_files,
        ]
    )
    if "100.00 %" not in summary or "lib/rfc6570.ml" not in summary:
        raise SystemExit("coverage summary does not prove 100% for lib/rfc6570.ml")
    run(
        [
            "opam",
            "exec",
            "--switch=guarded-thought",
            "--",
            "bisect-ppx-report",
            "cobertura",
            str(COVERAGE_XML.relative_to(ROOT)),
            *coverage_files,
        ]
    )
    print(f"coverage_files\t{len(coverage_files)}")


def main() -> None:
    check_generated_reproducible()
    run(["opam", "exec", "--switch=guarded-thought", "--", "dune", "build", "@fmt"])
    run(["opam", "exec", "--switch=guarded-thought", "--", "dune", "build", "@runtest", "@install"])
    check_wip_tsv_field_counts()
    check_generated_summary_counts()
    check_normative_statuses()
    check_stale_markers()
    check_coverage()
    print("absolute_gate\tOK")


if __name__ == "__main__":
    main()
