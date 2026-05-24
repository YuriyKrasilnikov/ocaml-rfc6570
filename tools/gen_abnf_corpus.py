#!/usr/bin/env python3
from __future__ import annotations

import argparse
from collections import Counter
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
DEFAULT_OUT_DIR = ROOT / "test" / "generated"

HEADER = [
    "id",
    "requirement_refs",
    "case_kind",
    "template",
    "expected_error",
    "expected_variables",
    "expected_expansion",
]


def esc(text: str) -> str:
    out: list[str] = []
    for ch in text:
        code = ord(ch)
        if ch == "\\":
            out.append("\\\\")
        elif ch == "\n":
            out.append("\\n")
        elif ch == "\t":
            out.append("\\t")
        elif code < 0x20 or code >= 0x7F:
            out.append(f"\\u{{{code:X}}}")
        else:
            out.append(ch)
    return "".join(out)


def ok(
    case_id: str,
    refs: str,
    template: str,
    variables: str,
    expected_expansion: str,
) -> list[str]:
    return [
        case_id,
        refs,
        "parse_ok",
        esc(template),
        "",
        variables or "-",
        expected_expansion,
    ]


def err(case_id: str, refs: str, template: str, expected_error: str) -> list[str]:
    return [
        case_id,
        refs,
        "parse_error",
        esc(template),
        expected_error,
        "-",
        "-",
    ]


ROWS = [
    ok(
        "ABNF-LITERAL-ASCII",
        "NORM-003;NORM-007;NORM-026",
        "AZaz09-._~:/?#[]@!$&'()*+,;=/{var}",
        "var",
        "AZaz09-._~:/?#[]@!$&'()*+,;=/x",
    ),
    ok(
        "ABNF-LITERAL-PCT",
        "NORM-003;NORM-007;NORM-026",
        "before%20after/{var}",
        "var",
        "before%20after/x",
    ),
    ok(
        "ABNF-LITERAL-UCSCHAR-A0",
        "NORM-003;NORM-007;NORM-027",
        "\u00A0/{var}",
        "var",
        "%C2%A0/x",
    ),
    ok(
        "ABNF-LITERAL-UCSCHAR-FOUR-BYTE",
        "NORM-003;NORM-007;NORM-027",
        "\U00010000/{var}",
        "var",
        "%F0%90%80%80/x",
    ),
    ok(
        "ABNF-LITERAL-IPRIVATE",
        "NORM-003;NORM-007;NORM-027",
        "\uE000/{var}",
        "var",
        "%EE%80%80/x",
    ),
    ok("ABNF-EXPR-SIMPLE", "NORM-006;NORM-008;NORM-028", "{var}", "var", "x"),
    ok("ABNF-EXPR-PLUS", "NORM-006;NORM-008;NORM-009", "{+var}", "var", "x"),
    ok("ABNF-EXPR-HASH", "NORM-006;NORM-008;NORM-009", "{#var}", "var", "#x"),
    ok("ABNF-EXPR-DOT", "NORM-006;NORM-008;NORM-009", "{.var}", "var", ".x"),
    ok("ABNF-EXPR-SLASH", "NORM-006;NORM-008;NORM-009", "{/var}", "var", "/x"),
    ok("ABNF-EXPR-SEMI", "NORM-006;NORM-008;NORM-009", "{;var}", "var", ";var=x"),
    ok("ABNF-EXPR-QUERY", "NORM-006;NORM-008;NORM-009", "{?var}", "var", "?var=x"),
    ok("ABNF-EXPR-AMP", "NORM-006;NORM-008;NORM-009", "{&var}", "var", "&var=x"),
    ok("ABNF-EXPR-COMMA", "NORM-006;NORM-008", "{var,empty}", "var;empty", "x,"),
    ok("ABNF-VARNAME-DOTTED", "NORM-006;NORM-012", "{semi.dot}", "semi.dot", "dotted"),
    ok("ABNF-VARNAME-PCT", "NORM-003;NORM-012", "{%24id}", "%24id", "pct"),
    ok("ABNF-VARNAME-DOT-PCT", "NORM-003;NORM-012", "{a.%62}", "a.%62", "pctdot"),
    ok("ABNF-PREFIX-1", "NORM-006;NORM-017", "{long:1}", "long", "a"),
    ok("ABNF-PREFIX-9", "NORM-006;NORM-017", "{long:9}", "long", "abcdefghi"),
    ok("ABNF-PREFIX-10", "NORM-006;NORM-017", "{long:10}", "long", "abcdefghij"),
    ok("ABNF-PREFIX-9999", "NORM-006;NORM-017", "{long:9999}", "long", "abcdefghij"),
    ok("ABNF-EXPLODE-LIST", "NORM-006;NORM-020", "{list*}", "list", "red,green"),
    ok("ABNF-EXPLODE-ASSOC", "NORM-006;NORM-020", "{keys*}", "keys", "a=1,b=2"),
    err("ABNF-ERR-LITERAL-SPACE", "NORM-007", "bad space/{var}", "Invalid_literal"),
    err("ABNF-ERR-LITERAL-DQUOTE", "NORM-007", "bad\"/{var}", "Invalid_literal"),
    err("ABNF-ERR-LITERAL-LT", "NORM-007", "bad</{var}", "Invalid_literal"),
    err("ABNF-ERR-LITERAL-CARET", "NORM-007", "bad^/{var}", "Invalid_literal"),
    err("ABNF-ERR-LITERAL-PCT", "NORM-003;NORM-007", "bad%zz/{var}", "Invalid_percent_triplet"),
    err("ABNF-ERR-LITERAL-PCT-SHORT", "NORM-003;NORM-007", "bad%2/{var}", "Invalid_percent_triplet"),
    err("ABNF-ERR-UNMATCHED-OPEN", "NORM-006;NORM-008", "before{var", "Unmatched_open_brace"),
    err("ABNF-ERR-UNMATCHED-CLOSE", "NORM-006;NORM-008", "before}after", "Unmatched_close_brace"),
    err("ABNF-ERR-NESTED-OPEN", "NORM-006;NORM-008", "before{var{bad}}", "Unmatched_open_brace"),
    err("ABNF-ERR-EMPTY-EXPR", "NORM-006;NORM-008", "{}", "Empty_expression"),
    err("ABNF-ERR-OPERATOR-NO-VAR", "NORM-006;NORM-008", "{+}", "Empty_expression"),
    err("ABNF-ERR-FUTURE-EQUAL", "NORM-010", "{=var}", "Reserved_operator"),
    err("ABNF-ERR-FUTURE-COMMA", "NORM-010", "{,var}", "Reserved_operator"),
    err("ABNF-ERR-FUTURE-BANG", "NORM-010", "{!var}", "Reserved_operator"),
    err("ABNF-ERR-FUTURE-AT", "NORM-010", "{@var}", "Reserved_operator"),
    err("ABNF-ERR-FUTURE-PIPE", "NORM-010", "{|var}", "Reserved_operator"),
    err("ABNF-ERR-VARNAME-DOLLAR", "NORM-011", "{$id}", "Invalid_varname"),
    err("ABNF-ERR-VARNAME-PARENS", "NORM-011", "{(id)}", "Invalid_varname"),
    err("ABNF-ERR-VARNAME-HYPHEN", "NORM-006;NORM-008", "{bad-name}", "Invalid_varname"),
    err("ABNF-ERR-VARNAME-EMPTY-SEGMENT", "NORM-006;NORM-008", "{a..b}", "Invalid_varname"),
    err("ABNF-ERR-VARNAME-TRAILING-COMMA", "NORM-006;NORM-008", "{var,}", "Invalid_varname"),
    err("ABNF-ERR-VARNAME-BAD-PCT", "NORM-003;NORM-012", "{%2G}", "Invalid_percent_triplet"),
    err("ABNF-ERR-PREFIX-ZERO", "NORM-017", "{long:0}", "Invalid_prefix_modifier"),
    err("ABNF-ERR-PREFIX-LEADING-ZERO", "NORM-017", "{long:0001}", "Invalid_prefix_modifier"),
    err("ABNF-ERR-PREFIX-TOO-LONG", "NORM-017", "{long:10000}", "Invalid_prefix_modifier"),
    err("ABNF-ERR-PREFIX-EMPTY", "NORM-017", "{long:}", "Invalid_prefix_modifier"),
    err("ABNF-ERR-PREFIX-NONDIGIT", "NORM-017", "{long:a}", "Invalid_prefix_modifier"),
    err("ABNF-ERR-EXPLODE-WITH-PREFIX", "NORM-017;NORM-020", "{list:*}", "Invalid_prefix_modifier"),
]


def write_tsv(out_dir: Path) -> None:
    out = out_dir / "abnf-corpus.tsv"
    out.parent.mkdir(parents=True, exist_ok=True)
    with out.open("w", encoding="utf-8", newline="\n") as f:
        f.write("\t".join(HEADER) + "\n")
        for row in ROWS:
            f.write("\t".join(row) + "\n")


def write_summary(out_dir: Path) -> None:
    summary = out_dir / "abnf-corpus.summary"
    by_kind = Counter(row[2] for row in ROWS)
    refs = Counter(ref for row in ROWS for ref in row[1].split(";"))
    with summary.open("w", encoding="utf-8", newline="\n") as f:
        f.write("generated_by\ttools/gen_abnf_corpus.py\n")
        f.write(f"rows\t{len(ROWS)}\n")
        for kind, count in sorted(by_kind.items()):
            f.write(f"kind.{kind}\t{count}\n")
        for ref, count in sorted(refs.items()):
            f.write(f"ref.{ref}\t{count}\n")


if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("--out-dir", type=Path, default=DEFAULT_OUT_DIR)
    args = parser.parse_args()
    write_tsv(args.out_dir)
    write_summary(args.out_dir)
