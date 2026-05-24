#!/usr/bin/env python3
from __future__ import annotations

import argparse
import re
import unicodedata
from collections import Counter
from pathlib import Path
from typing import Iterable

ROOT = Path(__file__).resolve().parents[1]
DEFAULT_OUT_DIR = ROOT / "test" / "generated"

HEADER = [
    "id",
    "requirement_refs",
    "operator_axis",
    "value_axis",
    "modifier_axis",
    "options",
    "template",
    "expected",
]

UNRESERVED = set("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-._~")
RESERVED = set(":/?#[]@!$&'()*+,;=")
PCT = re.compile(r"%[0-9A-Fa-f]{2}")

OPERATORS = [
    ("NUL", "", "", ",", False, "", "U", "NORM-038"),
    ("PLUS", "+", "", ",", False, "", "UR", "NORM-039"),
    ("HASH", "#", "#", ",", False, "", "UR", "NORM-040"),
    ("DOT", ".", ".", ".", False, "", "U", "NORM-041"),
    ("SLASH", "/", "/", "/", False, "", "U", "NORM-042"),
    ("SEMI", ";", ";", ";", True, "", "U", "NORM-043"),
    ("QUERY", "?", "?", "&", True, "=", "U", "NORM-044"),
    ("AMP", "&", "&", "&", True, "=", "U", "NORM-045"),
]

VALUES = {
    "var": ("scalar", "value"),
    "empty": ("scalar", ""),
    "reserved": ("scalar", "a/b?c%20d e!"),
    "badpct": ("scalar", "x%zz%"),
    "long": ("scalar", "abcdefghij"),
    "decomposed": ("scalar", "e\u0301"),
    "list": ("list", ["red", "green", "blue"]),
    "list_empty_item": ("list", ["red", "", "blue"]),
    "partial_list": ("list", [None, "red", None, "blue"]),
    "undefined_list": ("list", [None]),
    "empty_list": ("list", []),
    "keys": ("assoc", [("semi", ";"), ("dot", "."), ("comma", ",")]),
    "empty_value_keys": ("assoc", [("a", ""), ("b", "2")]),
    "partial_keys": ("assoc", [("a", None), ("b", "2"), ("c", None)]),
    "undefined_keys": ("assoc", [("a", None)]),
    "empty_keys": ("assoc", []),
    "missing": ("undefined", None),
}


def pct_encode_byte(byte: int) -> str:
    return f"%{byte:02X}"


def encode(text: str, allow: str) -> str:
    out: list[str] = []
    i = 0
    while i < len(text):
        ch = text[i]
        if ch in UNRESERVED or (allow == "UR" and ch in RESERVED):
            out.append(ch)
            i += 1
        elif allow == "UR" and ch == "%" and PCT.match(text, i):
            out.append(text[i : i + 3])
            i += 3
        else:
            out.extend(pct_encode_byte(byte) for byte in ch.encode("utf-8"))
            i += 1
    return "".join(out)


def normalize(text: str, options: str) -> str:
    if options == "default":
        return unicodedata.normalize("NFC", text)
    if options == "preserve":
        return text
    raise ValueError(options)


def defined_items(kind: str, value):
    if kind == "list":
        return [item for item in value if item is not None]
    if kind == "assoc":
        return [(key, item) for key, item in value if item is not None]
    raise ValueError(kind)


def expand_var(
    name: str,
    value_name: str,
    modifier: str,
    operator: tuple[str, str, str, str, bool, str, str, str],
    options: str,
) -> str | None:
    _, _, _, sep, named, ifemp, allow, _ = operator
    kind, raw_value = VALUES[value_name]
    if kind == "undefined":
        return None
    if kind == "scalar":
        value = normalize(raw_value, options)
        if modifier.startswith(":"):
            value = value[: int(modifier[1:])]
        encoded = encode(value, allow)
        if named:
            return name + ifemp if encoded == "" else name + "=" + encoded
        return encoded
    if kind == "list":
        items = [normalize(item, options) for item in defined_items(kind, raw_value)]
        if not items:
            return None
        encoded_items = [encode(item, allow) for item in items]
        if modifier == "*":
            if named:
                return sep.join(
                    name + ifemp if item == "" else name + "=" + item
                    for item in encoded_items
                )
            return sep.join(encoded_items)
        value = ",".join(encoded_items)
        if named:
            return name + ifemp if value == "" else name + "=" + value
        return value
    if kind == "assoc":
        items = [
            (normalize(key, options), normalize(item, options))
            for key, item in defined_items(kind, raw_value)
        ]
        if not items:
            return None
        if modifier == "*":
            pairs = []
            for key, value in items:
                encoded_key = encode(key, allow)
                encoded_value = encode(value, allow)
                if named and encoded_value == "":
                    pairs.append(encoded_key + ifemp)
                else:
                    pairs.append(encoded_key + "=" + encoded_value)
            return sep.join(pairs)
        pieces = []
        for key, value in items:
            pieces.append(encode(key, allow))
            pieces.append(encode(value, allow))
        value = ",".join(pieces)
        if named:
            return name + ifemp if value == "" else name + "=" + value
        return value
    raise ValueError(kind)


def expand(
    value_name: str,
    modifier: str,
    operator: tuple[str, str, str, str, bool, str, str, str],
    options: str,
) -> str:
    _, _, first, _, _, _, _, _ = operator
    expanded = expand_var(value_name, value_name, modifier, operator, options)
    if expanded is None:
        return ""
    return first + expanded


def template(op_symbol: str, value_name: str, modifier: str) -> str:
    return "{" + op_symbol + value_name + modifier + "}"


def row(
    case_id: str,
    refs: str,
    operator_axis: str,
    value_axis: str,
    modifier_axis: str,
    options: str,
    template_text: str,
    expected: str,
) -> list[str]:
    return [
        case_id,
        refs,
        operator_axis,
        value_axis,
        modifier_axis,
        options,
        template_text,
        expected,
    ]


def generated_rows() -> Iterable[list[str]]:
    scalar_values = ["var", "empty", "reserved", "badpct", "missing"]
    composite_values = [
        ("list", ""),
        ("list", "*"),
        ("list_empty_item", "*"),
        ("partial_list", "*"),
        ("undefined_list", ""),
        ("empty_list", ""),
        ("keys", ""),
        ("keys", "*"),
        ("empty_value_keys", "*"),
        ("partial_keys", "*"),
        ("undefined_keys", ""),
        ("empty_keys", ""),
    ]
    for operator in OPERATORS:
        op_name, op_symbol = operator[0], operator[1]
        refs = "NORM-009;NORM-020;NORM-028;NORM-030;NORM-031;NORM-033;" + operator[7]
        for value_name in scalar_values:
            yield row(
                f"OP-{op_name}-{value_name.upper()}",
                refs,
                op_name,
                value_name,
                "none",
                "default",
                template(op_symbol, value_name, ""),
                expand(value_name, "", operator, "default"),
            )
        yield row(
            f"OP-{op_name}-PREFIX-3",
            refs + ";NORM-017",
            op_name,
            "long",
            "prefix:3",
            "default",
            template(op_symbol, "long", ":3"),
            expand("long", ":3", operator, "default"),
        )
        for value_name, modifier in composite_values:
            mod_axis = "explode" if modifier == "*" else "none"
            yield row(
                f"OP-{op_name}-{value_name.upper()}-{mod_axis.upper()}",
                refs + ";NORM-035",
                op_name,
                value_name,
                mod_axis,
                "default",
                template(op_symbol, value_name, modifier),
                expand(value_name, modifier, operator, "default"),
            )
    repeated_rows = [
        ("QUERY", "{?var,var}", "?var=value&var=value", "NORM-023;NORM-032;NORM-044"),
        ("AMP", "{&var,var}", "&var=value&var=value", "NORM-023;NORM-032;NORM-045"),
    ]
    for op_name, template_text, expected, refs in repeated_rows:
        yield row(
            f"OP-{op_name}-REPEATED-VAR",
            refs,
            op_name,
            "var",
            "repeated",
            "default",
            template_text,
            expected,
        )

    for options in ["default", "preserve"]:
        for op_name in ["NUL", "PLUS"]:
            operator = next(op for op in OPERATORS if op[0] == op_name)
            yield row(
                f"OP-{op_name}-NFC-{options.upper()}",
                "NORM-005;NORM-030;NORM-033",
                op_name,
                "decomposed",
                "none",
                options,
                template(operator[1], "decomposed", ""),
                expand("decomposed", "", operator, options),
            )


def write_tsv(out_dir: Path, rows: list[list[str]]) -> None:
    out = out_dir / "operator-matrix.tsv"
    out.parent.mkdir(parents=True, exist_ok=True)
    with out.open("w", encoding="utf-8", newline="\n") as f:
        f.write("\t".join(HEADER) + "\n")
        for fields in rows:
            f.write("\t".join(fields) + "\n")


def write_summary(out_dir: Path, rows: list[list[str]]) -> None:
    summary = out_dir / "operator-matrix.summary"
    by_operator = Counter(row[2] for row in rows)
    by_modifier = Counter(row[4] for row in rows)
    refs = Counter(ref for row in rows for ref in row[1].split(";"))
    with summary.open("w", encoding="utf-8", newline="\n") as f:
        f.write("generated_by\ttools/gen_operator_matrix.py\n")
        f.write(f"rows\t{len(rows)}\n")
        for operator, count in sorted(by_operator.items()):
            f.write(f"operator.{operator}\t{count}\n")
        for modifier, count in sorted(by_modifier.items()):
            f.write(f"modifier.{modifier}\t{count}\n")
        for ref, count in sorted(refs.items()):
            f.write(f"ref.{ref}\t{count}\n")


if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("--out-dir", type=Path, default=DEFAULT_OUT_DIR)
    args = parser.parse_args()
    rows = list(generated_rows())
    write_tsv(args.out_dir, rows)
    write_summary(args.out_dir, rows)
