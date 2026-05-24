# RFC6570 Specification Conformance

This package targets RFC 6570 URI Template parsing and expansion.

Current status: release evidence is green for the current implementation. The
parser, value model, encoder, expansion engine, and diagnostic expansion API
pass the pinned official `uritemplate-test` positive, extended, by-section, and
negative suites, plus local boundary tests for value semantics, encoding,
imported literal ABNF, generated ABNF evidence, generated operator evidence,
expansion, typed error locations, Section 3 diagnostic behavior, Unicode value
validation, NFC normalization, and roundtripping. The local evidence gate
reports 100.00% `bisect_ppx` coverage for `lib/rfc6570.ml` (`656/656`). The
generated operator matrix covers all RFC 6570 operators across scalar, list,
assoc, empty, undefined, explode, named-mode, encoding, prefix, and
normalization behavior.

The RFC6570 conformance claim is scoped to URI Template processor behavior:
strict parsing, strict expansion, and diagnostic expansion. The evidence is:

- every pinned official `uritemplate-test` case remains executed and passing;
- every stable local matrix row has implementation evidence and remains passing;
- generated ABNF and operator matrices are produced by Dune rules and consumed
  by tests;
- Section 3 diagnostic malformed-template behavior is covered locally;
- the consolidated local evidence gate passes through
  `python3 tools/check_absolute_gate.py`.

Out of scope:

- URI parsing and resolution;
- IDNA and IRI host processing;
- reverse matching.

Documented policy:

- expansion returns URI-reference strings and does not parse or resolve them;
- variable bindings are cached once per expansion for stable repeated
  references;
- associative values preserve caller-supplied input order;
- undefined composite members are ignored;
- scalar, list item, assoc key, and assoc value strings must be valid UTF-8;
- values are NFC-normalized by default; `Preserve` is available through the
  explicit options API for byte-preserving expansion after UTF-8 validation;
- `expand_diagnostic` is diagnostic-only and preserves malformed expressions or
  raw remainders as specified by RFC6570 Section 3;
- expanded URI-reference strings must be checked by callers before dereference,
  fetching, authorization, or scheme-specific use.
