# Changes

## Unreleased

- Added the initial RFC 6570 URI Template parser and expander.
- Added strict parsing, strict expansion, diagnostic expansion, typed values,
  typed parse errors, typed expansion errors, UTF-8 value validation, and
  default NFC value normalization with an explicit Preserve option.
- Added pinned `uritemplate-test` positive, extended, by-section, and negative
  suites to the package test surface.
- Added Dune-generated ABNF and operator evidence consumed by tests.
- Added stable conformance documentation, a normative test matrix, and a test
  evidence audit.
- Added an explicit local evidence gate:
  `python3 tools/check_absolute_gate.py`.

No URI parser, URI resolver, IDNA/IRI processor, or reverse matcher is included
in this package.
