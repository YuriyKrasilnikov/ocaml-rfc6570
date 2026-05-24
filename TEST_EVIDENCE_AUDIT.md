# Test Evidence Audit

This document is the final evidence pass over the repository test story.

It complements:

- `README.md`
- `SPEC_CONFORMANCE.md`
- `NORMATIVE_TEST_MATRIX.md`

Its purpose is to separate package-safe tests, local release evidence, and
declared exclusions.

## Test Runner Tiers

The package-safe test surface is the default `dune runtest` suite and the opam
`with-test` build. It uses committed fixtures and Dune-generated evidence
files.

The local release evidence gate is:

```sh
python3 tools/check_absolute_gate.py
```

That gate verifies:

- generated ABNF and operator evidence is reproducible;
- `dune build @fmt` passes in the explicit switch used for release checks;
- `dune build @runtest @install` passes in the explicit switch used for release
  checks;
- internal TSV ledger files have consistent field counts;
- Dune-generated corpora are present and have expected row counts;
- stable documentation has no stale working markers for the public claim;
- `bisect_ppx` reports 100.00% line coverage for `lib/rfc6570.ml`.

## Normative-Covered Runtime Surfaces

The following runtime surfaces have direct test evidence:

- RFC 6570 template parsing;
- RFC 6570 template expansion;
- operator behavior for Sections 3.2.2 through 3.2.9;
- variable modifiers, including prefix and explode;
- scalar, list, and associative value expansion;
- undefined, empty, and partially undefined composite value behavior;
- literal and value encoding behavior;
- invalid UTF-8 value rejection;
- default NFC value normalization and Preserve opt-out behavior;
- typed strict parser and expansion errors;
- diagnostic expansion for malformed-template processing.

Evidence basis:

- pinned `uritemplate-test` fixtures under `test/uritemplate-test/`;
- Dune-generated corpora under `test/generated/`;
- local boundary tests under `test/test_*.ml`;
- the local release evidence gate.

## Generated Evidence Boundary

Generated ABNF and operator matrices are build artifacts because package tests
consume them. The generator tools are committed so the local evidence gate can
prove that the Dune-generated files are reproducible.

This keeps the package test surface independent from internal ledgers while still
making release evidence reproducible.

## Declared Exclusions

The package does not implement:

- URI parsing;
- URI resolution;
- IDNA or IRI host processing;
- reverse matching or extraction.

These exclusions are package scope boundaries, not forgotten test gaps.
