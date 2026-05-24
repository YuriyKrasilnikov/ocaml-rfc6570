# rfc6570

RFC 6570 URI Template parser and expander for OCaml.

This package implements RFC 6570 URI Template parsing and expansion for OCaml.
The release evidence supports a scoped 100% RFC 6570 conformance claim for the
documented URI Template processor surface.

## Installation

```
opam install rfc6570
```

For the normative baseline, see [SPEC_CONFORMANCE.md](SPEC_CONFORMANCE.md).
For requirement-to-test coverage, see
[NORMATIVE_TEST_MATRIX.md](NORMATIVE_TEST_MATRIX.md).
For package and local evidence status, see
[TEST_EVIDENCE_AUDIT.md](TEST_EVIDENCE_AUDIT.md).
For release notes, see [CHANGES.md](CHANGES.md).

## Scope

Included:

- URI Template parsing;
- URI Template expansion;
- diagnostic expansion for malformed-template processing;
- typed RFC 6570 value model;
- typed parse and expansion errors.

Excluded:

- URI parsing;
- URI resolution;
- IDNA or IRI host processing;
- reverse matching or extraction.

## Value Semantics

`Rfc6570.expand` evaluates each variable binding once per expansion and reuses
that value for repeated references to the same variable.

`Assoc` values expand in the input list order supplied by the caller. Undefined
list or assoc members are ignored. Empty lists and assocs with no defined
members are treated as undefined.

Value strings must be valid UTF-8. `expand` and `expand_diagnostic` reject
invalid scalar, list item, assoc key, and assoc value strings with
`Invalid_value_utf8` before encoding or prefix slicing.

By default, value strings are NFC-normalized before expansion. Use
`expand_with { normalization = Preserve }` or `expand_diagnostic_with` with the
same option when byte-preserving behavior is required after UTF-8 validation.

Parse and expansion failures are returned as `Rfc6570.error` values. The public
error variants are part of the library interface.

`Rfc6570.expand_diagnostic` expands a raw template in diagnostic mode. Malformed
expressions are preserved in the output and reported as typed errors; malformed
literal or outside-expression text stops processing and leaves the remainder
unexpanded. This API is for diagnostics and does not weaken strict `parse` or
`expand`.

## Status

The parser, value model, encoder, and expansion engine are driven by pinned
official fixtures. The pinned official `uritemplate-test` positive, extended,
by-section, and negative suites pass. Local boundary tests pass, including
imported literal ABNF, literal expansion encoding, generated ABNF evidence,
generated operator/value/modifier evidence, typed parser error locations,
Unicode value validation, NFC value normalization, and RFC6570 Section 3
diagnostic expansion behavior. The local evidence gate reports 100.00%
`bisect_ppx` coverage for `lib/rfc6570.ml` (`656/656`).

The release evidence gate is green for the strict parser/expander, diagnostic
API, generated evidence, Unicode/NFC policy, documentation consistency, and
coverage. The reproducible local gate is `python3 tools/check_absolute_gate.py`.

## Security Considerations

URI Templates are data. This package parses templates and expands them to
URI-reference strings; it does not fetch, resolve, authorize, normalize hosts,
or choose URI schemes. Callers that dereference an expanded URI reference must
validate the template source, supplied values, resulting scheme/authority/path,
and any IDNA/IRI policy at their own trust boundary.

## License

ISC
