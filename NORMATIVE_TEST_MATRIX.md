# RFC6570 Normative Test Matrix

This stable matrix is the conformance test matrix for the documented RFC 6570
URI Template parser and expander scope.

| Area | Source | Tests | Status |
|---|---|---|---|
| Official examples | `uritemplate-test` pinned suite | `test_spec_examples`, `test_spec_examples_by_section`, `test_extended_examples` | passing |
| Negative syntax | `uritemplate-test/negative-tests.json` plus generated ABNF corpus | `test_negative_examples`, `test_parser_abnf`, `test_parser_error_locations`, `test_generated_abnf_corpus` | passing official, generated, and typed-location matrix |
| Operator table | RFC6570 Sections 3.2.2-3.2.9 plus generated operator matrix | `test_expansion_matrix`, `test_operator_cross_product`, `test_generated_operator_matrix` | passing official and generated matrix |
| Value model | RFC6570 Sections 2.3 and 3.2.1 | `test_value_model`, `test_operator_cross_product`, `test_generated_operator_matrix` | passing official, local, and generated matrix |
| Unicode value validation | RFC6570 Section 1.6 | `test_unicode_value_model` | passing scalar/list/assoc invalid and valid UTF-8 matrix |
| NFC policy | RFC6570 Section 1.6 | `test_nfc_policy`, `test_generated_operator_matrix` | passing default normalization and Preserve opt-out matrix |
| Encoding | RFC6570 Section 1.6 and 3.2.1 | `test_encoding`, `test_unicode_value_model` | passing official and local matrix |
| Imported ABNF literals | RFC6570 Sections 1.5, 2.1, and verified Errata-ID 6937 | `test_parser_abnf`, `test_literal_expansion`, `test_generated_abnf_corpus` | passing local and generated boundary matrix |
| Prefix and explode | RFC6570 Sections 2.4.1 and 2.4.2 | `test_parser_abnf`, `test_encoding`, `test_expansion_matrix`, `test_error_reporting`, `test_generated_abnf_corpus`, `test_generated_operator_matrix` | passing official, local, and generated matrix |
| Diagnostic expansion | RFC6570 Section 3 | `test_diagnostic_expansion` | passing partial-output and typed-error matrix |
| Generated evidence | RFC6570 finite ABNF/operator axes | `test_generated_abnf_corpus`, `test_generated_operator_matrix`; Dune-generated `test/generated/*.tsv` targets | passing 51-row ABNF corpus and 150-row operator matrix |
| Public API scope | RFC6570 Section 1.4 | `test_public_api` | passing |
| Library coverage | `tools/check_absolute_gate.py` | `bisect_ppx` over `lib/rfc6570.ml` | 100.00% line coverage, `656/656` |

Absolute gate command: `python3 tools/check_absolute_gate.py`. Current result: `absolute_gate\tOK`.
