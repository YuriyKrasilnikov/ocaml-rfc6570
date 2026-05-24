(lang dune 3.10)

(name rfc6570)

(generate_opam_files true)

(license ISC)

(authors "Yuriy Krasilnikov")

(maintainers "Yuriy Krasilnikov")

(source (github YuriyKrasilnikov/ocaml-rfc6570))

(homepage "https://github.com/YuriyKrasilnikov/ocaml-rfc6570")

(bug_reports "https://github.com/YuriyKrasilnikov/ocaml-rfc6570/issues")

(formatting
 (enabled_for ocaml))

(package
 (name rfc6570)
 (synopsis "RFC 6570 URI Template parser and expander for OCaml")
 (description
  "A strict RFC 6570 URI Template parser and expander for OCaml. The package scope is URI Template expansion, not URI parsing, URI resolution, IDNA, or reverse matching.")
 (depends
  (ocaml
   (>= 4.14))
  (dune
   (>= 3.10))
  (uunf
   (>= 17.0.0))
  (conf-python-3
   :with-test)
  (alcotest
   :with-test)
  (yojson
   :with-test)
  (bisect_ppx
   :with-test)))
