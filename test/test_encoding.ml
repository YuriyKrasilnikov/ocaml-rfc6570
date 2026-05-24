let parse raw =
  match Rfc6570.parse raw with
  | Ok template -> template
  | Error errors ->
      Alcotest.failf "parse %S failed with %d error(s)" raw (List.length errors)

let expand raw value =
  let bindings = function
    | "var" -> Rfc6570.String value
    | _ -> Rfc6570.Undefined
  in
  match Rfc6570.expand (parse raw) bindings with
  | Ok value -> value
  | Error errors ->
      Alcotest.failf "expand %S failed with %d error(s)" raw
        (List.length errors)

let test_simple_encoding_allows_only_unreserved () =
  Alcotest.(check string)
    "reserved and percent encoded" "a%2Fb%3Fc%2520d%20e%21"
    (expand "{var}" "a/b?c%20d e!");
  Alcotest.(check string)
    "complete unreserved sample" "AZaz09-._~"
    (expand "{var}" "AZaz09-._~")

let test_reserved_encoding_preserves_reserved_and_pct_triplets () =
  Alcotest.(check string)
    "reserved set" ":/?#[]@!$&'()*+,;="
    (expand "{+var}" ":/?#[]@!$&'()*+,;=");
  Alcotest.(check string)
    "reserved and valid pct preserved" "a/b?c%20d%20e!"
    (expand "{+var}" "a/b?c%20d e!");
  Alcotest.(check string)
    "invalid pct encoded" "x%25zz%25" (expand "{+var}" "x%zz%")

let test_non_ascii_utf8_is_pct_encoded () =
  Alcotest.(check string) "utf8 scalar" "caf%C3%A9" (expand "{var}" "café")

let test_prefix_counts_unicode_characters () =
  Alcotest.(check string)
    "unicode prefix" "caf%C3%A9"
    (expand "{var:4}" "cafétéria");
  Alcotest.(check string)
    "three byte prefix" "%E2%82%AC"
    (expand "{var:1}" "€uro");
  Alcotest.(check string)
    "four byte prefix" "%F0%9F%98%80" (expand "{var:1}" "😀x")

let test_prefix_does_not_split_pct_triplets () =
  Alcotest.(check string)
    "reserved pct triplet prefix" "a%2F"
    (expand "{+var:2}" "a%2Fb");
  Alcotest.(check string)
    "simple pct triplet prefix" "a%252F" (expand "{var:2}" "a%2Fb")

let () =
  Alcotest.run "rfc6570-encoding"
    [
      ( "encoding",
        [
          Alcotest.test_case "simple allow set" `Quick
            test_simple_encoding_allows_only_unreserved;
          Alcotest.test_case "reserved allow set" `Quick
            test_reserved_encoding_preserves_reserved_and_pct_triplets;
          Alcotest.test_case "utf8 pct encoding" `Quick
            test_non_ascii_utf8_is_pct_encoded;
          Alcotest.test_case "unicode prefix" `Quick
            test_prefix_counts_unicode_characters;
          Alcotest.test_case "pct-triplet prefix" `Quick
            test_prefix_does_not_split_pct_triplets;
        ] );
    ]
