let parse raw =
  match Rfc6570.parse raw with
  | Ok template -> template
  | Error errors ->
      Alcotest.failf "parse %S failed with %d error(s)" raw (List.length errors)

let expand raw =
  let bindings = function
    | "var" -> Rfc6570.String "x"
    | _ -> Rfc6570.Undefined
  in
  match Rfc6570.expand (parse raw) bindings with
  | Ok value -> value
  | Error errors ->
      Alcotest.failf "expand %S failed with %d error(s)" raw
        (List.length errors)

let check raw expected = Alcotest.(check string) raw expected (expand raw)

let test_non_uri_unicode_literal_is_pct_encoded () =
  check "café/{var}" "caf%C3%A9/x"

let test_iprivate_literal_is_pct_encoded () =
  check ("\238\128\128" ^ "/{var}") "%EE%80%80/x"

let test_utf8_literal_decoder_boundaries () =
  check ("\224\160\128" ^ "/{var}") "%E0%A0%80/x";
  check ("\226\130\172" ^ "/{var}") "%E2%82%AC/x";
  check ("\237\159\191" ^ "/{var}") "%ED%9F%BF/x";
  check ("\240\144\128\128" ^ "/{var}") "%F0%90%80%80/x";
  check ("\241\128\128\128" ^ "/{var}") "%F1%80%80%80/x";
  check ("\244\143\191\189" ^ "/{var}") "%F4%8F%BF%BD/x"

let test_literal_pct_triplet_is_preserved () =
  check "before%20after/{var}" "before%20after/x"

let test_uri_allowed_ascii_literals_are_preserved () =
  check "AZaz09-._~:/?#[]@!$&'()*+,;=/{var}" "AZaz09-._~:/?#[]@!$&'()*+,;=/x"

let parse_error raw =
  match Rfc6570.parse raw with
  | Ok _ -> Alcotest.failf "parse %S unexpectedly succeeded" raw
  | Error [ error ] -> error
  | Error errors ->
      Alcotest.failf "parse %S returned %d errors" raw (List.length errors)

let check_invalid_literal raw expected_offset expected_codepoint =
  match parse_error raw with
  | Rfc6570.Invalid_literal { position; character } ->
      Alcotest.(check int) "offset" expected_offset position.Rfc6570.offset;
      Alcotest.(check int)
        "character" expected_codepoint (Uchar.to_int character)
  | _ -> Alcotest.fail "expected Invalid_literal"

let test_invalid_utf8_literal_byte_is_rejected () =
  check_invalid_literal ("bad" ^ "\255" ^ "/{var}") 3 0xff

let test_non_ucschar_unicode_literal_is_rejected () =
  check_invalid_literal ("bad" ^ "\194\128" ^ "/{var}") 3 0x80

let test_non_literal_unicode_scalar_is_rejected () =
  check_invalid_literal ("bad" ^ "\239\191\191" ^ "/{var}") 3 0xFFFF

let test_surrogate_utf8_literal_is_rejected () =
  check_invalid_literal ("bad" ^ "\237\160\128" ^ "/{var}") 3 0xED

let test_above_unicode_range_literal_is_rejected () =
  check_invalid_literal ("bad" ^ "\244\144\128\128" ^ "/{var}") 3 0xF4

let () =
  Alcotest.run "rfc6570-literal-expansion"
    [
      ( "literal expansion",
        [
          Alcotest.test_case "non-uri unicode literal pct encoded" `Quick
            test_non_uri_unicode_literal_is_pct_encoded;
          Alcotest.test_case "iprivate literal pct encoded" `Quick
            test_iprivate_literal_is_pct_encoded;
          Alcotest.test_case "utf8 literal decoder boundaries" `Quick
            test_utf8_literal_decoder_boundaries;
          Alcotest.test_case "literal pct triplet preserved" `Quick
            test_literal_pct_triplet_is_preserved;
          Alcotest.test_case "uri allowed ascii literals preserved" `Quick
            test_uri_allowed_ascii_literals_are_preserved;
          Alcotest.test_case "invalid utf8 literal byte rejected" `Quick
            test_invalid_utf8_literal_byte_is_rejected;
          Alcotest.test_case "non-ucschar unicode literal rejected" `Quick
            test_non_ucschar_unicode_literal_is_rejected;
          Alcotest.test_case "non-literal unicode scalar rejected" `Quick
            test_non_literal_unicode_scalar_is_rejected;
          Alcotest.test_case "surrogate utf8 literal rejected" `Quick
            test_surrogate_utf8_literal_is_rejected;
          Alcotest.test_case "above unicode range literal rejected" `Quick
            test_above_unicode_range_literal_is_rejected;
        ] );
    ]
