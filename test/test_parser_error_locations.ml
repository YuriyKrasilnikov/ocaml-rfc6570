let one_parse_error raw =
  match Rfc6570.parse raw with
  | Ok _ -> Alcotest.failf "parse %S unexpectedly succeeded" raw
  | Error [ error ] -> error
  | Error errors ->
      Alcotest.failf "parse %S returned %d errors" raw (List.length errors)

let pos ?(line = 1) offset column = { Rfc6570.offset; line; column }

let check_position label expected actual =
  Alcotest.(check int)
    (label ^ " offset") expected.Rfc6570.offset actual.Rfc6570.offset;
  Alcotest.(check int)
    (label ^ " line") expected.Rfc6570.line actual.Rfc6570.line;
  Alcotest.(check int)
    (label ^ " column") expected.Rfc6570.column actual.Rfc6570.column

let check_reserved_operator raw operator expected =
  match one_parse_error raw with
  | Rfc6570.Reserved_operator { position; operator = actual } ->
      Alcotest.(check char) raw operator actual;
      check_position raw expected position
  | _ -> Alcotest.failf "expected Reserved_operator for %S" raw

let test_future_operator_locations () =
  List.iter
    (fun (operator, raw, expected) ->
      check_reserved_operator raw operator expected)
    [
      ('=', "{=var}", pos 1 2);
      (',', "{,var}", pos 1 2);
      ('!', "{!var}", pos 1 2);
      ('@', "{@var}", pos 1 2);
      ('|', "{|var}", pos 1 2);
      ('=', "x{=var}", pos 2 3);
    ]

let check_unmatched_open raw expected =
  match one_parse_error raw with
  | Rfc6570.Unmatched_open_brace { position } ->
      check_position raw expected position
  | _ -> Alcotest.failf "expected Unmatched_open_brace for %S" raw

let check_unmatched_close raw expected =
  match one_parse_error raw with
  | Rfc6570.Unmatched_close_brace { position } ->
      check_position raw expected position
  | _ -> Alcotest.failf "expected Unmatched_close_brace for %S" raw

let check_empty_expression raw expected =
  match one_parse_error raw with
  | Rfc6570.Empty_expression { position } ->
      check_position raw expected position
  | _ -> Alcotest.failf "expected Empty_expression for %S" raw

let test_brace_locations () =
  check_unmatched_open "a{var" (pos 1 2);
  check_unmatched_open "{a{b}" (pos 2 3);
  check_unmatched_close "a}b" (pos 1 2);
  check_unmatched_close "}" (pos 0 1);
  check_unmatched_close "ok\n}" (pos ~line:2 3 1);
  check_unmatched_close "{var}}" (pos 5 6);
  check_unmatched_close "{var}\n}" (pos ~line:2 6 1);
  check_empty_expression "{}" (pos 1 2);
  check_empty_expression "{+}" (pos 1 2)

let check_invalid_varname raw expected_raw expected =
  match one_parse_error raw with
  | Rfc6570.Invalid_varname { position; raw = actual_raw } ->
      Alcotest.(check string) (raw ^ " raw") expected_raw actual_raw;
      check_position raw expected position
  | _ -> Alcotest.failf "expected Invalid_varname for %S" raw

let check_invalid_percent raw expected_raw expected =
  match one_parse_error raw with
  | Rfc6570.Invalid_percent_triplet { position; raw = actual_raw } ->
      Alcotest.(check string) (raw ^ " raw") expected_raw actual_raw;
      check_position raw expected position
  | _ -> Alcotest.failf "expected Invalid_percent_triplet for %S" raw

let check_invalid_prefix raw expected_raw expected =
  match one_parse_error raw with
  | Rfc6570.Invalid_prefix_modifier { position; raw = actual_raw } ->
      Alcotest.(check string) (raw ^ " raw") expected_raw actual_raw;
      check_position raw expected position
  | _ -> Alcotest.failf "expected Invalid_prefix_modifier for %S" raw

let test_expression_error_locations () =
  check_invalid_varname "{-bad}" "-bad" (pos 1 2);
  check_invalid_varname "{var,-bad}" "-bad" (pos 5 6);
  check_invalid_percent "a%GZ{var}" "%GZ" (pos 1 2);
  check_invalid_percent "{%GZ}" "%GZ" (pos 1 2);
  check_invalid_percent "{var,%GZ}" "%GZ" (pos 5 6);
  check_invalid_prefix "{var:}" "" (pos 5 6);
  check_invalid_prefix "{var:0}" "0" (pos 5 6);
  check_invalid_prefix "{var:00000}" "00000" (pos 5 6);
  check_invalid_prefix "{var:a}" "a" (pos 5 6)

let () =
  Alcotest.run "rfc6570-parser-error-locations"
    [
      ( "locations",
        [
          Alcotest.test_case "future operators" `Quick
            test_future_operator_locations;
          Alcotest.test_case "braces" `Quick test_brace_locations;
          Alcotest.test_case "expression errors" `Quick
            test_expression_error_locations;
        ] );
    ]
