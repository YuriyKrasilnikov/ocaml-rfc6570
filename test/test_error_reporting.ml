let one_parse_error raw =
  match Rfc6570.parse raw with
  | Ok _ -> Alcotest.failf "parse %S unexpectedly succeeded" raw
  | Error [ error ] -> error
  | Error errors ->
      Alcotest.failf "parse %S returned %d errors" raw (List.length errors)

let check_position label expected actual =
  Alcotest.(check int)
    (label ^ " offset") expected.Rfc6570.offset actual.Rfc6570.offset;
  Alcotest.(check int)
    (label ^ " line") expected.Rfc6570.line actual.Rfc6570.line;
  Alcotest.(check int)
    (label ^ " column") expected.Rfc6570.column actual.Rfc6570.column

let pos ?(line = 1) offset column = { Rfc6570.offset; line; column }

let test_unmatched_open_brace () =
  match one_parse_error "a{var" with
  | Rfc6570.Unmatched_open_brace { position } ->
      check_position "open" (pos 1 2) position
  | _ -> Alcotest.fail "expected Unmatched_open_brace"

let test_unmatched_close_brace () =
  match one_parse_error "a}b" with
  | Rfc6570.Unmatched_close_brace { position } ->
      check_position "close" (pos 1 2) position
  | _ -> Alcotest.fail "expected Unmatched_close_brace"

let test_unmatched_close_brace_at_start () =
  match one_parse_error "}" with
  | Rfc6570.Unmatched_close_brace { position } ->
      check_position "close at start" (pos 0 1) position
  | _ -> Alcotest.fail "expected Unmatched_close_brace"

let test_multiline_position () =
  match one_parse_error "ok\n}" with
  | Rfc6570.Unmatched_close_brace { position } ->
      check_position "multiline"
        { Rfc6570.offset = 3; line = 2; column = 1 }
        position
  | _ -> Alcotest.fail "expected Unmatched_close_brace"

let test_empty_expression () =
  match one_parse_error "{}" with
  | Rfc6570.Empty_expression { position } ->
      check_position "empty" (pos 1 2) position
  | _ -> Alcotest.fail "expected Empty_expression"

let test_reserved_operator () =
  match one_parse_error "{=var}" with
  | Rfc6570.Reserved_operator { position; operator } ->
      check_position "reserved" (pos 1 2) position;
      Alcotest.(check char) "operator" '=' operator
  | _ -> Alcotest.fail "expected Reserved_operator"

let test_invalid_literal () =
  match one_parse_error "bad literal" with
  | Rfc6570.Invalid_literal { position; character } ->
      check_position "literal" (pos 3 4) position;
      Alcotest.(check int) "character" 0x20 (Uchar.to_int character)
  | _ -> Alcotest.fail "expected Invalid_literal"

let test_invalid_printable_literal () =
  match one_parse_error "bad\"literal" with
  | Rfc6570.Invalid_literal { position; character } ->
      check_position "printable literal" (pos 3 4) position;
      Alcotest.(check int) "character" 0x22 (Uchar.to_int character)
  | _ -> Alcotest.fail "expected Invalid_literal"

let test_invalid_varname () =
  match one_parse_error "{bad-name}" with
  | Rfc6570.Invalid_varname { raw; _ } ->
      Alcotest.(check string) "raw" "bad-name" raw
  | _ -> Alcotest.fail "expected Invalid_varname"

let test_invalid_varname_shapes () =
  List.iter
    (fun (template, expected_raw) ->
      match one_parse_error template with
      | Rfc6570.Invalid_varname { raw; _ } ->
          Alcotest.(check string) template expected_raw raw
      | _ -> Alcotest.failf "expected Invalid_varname for %S" template)
    [
      ("{a..b}", "a..b");
      ("{var,}", "");
      ("{var,:2}", "");
      ("{bad-name:2}", "bad-name");
    ]

let test_empty_operator_expression () =
  match one_parse_error "{+}" with
  | Rfc6570.Empty_expression { position } ->
      check_position "operator empty" (pos 1 2) position
  | _ -> Alcotest.fail "expected Empty_expression"

let test_invalid_literal_before_expression () =
  match one_parse_error "bad {var}" with
  | Rfc6570.Invalid_literal { position; character } ->
      check_position "literal before expression" (pos 3 4) position;
      Alcotest.(check int) "character" 0x20 (Uchar.to_int character)
  | _ -> Alcotest.fail "expected Invalid_literal"

let test_invalid_literal_percent_triplet () =
  match one_parse_error "a%GZ" with
  | Rfc6570.Invalid_percent_triplet { position; raw } ->
      check_position "percent" (pos 1 2) position;
      Alcotest.(check string) "raw" "%GZ" raw
  | _ -> Alcotest.fail "expected Invalid_percent_triplet"

let test_invalid_varname_percent_triplet () =
  match one_parse_error "{%GZ}" with
  | Rfc6570.Invalid_percent_triplet { position; raw } ->
      check_position "varname percent" (pos 1 2) position;
      Alcotest.(check string) "raw" "%GZ" raw
  | _ -> Alcotest.fail "expected Invalid_percent_triplet"

let test_invalid_prefix_modifier () =
  List.iter
    (fun (template, expected_raw) ->
      match one_parse_error template with
      | Rfc6570.Invalid_prefix_modifier { raw; _ } ->
          Alcotest.(check string) template expected_raw raw
      | _ -> Alcotest.failf "expected Invalid_prefix_modifier for %S" template)
    [
      ("{var:}", "");
      ("{var:0}", "0");
      ("{var:00000}", "00000");
      ("{var:a}", "a");
    ]

let parse_template raw =
  match Rfc6570.parse raw with
  | Ok template -> template
  | Error errors ->
      Alcotest.failf "parse %S failed with %d error(s)" raw (List.length errors)

let check_prefix_on_composite raw bindings variable expected_position =
  match Rfc6570.expand (parse_template raw) bindings with
  | Error [ Rfc6570.Prefix_on_composite { position; variable = actual } ] ->
      Alcotest.(check string) (raw ^ " variable") variable actual;
      check_position raw expected_position position
  | Ok value -> Alcotest.failf "unexpected expansion %S" value
  | Error errors -> Alcotest.failf "unexpected %d error(s)" (List.length errors)

let check_expands raw bindings expected =
  match Rfc6570.expand (parse_template raw) bindings with
  | Ok actual -> Alcotest.(check string) raw expected actual
  | Error errors -> Alcotest.failf "unexpected %d error(s)" (List.length errors)

let test_prefix_on_composite () =
  let list_bindings = function
    | "list" -> Rfc6570.List [ Rfc6570.Item_string "red" ]
    | _ -> Rfc6570.Undefined
  in
  let assoc_bindings = function
    | "keys" -> Rfc6570.Assoc [ ("semi", Rfc6570.Item_string ";") ]
    | "var" -> Rfc6570.String "value"
    | _ -> Rfc6570.Undefined
  in
  check_prefix_on_composite "{list:1}" list_bindings "list" (pos 6 7);
  check_prefix_on_composite "{keys:1}" assoc_bindings "keys" (pos 6 7);
  check_prefix_on_composite "{?var,keys:1}" assoc_bindings "keys" (pos 11 12)

let test_prefix_on_undefined_composite () =
  let bindings = function
    | "empty_list" -> Rfc6570.List []
    | "undefined_list" -> Rfc6570.List [ Rfc6570.Item_undefined ]
    | "empty_keys" -> Rfc6570.Assoc []
    | "undefined_keys" -> Rfc6570.Assoc [ ("a", Rfc6570.Item_undefined) ]
    | _ -> Rfc6570.Undefined
  in
  check_expands "{empty_list:1}" bindings "";
  check_expands "{undefined_list:1}" bindings "";
  check_expands "{empty_keys:1}" bindings "";
  check_expands "{undefined_keys:1}" bindings ""

let () =
  Alcotest.run "rfc6570-error-reporting"
    [
      ( "errors",
        [
          Alcotest.test_case "unmatched open brace" `Quick
            test_unmatched_open_brace;
          Alcotest.test_case "unmatched close brace" `Quick
            test_unmatched_close_brace;
          Alcotest.test_case "unmatched close brace at start" `Quick
            test_unmatched_close_brace_at_start;
          Alcotest.test_case "multiline position" `Quick test_multiline_position;
          Alcotest.test_case "empty expression" `Quick test_empty_expression;
          Alcotest.test_case "reserved operator" `Quick test_reserved_operator;
          Alcotest.test_case "invalid literal" `Quick test_invalid_literal;
          Alcotest.test_case "invalid printable literal" `Quick
            test_invalid_printable_literal;
          Alcotest.test_case "invalid varname" `Quick test_invalid_varname;
          Alcotest.test_case "invalid varname shapes" `Quick
            test_invalid_varname_shapes;
          Alcotest.test_case "empty operator expression" `Quick
            test_empty_operator_expression;
          Alcotest.test_case "invalid literal before expression" `Quick
            test_invalid_literal_before_expression;
          Alcotest.test_case "invalid literal percent" `Quick
            test_invalid_literal_percent_triplet;
          Alcotest.test_case "invalid varname percent" `Quick
            test_invalid_varname_percent_triplet;
          Alcotest.test_case "invalid prefix" `Quick
            test_invalid_prefix_modifier;
          Alcotest.test_case "prefix on composite" `Quick
            test_prefix_on_composite;
          Alcotest.test_case "prefix on undefined composite" `Quick
            test_prefix_on_undefined_composite;
        ] );
    ]
