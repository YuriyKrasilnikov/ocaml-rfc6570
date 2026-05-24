let pos offset column = { Rfc6570.offset; line = 1; column }

let check_position label expected actual =
  Alcotest.(check int)
    (label ^ " offset") expected.Rfc6570.offset actual.Rfc6570.offset;
  Alcotest.(check int)
    (label ^ " line") expected.Rfc6570.line actual.Rfc6570.line;
  Alcotest.(check int)
    (label ^ " column") expected.Rfc6570.column actual.Rfc6570.column

let bindings pairs name =
  match List.assoc_opt name pairs with
  | Some value -> value
  | None -> Rfc6570.Undefined

let check_output label expected diagnostic =
  Alcotest.(check string) (label ^ " output") expected diagnostic.Rfc6570.output

let one_error label diagnostic =
  match diagnostic.Rfc6570.errors with
  | [ error ] -> error
  | errors ->
      Alcotest.failf "%s expected one error, got %d" label (List.length errors)

let no_errors label diagnostic =
  match diagnostic.Rfc6570.errors with
  | [] -> ()
  | errors ->
      Alcotest.failf "%s expected no errors, got %d" label (List.length errors)

let check_one label raw pairs expected_output check_error =
  let diagnostic = Rfc6570.expand_diagnostic raw (bindings pairs) in
  check_output label expected_output diagnostic;
  check_error (one_error label diagnostic)

let test_success () =
  let diagnostic =
    Rfc6570.expand_diagnostic "A{var}B"
      (bindings [ ("var", Rfc6570.String "x") ])
  in
  check_output "success" "AxB" diagnostic;
  no_errors "success" diagnostic

let test_invalid_literal_stops_after_expanded_prefix () =
  check_one "literal stop" "A{var} bad"
    [ ("var", Rfc6570.String "x") ]
    "Ax bad"
    (function
      | Rfc6570.Invalid_literal { position; character } ->
          check_position "literal stop" (pos 6 7) position;
          Alcotest.(check int)
            "literal stop character" 0x20 (Uchar.to_int character)
      | _ -> Alcotest.fail "expected Invalid_literal")

let test_invalid_literal_before_expression () =
  check_one "literal before" "bad {var}"
    [ ("var", Rfc6570.String "x") ]
    "bad {var}"
    (function
      | Rfc6570.Invalid_literal { position; character } ->
          check_position "literal before" (pos 3 4) position;
          Alcotest.(check int)
            "literal before character" 0x20 (Uchar.to_int character)
      | _ -> Alcotest.fail "expected Invalid_literal")

let test_literal_percent_triplet_is_preserved () =
  let diagnostic =
    Rfc6570.expand_diagnostic "A%20{var}"
      (bindings [ ("var", Rfc6570.String "x") ])
  in
  check_output "literal pct" "A%20x" diagnostic;
  no_errors "literal pct" diagnostic

let test_invalid_literal_percent_stops () =
  check_one "literal bad pct" "A{var}%zzB"
    [ ("var", Rfc6570.String "x") ]
    "Ax%zzB"
    (function
      | Rfc6570.Invalid_percent_triplet { position; raw } ->
          check_position "literal bad pct" (pos 6 7) position;
          Alcotest.(check string) "literal bad pct raw" "%zz" raw
      | _ -> Alcotest.fail "expected Invalid_percent_triplet")

let test_unicode_literal_is_pct_encoded () =
  let diagnostic =
    Rfc6570.expand_diagnostic ("\194\160" ^ "{var}")
      (bindings [ ("var", Rfc6570.String "x") ])
  in
  check_output "unicode literal" "%C2%A0x" diagnostic;
  no_errors "unicode literal" diagnostic

let test_non_literal_unicode_scalar_stops () =
  check_one "non literal unicode" ("\239\191\191" ^ "{var}")
    [ ("var", Rfc6570.String "x") ]
    ("\239\191\191" ^ "{var}")
    (function
      | Rfc6570.Invalid_literal { position; character } ->
          check_position "non literal unicode" (pos 0 1) position;
          Alcotest.(check int)
            "non literal unicode character" 0xFFFF (Uchar.to_int character)
      | _ -> Alcotest.fail "expected Invalid_literal")

let test_invalid_utf8_literal_stops () =
  check_one "invalid utf8 literal" ("\255" ^ "{var}")
    [ ("var", Rfc6570.String "x") ]
    ("\255" ^ "{var}")
    (function
      | Rfc6570.Invalid_literal { position; character } ->
          check_position "invalid utf8 literal" (pos 0 1) position;
          Alcotest.(check int)
            "invalid utf8 literal character" 0xFF (Uchar.to_int character)
      | _ -> Alcotest.fail "expected Invalid_literal")

let test_future_operator_preserves_expression () =
  check_one "future operator" "A{=var}B{var}"
    [ ("var", Rfc6570.String "x") ]
    "A{=var}Bx"
    (function
      | Rfc6570.Reserved_operator { position; operator } ->
          check_position "future operator" (pos 2 3) position;
          Alcotest.(check char) "future operator" '=' operator
      | _ -> Alcotest.fail "expected Reserved_operator")

let test_invalid_varname_preserves_expression () =
  check_one "invalid varname" "A{-bad}B{var}"
    [ ("var", Rfc6570.String "x") ]
    "A{-bad}Bx"
    (function
      | Rfc6570.Invalid_varname { position; raw } ->
          check_position "invalid varname" (pos 2 3) position;
          Alcotest.(check string) "invalid varname raw" "-bad" raw
      | _ -> Alcotest.fail "expected Invalid_varname")

let test_invalid_prefix_preserves_expression () =
  check_one "invalid prefix" "A{var:0}B{var}"
    [ ("var", Rfc6570.String "x") ]
    "A{var:0}Bx"
    (function
      | Rfc6570.Invalid_prefix_modifier { position; raw } ->
          check_position "invalid prefix" (pos 6 7) position;
          Alcotest.(check string) "invalid prefix raw" "0" raw
      | _ -> Alcotest.fail "expected Invalid_prefix_modifier")

let test_nested_expression_preserves_expression () =
  check_one "nested" "A{a{b}C{var}"
    [ ("var", Rfc6570.String "x") ]
    "A{a{b}Cx"
    (function
      | Rfc6570.Unmatched_open_brace { position } ->
          check_position "nested" (pos 3 4) position
      | _ -> Alcotest.fail "expected Unmatched_open_brace")

let test_unmatched_open_preserves_remainder () =
  check_one "unmatched open" "A{var"
    [ ("var", Rfc6570.String "x") ]
    "A{var"
    (function
      | Rfc6570.Unmatched_open_brace { position } ->
          check_position "unmatched open" (pos 1 2) position
      | _ -> Alcotest.fail "expected Unmatched_open_brace")

let test_unmatched_close_stops () =
  check_one "unmatched close" "A}B{var}"
    [ ("var", Rfc6570.String "x") ]
    "A}B{var}"
    (function
      | Rfc6570.Unmatched_close_brace { position } ->
          check_position "unmatched close" (pos 1 2) position
      | _ -> Alcotest.fail "expected Unmatched_close_brace")

let test_multiple_expression_errors_keep_order () =
  let diagnostic =
    Rfc6570.expand_diagnostic "A{=bad}B{-bad}C{var}"
      (bindings [ ("var", Rfc6570.String "x") ])
  in
  check_output "multi" "A{=bad}B{-bad}Cx" diagnostic;
  match diagnostic.Rfc6570.errors with
  | [
   Rfc6570.Reserved_operator { position = reserved_position; operator = '=' };
   Rfc6570.Invalid_varname { position = varname_position; raw = "-bad" };
  ] ->
      check_position "multi reserved" (pos 2 3) reserved_position;
      check_position "multi varname" (pos 9 10) varname_position
  | _ -> Alcotest.fail "expected Reserved_operator then Invalid_varname"

let test_prefix_on_composite_is_recoverable () =
  check_one "prefix composite" "A{list:1}B{var}"
    [
      ("list", Rfc6570.List [ Rfc6570.Item_string "x" ]);
      ("var", Rfc6570.String "y");
    ]
    "A{list:1}By"
    (function
      | Rfc6570.Prefix_on_composite { position; variable } ->
          Alcotest.(check string) "prefix composite variable" "list" variable;
          check_position "prefix composite" (pos 7 8) position
      | _ -> Alcotest.fail "expected Prefix_on_composite")

let test_lazy_lookup_stability () =
  let calls = ref 0 in
  let bindings = function
    | "var" ->
        incr calls;
        Rfc6570.String ("v" ^ string_of_int !calls)
    | _ -> Rfc6570.Undefined
  in
  let diagnostic = Rfc6570.expand_diagnostic "{var}{bad expr}{var}" bindings in
  check_output "lazy" "v1{bad expr}v1" diagnostic;
  Alcotest.(check int) "lazy binding calls" 1 !calls;
  match diagnostic.Rfc6570.errors with
  | [ Rfc6570.Invalid_varname { position; raw = "bad expr" } ] ->
      check_position "lazy error" (pos 6 7) position
  | _ -> Alcotest.fail "expected one Invalid_varname"

let () =
  Alcotest.run "rfc6570-diagnostic-expansion"
    [
      ( "diagnostic",
        [
          Alcotest.test_case "success" `Quick test_success;
          Alcotest.test_case "literal stop" `Quick
            test_invalid_literal_stops_after_expanded_prefix;
          Alcotest.test_case "literal before expression" `Quick
            test_invalid_literal_before_expression;
          Alcotest.test_case "literal percent triplet" `Quick
            test_literal_percent_triplet_is_preserved;
          Alcotest.test_case "invalid literal percent" `Quick
            test_invalid_literal_percent_stops;
          Alcotest.test_case "unicode literal" `Quick
            test_unicode_literal_is_pct_encoded;
          Alcotest.test_case "non literal unicode scalar" `Quick
            test_non_literal_unicode_scalar_stops;
          Alcotest.test_case "invalid utf8 literal" `Quick
            test_invalid_utf8_literal_stops;
          Alcotest.test_case "future operator" `Quick
            test_future_operator_preserves_expression;
          Alcotest.test_case "invalid varname" `Quick
            test_invalid_varname_preserves_expression;
          Alcotest.test_case "invalid prefix" `Quick
            test_invalid_prefix_preserves_expression;
          Alcotest.test_case "nested expression" `Quick
            test_nested_expression_preserves_expression;
          Alcotest.test_case "unmatched open" `Quick
            test_unmatched_open_preserves_remainder;
          Alcotest.test_case "unmatched close" `Quick test_unmatched_close_stops;
          Alcotest.test_case "multiple errors" `Quick
            test_multiple_expression_errors_keep_order;
          Alcotest.test_case "prefix on composite" `Quick
            test_prefix_on_composite_is_recoverable;
          Alcotest.test_case "lazy lookup stability" `Quick
            test_lazy_lookup_stability;
        ] );
    ]
