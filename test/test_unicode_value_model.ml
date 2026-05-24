let pos offset column = { Rfc6570.offset; line = 1; column }

let check_position label expected actual =
  Alcotest.(check int)
    (label ^ " offset") expected.Rfc6570.offset actual.Rfc6570.offset;
  Alcotest.(check int)
    (label ^ " line") expected.Rfc6570.line actual.Rfc6570.line;
  Alcotest.(check int)
    (label ^ " column") expected.Rfc6570.column actual.Rfc6570.column

let parse raw =
  match Rfc6570.parse raw with
  | Ok template -> template
  | Error errors ->
      Alcotest.failf "parse %S failed with %d error(s)" raw (List.length errors)

let expand raw bindings = Rfc6570.expand (parse raw) bindings

let expect_invalid_utf8 label raw bindings expected_variable expected_component
    expected_position expected_offset =
  match expand raw bindings with
  | Error
      [ Rfc6570.Invalid_value_utf8 { position; variable; component; offset } ]
    ->
      Alcotest.(check string) (label ^ " variable") expected_variable variable;
      Alcotest.(check int) (label ^ " offset") expected_offset offset;
      Alcotest.(check bool)
        (label ^ " component") true
        (expected_component = component);
      check_position label expected_position position
  | Ok value -> Alcotest.failf "%s unexpectedly expanded to %S" label value
  | Error errors ->
      Alcotest.failf "%s expected one Invalid_value_utf8, got %d error(s)" label
        (List.length errors)

let expect_expands label raw bindings expected =
  match expand raw bindings with
  | Ok actual -> Alcotest.(check string) label expected actual
  | Error errors ->
      Alcotest.failf "%s expected success, got %d error(s)" label
        (List.length errors)

let test_scalar_invalid_utf8 () =
  expect_invalid_utf8 "scalar continuation" "{var}"
    (function "var" -> Rfc6570.String "\128x" | _ -> Rfc6570.Undefined)
    "var" Rfc6570.Scalar_value (pos 1 2) 0;
  expect_invalid_utf8 "scalar later byte" "{var}"
    (function "var" -> Rfc6570.String "ok\128" | _ -> Rfc6570.Undefined)
    "var" Rfc6570.Scalar_value (pos 1 2) 2

let test_scalar_invalid_utf8_forms () =
  List.iter
    (fun (label, value) ->
      expect_invalid_utf8 label "{var}"
        (function "var" -> Rfc6570.String value | _ -> Rfc6570.Undefined)
        "var" Rfc6570.Scalar_value (pos 1 2) 0)
    [
      ("continuation", "\128");
      ("overlong", "\192\128");
      ("truncated", "\226\130");
      ("surrogate", "\237\160\128");
      ("above max", "\244\144\128\128");
    ]

let test_list_item_invalid_utf8 () =
  expect_invalid_utf8 "list item" "{list}"
    (function
      | "list" ->
          Rfc6570.List
            [
              Rfc6570.Item_undefined;
              Rfc6570.Item_string "ok";
              Rfc6570.Item_string "\128bad";
            ]
      | _ -> Rfc6570.Undefined)
    "list" (Rfc6570.List_item 2) (pos 1 2) 0

let test_assoc_key_invalid_utf8 () =
  expect_invalid_utf8 "assoc key" "{keys}"
    (function
      | "keys" -> Rfc6570.Assoc [ ("\128key", Rfc6570.Item_string "value") ]
      | _ -> Rfc6570.Undefined)
    "keys" (Rfc6570.Assoc_key 0) (pos 1 2) 0

let test_assoc_value_invalid_utf8 () =
  expect_invalid_utf8 "assoc value" "{keys}"
    (function
      | "keys" -> Rfc6570.Assoc [ ("key", Rfc6570.Item_string "v\128") ]
      | _ -> Rfc6570.Undefined)
    "keys" (Rfc6570.Assoc_value 0) (pos 1 2) 1

let test_prefix_rejects_invalid_utf8_before_slicing () =
  expect_invalid_utf8 "prefix invalid" "{var:1}"
    (function "var" -> Rfc6570.String "\128x" | _ -> Rfc6570.Undefined)
    "var" Rfc6570.Scalar_value (pos 1 2) 0

let test_valid_utf8_boundaries_still_expand () =
  List.iter
    (fun (label, value, expected) ->
      expect_expands label "{var}"
        (function "var" -> Rfc6570.String value | _ -> Rfc6570.Undefined)
        expected)
    [
      ("nul", "\000", "%00");
      ("two byte", "\194\128", "%C2%80");
      ("three byte", "\224\160\128", "%E0%A0%80");
      ("four byte", "\240\144\128\128", "%F0%90%80%80");
    ]

let test_undefined_members_are_not_validated () =
  expect_expands "undefined list" "{list}"
    (function
      | "list" -> Rfc6570.List [ Rfc6570.Item_undefined ]
      | _ -> Rfc6570.Undefined)
    "";
  expect_expands "undefined assoc" "{keys}"
    (function
      | "keys" -> Rfc6570.Assoc [ ("\128", Rfc6570.Item_undefined) ]
      | _ -> Rfc6570.Undefined)
    ""

let test_diagnostic_invalid_value_preserves_expression () =
  let diagnostic =
    Rfc6570.expand_diagnostic "A{var}B{ok}" (function
      | "var" -> Rfc6570.String "\128x"
      | "ok" -> Rfc6570.String "y"
      | _ -> Rfc6570.Undefined)
  in
  Alcotest.(check string) "diagnostic output" "A{var}By" diagnostic.output;
  match diagnostic.errors with
  | [ Rfc6570.Invalid_value_utf8 { position; variable; component; offset } ] ->
      Alcotest.(check string) "diagnostic variable" "var" variable;
      Alcotest.(check int) "diagnostic offset" 0 offset;
      Alcotest.(check bool)
        "diagnostic component" true
        (component = Rfc6570.Scalar_value);
      check_position "diagnostic" (pos 2 3) position
  | errors ->
      Alcotest.failf "diagnostic expected one Invalid_value_utf8, got %d"
        (List.length errors)

let () =
  Alcotest.run "rfc6570-unicode-value-model"
    [
      ( "unicode values",
        [
          Alcotest.test_case "scalar invalid utf8" `Quick
            test_scalar_invalid_utf8;
          Alcotest.test_case "scalar invalid utf8 forms" `Quick
            test_scalar_invalid_utf8_forms;
          Alcotest.test_case "list item invalid utf8" `Quick
            test_list_item_invalid_utf8;
          Alcotest.test_case "assoc key invalid utf8" `Quick
            test_assoc_key_invalid_utf8;
          Alcotest.test_case "assoc value invalid utf8" `Quick
            test_assoc_value_invalid_utf8;
          Alcotest.test_case "prefix rejects invalid utf8" `Quick
            test_prefix_rejects_invalid_utf8_before_slicing;
          Alcotest.test_case "valid utf8 boundaries" `Quick
            test_valid_utf8_boundaries_still_expand;
          Alcotest.test_case "undefined members are not validated" `Quick
            test_undefined_members_are_not_validated;
          Alcotest.test_case "diagnostic invalid value" `Quick
            test_diagnostic_invalid_value_preserves_expression;
        ] );
    ]
