let parse raw =
  match Rfc6570.parse raw with
  | Ok template -> template
  | Error errors ->
      Alcotest.failf "parse %S failed with %d error(s)" raw (List.length errors)

let decomposed_e_acute = "e\204\129"
let composed_e_acute_pct = "%C3%A9"
let decomposed_e_acute_pct = "e%CC%81"

let bindings value name =
  match name with "var" -> Rfc6570.String value | _ -> Rfc6570.Undefined

let expand ?options raw bindings =
  match options with
  | None -> Rfc6570.expand (parse raw) bindings
  | Some options -> Rfc6570.expand_with options (parse raw) bindings

let expect_expands label ?options raw bindings expected =
  match expand ?options raw bindings with
  | Ok actual -> Alcotest.(check string) label expected actual
  | Error errors ->
      Alcotest.failf "%s expected success, got %d error(s)" label
        (List.length errors)

let preserve = { Rfc6570.normalization = Rfc6570.Preserve }
let normalize_nfc = { Rfc6570.normalization = Rfc6570.Normalize_nfc }

let test_default_normalizes_scalar () =
  Alcotest.(check bool)
    "default is NFC" true
    (Rfc6570.default_options.normalization = Rfc6570.Normalize_nfc);
  expect_expands "default scalar" "{var}"
    (bindings decomposed_e_acute)
    composed_e_acute_pct

let test_preserve_scalar () =
  expect_expands "preserve scalar" ~options:preserve "{var}"
    (bindings decomposed_e_acute)
    decomposed_e_acute_pct;
  expect_expands "explicit nfc scalar" ~options:normalize_nfc "{var}"
    (bindings decomposed_e_acute)
    composed_e_acute_pct

let test_list_item_normalization () =
  let bindings = function
    | "list" -> Rfc6570.List [ Rfc6570.Item_string decomposed_e_acute ]
    | _ -> Rfc6570.Undefined
  in
  expect_expands "list default" "{list}" bindings composed_e_acute_pct;
  expect_expands "list preserve" ~options:preserve "{list}" bindings
    decomposed_e_acute_pct

let test_assoc_key_normalization () =
  let bindings = function
    | "keys" -> Rfc6570.Assoc [ (decomposed_e_acute, Rfc6570.Item_string "x") ]
    | _ -> Rfc6570.Undefined
  in
  expect_expands "assoc key default" "{keys*}" bindings
    (composed_e_acute_pct ^ "=x");
  expect_expands "assoc key preserve" ~options:preserve "{keys*}" bindings
    (decomposed_e_acute_pct ^ "=x")

let test_assoc_value_normalization () =
  let bindings = function
    | "keys" -> Rfc6570.Assoc [ ("k", Rfc6570.Item_string decomposed_e_acute) ]
    | _ -> Rfc6570.Undefined
  in
  expect_expands "assoc value default" "{keys*}" bindings
    ("k=" ^ composed_e_acute_pct);
  expect_expands "assoc value preserve" ~options:preserve "{keys*}" bindings
    ("k=" ^ decomposed_e_acute_pct)

let test_prefix_slices_after_normalization () =
  expect_expands "prefix default" "{var:1}"
    (bindings (decomposed_e_acute ^ "x"))
    composed_e_acute_pct;
  expect_expands "prefix preserve" ~options:preserve "{var:1}"
    (bindings (decomposed_e_acute ^ "x"))
    "e"

let test_diagnostic_uses_policy () =
  let diagnostic =
    Rfc6570.expand_diagnostic "A{var}B" (bindings decomposed_e_acute)
  in
  Alcotest.(check string)
    "diagnostic default"
    ("A" ^ composed_e_acute_pct ^ "B")
    diagnostic.output;
  Alcotest.(check int) "diagnostic errors" 0 (List.length diagnostic.errors);
  let diagnostic =
    Rfc6570.expand_diagnostic_with preserve "A{var}B"
      (bindings decomposed_e_acute)
  in
  Alcotest.(check string)
    "diagnostic preserve"
    ("A" ^ decomposed_e_acute_pct ^ "B")
    diagnostic.output;
  Alcotest.(check int)
    "diagnostic preserve errors" 0
    (List.length diagnostic.errors)

let test_invalid_utf8_fails_before_normalization () =
  match expand "{var}" (bindings "\128") with
  | Error [ Rfc6570.Invalid_value_utf8 { variable = "var"; offset = 0; _ } ] ->
      ()
  | Ok value -> Alcotest.failf "unexpected expansion %S" value
  | Error errors ->
      Alcotest.failf "expected one Invalid_value_utf8, got %d"
        (List.length errors)

let () =
  Alcotest.run "rfc6570-nfc-policy"
    [
      ( "nfc policy",
        [
          Alcotest.test_case "default normalizes scalar" `Quick
            test_default_normalizes_scalar;
          Alcotest.test_case "preserve scalar" `Quick test_preserve_scalar;
          Alcotest.test_case "list item" `Quick test_list_item_normalization;
          Alcotest.test_case "assoc key" `Quick test_assoc_key_normalization;
          Alcotest.test_case "assoc value" `Quick test_assoc_value_normalization;
          Alcotest.test_case "prefix after nfc" `Quick
            test_prefix_slices_after_normalization;
          Alcotest.test_case "diagnostic policy" `Quick
            test_diagnostic_uses_policy;
          Alcotest.test_case "invalid before normalization" `Quick
            test_invalid_utf8_fails_before_normalization;
        ] );
    ]
