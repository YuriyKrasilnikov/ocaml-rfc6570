let parse raw =
  match Rfc6570.parse raw with
  | Ok template -> template
  | Error errors ->
      Alcotest.failf "parse %S failed with %d error(s)" raw (List.length errors)

let bindings = function
  | "var" -> Rfc6570.String "value"
  | "empty" -> Rfc6570.String ""
  | "path" -> Rfc6570.String "/foo/bar"
  | "list" ->
      Rfc6570.List
        [
          Rfc6570.Item_string "red";
          Rfc6570.Item_string "green";
          Rfc6570.Item_string "blue";
        ]
  | "keys" ->
      Rfc6570.Assoc
        [
          ("semi", Rfc6570.Item_string ";");
          ("dot", Rfc6570.Item_string ".");
          ("comma", Rfc6570.Item_string ",");
        ]
  | _ -> Rfc6570.Undefined

let expand raw =
  match Rfc6570.expand (parse raw) bindings with
  | Ok value -> value
  | Error errors ->
      Alcotest.failf "expand %S failed with %d error(s)" raw
        (List.length errors)

let check raw expected = Alcotest.(check string) raw expected (expand raw)

let test_scalar_operators () =
  check "{var}" "value";
  check "{+path}" "/foo/bar";
  check "{#path}" "#/foo/bar";
  check "{.var,empty}" ".value.";
  check "{/var,empty}" "/value/";
  check "{;var,empty}" ";var=value;empty";
  check "{?var,empty}" "?var=value&empty=";
  check "{&var,empty}" "&var=value&empty=";
  check "{?undef}" "";
  check "{#undef}" ""

let test_list_expansion () =
  check "{list}" "red,green,blue";
  check "{/list*}" "/red/green/blue";
  check "{?list*}" "?list=red&list=green&list=blue"

let test_assoc_expansion () =
  check "{keys}" "semi,%3B,dot,.,comma,%2C";
  check "{+keys}" "semi,;,dot,.,comma,,";
  check "{?keys*}" "?semi=%3B&dot=.&comma=%2C";
  check "{;keys*}" ";semi=%3B;dot=.;comma=%2C"

let () =
  Alcotest.run "rfc6570-expansion-matrix"
    [
      ( "expansion",
        [
          Alcotest.test_case "scalar operators" `Quick test_scalar_operators;
          Alcotest.test_case "list expansion" `Quick test_list_expansion;
          Alcotest.test_case "assoc expansion" `Quick test_assoc_expansion;
        ] );
    ]
