let parse raw =
  match Rfc6570.parse raw with
  | Ok template -> template
  | Error errors ->
      Alcotest.failf "parse %S failed with %d error(s)" raw (List.length errors)

let expand raw bindings =
  match Rfc6570.expand (parse raw) bindings with
  | Ok value -> value
  | Error errors ->
      Alcotest.failf "expand %S failed with %d error(s)" raw
        (List.length errors)

let test_roundtrip_and_first_seen_variables () =
  let raw = "{x}{y,x}{+%24id,hello.world}" in
  let template = parse raw in
  Alcotest.(check string) "source" raw (Rfc6570.to_string template);
  Alcotest.(check (list string))
    "variables"
    [ "x"; "y"; "%24id"; "hello.world" ]
    (Rfc6570.variables template)

let test_dotted_varname_expands () =
  let bindings = function
    | "hello.world" -> Rfc6570.String "ok"
    | _ -> Rfc6570.Undefined
  in
  Alcotest.(check string) "dotted" "ok" (expand "{hello.world}" bindings)

let test_varnames_are_case_sensitive () =
  let bindings = function
    | "Var" -> Rfc6570.String "upper"
    | "var" -> Rfc6570.String "lower"
    | _ -> Rfc6570.Undefined
  in
  Alcotest.(check string) "case" "upper,lower" (expand "{Var,var}" bindings)

let () =
  Alcotest.run "rfc6570-roundtrip"
    [
      ( "roundtrip",
        [
          Alcotest.test_case "source and variables" `Quick
            test_roundtrip_and_first_seen_variables;
          Alcotest.test_case "dotted varname" `Quick test_dotted_varname_expands;
          Alcotest.test_case "case-sensitive varnames" `Quick
            test_varnames_are_case_sensitive;
        ] );
    ]
