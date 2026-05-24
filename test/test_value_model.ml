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

let bind pairs name =
  match List.assoc_opt name pairs with
  | Some value -> value
  | None -> Rfc6570.Undefined

let test_missing_and_explicit_undefined_are_ignored () =
  let bindings =
    bind [ ("var", Rfc6570.String "value"); ("u", Rfc6570.Undefined) ]
  in
  Alcotest.(check string)
    "missing omitted" "value"
    (expand "{missing,var}" bindings);
  Alcotest.(check string)
    "undefined omitted" "value"
    (expand "{u,var}" bindings);
  Alcotest.(check string)
    "all undefined expression omitted" ""
    (expand "{missing,u}" bindings)

let test_empty_string_is_defined () =
  let bindings = bind [ ("empty", Rfc6570.String "") ] in
  Alcotest.(check string) "query empty" "?empty=" (expand "{?empty}" bindings);
  Alcotest.(check string)
    "parameter empty" ";empty"
    (expand "{;empty}" bindings);
  Alcotest.(check string) "fragment empty" "#" (expand "{#empty}" bindings);
  Alcotest.(check string) "label empty" "." (expand "{.empty}" bindings)

let test_empty_composites_are_undefined () =
  let bindings =
    bind
      [
        ("list", Rfc6570.List []);
        ("assoc", Rfc6570.Assoc []);
        ("var", Rfc6570.String "value");
      ]
  in
  Alcotest.(check string)
    "empty composites omitted" "value"
    (expand "{list}{assoc}{var}" bindings)

let test_undefined_members_are_ignored () =
  let bindings =
    bind
      [
        ( "list",
          Rfc6570.List
            [
              Rfc6570.Item_undefined;
              Rfc6570.Item_string "red";
              Rfc6570.Item_undefined;
              Rfc6570.Item_string "blue";
            ] );
        ( "keys",
          Rfc6570.Assoc
            [ ("a", Rfc6570.Item_undefined); ("b", Rfc6570.Item_string "2") ] );
      ]
  in
  Alcotest.(check string) "list members" "red,blue" (expand "{list}" bindings);
  Alcotest.(check string) "assoc members" "b,2" (expand "{keys}" bindings);
  Alcotest.(check string)
    "assoc exploded query" "?b=2"
    (expand "{?keys*}" bindings)

let test_repeated_variable_lookup_is_stable_per_expansion () =
  let calls = ref 0 in
  let bindings = function
    | "var" ->
        incr calls;
        Rfc6570.String (Printf.sprintf "v%d" !calls)
    | _ -> Rfc6570.Undefined
  in
  Alcotest.(check string)
    "stable repeated value" "v1/v1"
    (expand "{var}/{var}" bindings);
  Alcotest.(check int) "one lookup" 1 !calls

let () =
  Alcotest.run "rfc6570-value-model"
    [
      ( "values",
        [
          Alcotest.test_case "missing and undefined" `Quick
            test_missing_and_explicit_undefined_are_ignored;
          Alcotest.test_case "empty string" `Quick test_empty_string_is_defined;
          Alcotest.test_case "empty composites" `Quick
            test_empty_composites_are_undefined;
          Alcotest.test_case "undefined members" `Quick
            test_undefined_members_are_ignored;
          Alcotest.test_case "stable repeated lookup" `Quick
            test_repeated_variable_lookup_is_stable_per_expansion;
        ] );
    ]
