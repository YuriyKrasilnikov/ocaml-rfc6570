let parse_ok raw =
  match Rfc6570.parse raw with
  | Ok template -> template
  | Error errors ->
      Alcotest.failf "parse %S failed with %d error(s)" raw (List.length errors)

let bind name =
  match name with "segment" -> Rfc6570.String "value" | _ -> Rfc6570.Undefined

let test_expansion_does_not_parse_uri_reference () =
  let template = parse_ok "http://example.com:bad/{segment}" in
  match Rfc6570.expand template bind with
  | Error errors ->
      Alcotest.failf "expand failed with %d error(s)" (List.length errors)
  | Ok actual ->
      Alcotest.(check string)
        "expanded URI reference string" "http://example.com:bad/value" actual

let test_reverse_matching_is_out_of_scope () =
  let template = parse_ok "items/{id}" in
  Alcotest.(check (list string))
    "variables only" [ "id" ]
    (Rfc6570.variables template)

let () =
  Alcotest.run "rfc6570-public-api"
    [
      ( "scope",
        [
          Alcotest.test_case "expansion does not parse URI reference" `Quick
            test_expansion_does_not_parse_uri_reference;
          Alcotest.test_case "reverse matching is out of scope" `Quick
            test_reverse_matching_is_out_of_scope;
        ] );
    ]
