let parse_ok raw =
  match Rfc6570.parse raw with
  | Ok template -> template
  | Error errors ->
      Alcotest.failf "parse %S failed with %d error(s)" raw (List.length errors)

let parse_fails raw =
  match Rfc6570.parse raw with Ok _ -> false | Error _ -> true

let test_literal_single_quote_erratum () =
  let template = parse_ok "author's/{var}" in
  Alcotest.(check string)
    "preserved template" "author's/{var}"
    (Rfc6570.to_string template)

let test_future_reserved_operator_rejected () =
  List.iter
    (fun operator ->
      Alcotest.(check bool)
        (Printf.sprintf "future operator %c rejected" operator)
        true
        (parse_fails (Printf.sprintf "{%cvar}" operator)))
    [ '='; ','; '!'; '@'; '|' ]

let test_literal_percent_triplet_identity () =
  let template = parse_ok "before%20after/{var}" in
  Alcotest.(check string)
    "preserved literal pct triplet" "before%20after/{var}"
    (Rfc6570.to_string template)

let test_non_ascii_literal_preserved () =
  let template = parse_ok "café/{var}" in
  Alcotest.(check string)
    "preserved non-ascii literal" "café/{var}"
    (Rfc6570.to_string template)

let test_varname_percent_triplet_identity () =
  let template = parse_ok "{%24id}" in
  Alcotest.(check (list string))
    "variables" [ "%24id" ]
    (Rfc6570.variables template)

let test_invalid_dollar_varname_rejected () =
  Alcotest.(check bool) "invalid varname rejected" true (parse_fails "{$id}")

let () =
  Alcotest.run "rfc6570-parser-abnf"
    [
      ( "syntax",
        [
          Alcotest.test_case "literal single quote erratum" `Quick
            test_literal_single_quote_erratum;
          Alcotest.test_case "future reserved operator rejected" `Quick
            test_future_reserved_operator_rejected;
          Alcotest.test_case "literal percent triplet identity" `Quick
            test_literal_percent_triplet_identity;
          Alcotest.test_case "non-ascii literal preserved" `Quick
            test_non_ascii_literal_preserved;
          Alcotest.test_case "varname percent triplet identity" `Quick
            test_varname_percent_triplet_identity;
          Alcotest.test_case "invalid dollar varname rejected" `Quick
            test_invalid_dollar_varname_rejected;
        ] );
    ]
