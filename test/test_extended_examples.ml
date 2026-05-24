let suite_path = "uritemplate-test/extended-tests.json"
let test_extended_examples () = Test_support.run_positive_suite suite_path

let () =
  Alcotest.run "rfc6570-extended-examples"
    [
      ( "official suite",
        [ Alcotest.test_case "extended examples" `Quick test_extended_examples ]
      );
    ]
