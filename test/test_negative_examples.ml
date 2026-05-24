let suite_path = "uritemplate-test/negative-tests.json"
let test_negative_examples () = Test_support.run_negative_suite suite_path

let () =
  Alcotest.run "rfc6570-negative-examples"
    [
      ( "official suite",
        [ Alcotest.test_case "negative examples" `Quick test_negative_examples ]
      );
    ]
