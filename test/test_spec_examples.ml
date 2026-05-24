let suite_path = "uritemplate-test/spec-examples.json"
let test_spec_examples () = Test_support.run_positive_suite suite_path

let () =
  Alcotest.run "rfc6570-spec-examples"
    [
      ( "official suite",
        [ Alcotest.test_case "spec examples" `Quick test_spec_examples ] );
    ]
