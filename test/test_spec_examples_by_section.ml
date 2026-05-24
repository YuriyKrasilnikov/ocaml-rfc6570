let suite_path = "uritemplate-test/spec-examples-by-section.json"

let test_spec_examples_by_section () =
  Test_support.run_positive_suite suite_path

let () =
  Alcotest.run "rfc6570-spec-examples-by-section"
    [
      ( "official suite",
        [
          Alcotest.test_case "spec examples by section" `Quick
            test_spec_examples_by_section;
        ] );
    ]
