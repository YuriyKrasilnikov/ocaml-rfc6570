let read_file path =
  let ic = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr ic)
    (fun () ->
      let len = in_channel_length ic in
      really_input_string ic len)

let load_json path = Yojson.Safe.from_string (read_file path)

let item_of_json = function
  | `Null -> Rfc6570.Item_undefined
  | `String value -> Rfc6570.Item_string value
  | json -> Rfc6570.Item_string (Yojson.Safe.to_string json)

let value_of_json = function
  | `Null -> Rfc6570.Undefined
  | `String value -> Rfc6570.String value
  | `List values -> Rfc6570.List (List.map item_of_json values)
  | `Assoc values ->
      Rfc6570.Assoc
        (List.map (fun (name, value) -> (name, item_of_json value)) values)
  | json -> Rfc6570.String (Yojson.Safe.to_string json)

let variables_of_json = function
  | `Assoc fields ->
      List.map (fun (name, value) -> (name, value_of_json value)) fields
  | json ->
      Alcotest.failf "variables must be a JSON object, got %s"
        (Yojson.Safe.to_string json)

let binding variables name =
  match List.assoc_opt name variables with
  | Some value -> value
  | None -> Rfc6570.Undefined

let expected_matches expected actual =
  match expected with
  | `String value -> String.equal value actual
  | `List values ->
      List.exists
        (function `String value -> String.equal value actual | _ -> false)
        values
  | json ->
      Alcotest.failf "unsupported expected value %s"
        (Yojson.Safe.to_string json)

let run_positive_case group_name variables index = function
  | `List [ `String template; expected ] -> (
      match Rfc6570.parse template with
      | Error errors ->
          Alcotest.failf "%s[%d]: parse failed with %d error(s)" group_name
            index (List.length errors)
      | Ok template_value -> (
          match Rfc6570.expand template_value (binding variables) with
          | Error errors ->
              Alcotest.failf "%s[%d]: expand failed with %d error(s)" group_name
                index (List.length errors)
          | Ok actual ->
              if not (expected_matches expected actual) then
                Alcotest.failf "%s[%d]: unexpected expansion %S" group_name
                  index actual))
  | json ->
      Alcotest.failf "%s[%d]: malformed testcase %s" group_name index
        (Yojson.Safe.to_string json)

let variables_and_testcases group_name group_json =
  match group_json with
  | `Assoc fields -> (
      match
        (List.assoc_opt "variables" fields, List.assoc_opt "testcases" fields)
      with
      | Some variables, Some (`List testcases) ->
          (variables_of_json variables, testcases)
      | _ -> Alcotest.failf "%s: missing variables/testcases" group_name)
  | json ->
      Alcotest.failf "%s: malformed group %s" group_name
        (Yojson.Safe.to_string json)

let run_positive_group (group_name, group_json) =
  let variables, testcases = variables_and_testcases group_name group_json in
  List.iteri (run_positive_case group_name variables) testcases

let run_positive_suite path =
  match load_json path with
  | `Assoc groups -> List.iter run_positive_group groups
  | json ->
      Alcotest.failf "suite root must be object, got %s"
        (Yojson.Safe.to_string json)

let case_fails variables template =
  match Rfc6570.parse template with
  | Error _ -> true
  | Ok parsed -> (
      match Rfc6570.expand parsed (binding variables) with
      | Error _ -> true
      | Ok _ -> false)

let run_negative_case group_name variables index = function
  | `List [ `String template; `Bool false ] ->
      Alcotest.(check bool)
        (Printf.sprintf "%s[%d] fails" group_name index)
        true
        (case_fails variables template)
  | json ->
      Alcotest.failf "%s[%d]: malformed negative testcase %s" group_name index
        (Yojson.Safe.to_string json)

let run_negative_group (group_name, group_json) =
  match group_json with
  | `Assoc fields -> (
      match
        (List.assoc_opt "variables" fields, List.assoc_opt "testcases" fields)
      with
      | Some variables, Some (`List testcases) ->
          List.iteri
            (run_negative_case group_name (variables_of_json variables))
            testcases
      | _ -> Alcotest.failf "%s: missing variables/testcases" group_name)
  | json ->
      Alcotest.failf "%s: malformed group %s" group_name
        (Yojson.Safe.to_string json)

let run_negative_suite path =
  match load_json path with
  | `Assoc groups -> List.iter run_negative_group groups
  | json ->
      Alcotest.failf "suite root must be object, got %s"
        (Yojson.Safe.to_string json)
