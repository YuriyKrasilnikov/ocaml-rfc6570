let parse raw =
  match Rfc6570.parse raw with
  | Ok template -> template
  | Error errors ->
      Alcotest.failf "parse %S failed with %d error(s)" raw (List.length errors)

let value_bindings name =
  match name with
  | "var" -> Rfc6570.String "value"
  | "empty" -> Rfc6570.String ""
  | "reserved" -> Rfc6570.String "a/b?c%20d e!"
  | "badpct" -> Rfc6570.String "x%zz%"
  | "u" -> Rfc6570.Undefined
  | "list" ->
      Rfc6570.List
        [
          Rfc6570.Item_string "red";
          Rfc6570.Item_string "green";
          Rfc6570.Item_string "blue";
        ]
  | "partial_list" ->
      Rfc6570.List
        [
          Rfc6570.Item_undefined;
          Rfc6570.Item_string "red";
          Rfc6570.Item_undefined;
          Rfc6570.Item_string "blue";
        ]
  | "undefined_list" -> Rfc6570.List [ Rfc6570.Item_undefined ]
  | "empty_list" -> Rfc6570.List []
  | "keys" ->
      Rfc6570.Assoc
        [
          ("semi", Rfc6570.Item_string ";");
          ("dot", Rfc6570.Item_string ".");
          ("comma", Rfc6570.Item_string ",");
        ]
  | "partial_keys" ->
      Rfc6570.Assoc
        [
          ("a", Rfc6570.Item_undefined);
          ("b", Rfc6570.Item_string "2");
          ("c", Rfc6570.Item_undefined);
        ]
  | "undefined_keys" -> Rfc6570.Assoc [ ("a", Rfc6570.Item_undefined) ]
  | "empty_keys" -> Rfc6570.Assoc []
  | _ -> Rfc6570.Undefined

let expand_with bindings raw =
  match Rfc6570.expand (parse raw) bindings with
  | Ok value -> value
  | Error errors ->
      Alcotest.failf "expand %S failed with %d error(s)" raw
        (List.length errors)

let expand raw = expand_with value_bindings raw
let check raw expected = Alcotest.(check string) raw expected (expand raw)

let check_cases cases =
  List.iter (fun (raw, expected) -> check raw expected) cases

let test_scalar_defined_empty_missing_by_operator () =
  check_cases
    [
      ("{var,empty,missing}", "value,");
      ("{+var,empty,missing}", "value,");
      ("{#var,empty,missing}", "#value,");
      ("{.var,empty,missing}", ".value.");
      ("{/var,empty,missing}", "/value/");
      ("{;var,empty,missing}", ";var=value;empty");
      ("{?var,empty,missing}", "?var=value&empty=");
      ("{&var,empty,missing}", "&var=value&empty=");
    ]

let test_all_undefined_omits_operator_prefix () =
  check_cases
    [
      ("{missing,u}", "");
      ("{+missing,u}", "");
      ("{#missing,u}", "");
      ("{.missing,u}", "");
      ("{/missing,u}", "");
      ("{;missing,u}", "");
      ("{?missing,u}", "");
      ("{&missing,u}", "");
    ]

let test_scalar_encoding_by_operator () =
  check_cases
    [
      ("{reserved}", "a%2Fb%3Fc%2520d%20e%21");
      ("{+reserved}", "a/b?c%20d%20e!");
      ("{#reserved}", "#a/b?c%20d%20e!");
      ("{.reserved}", ".a%2Fb%3Fc%2520d%20e%21");
      ("{/reserved}", "/a%2Fb%3Fc%2520d%20e%21");
      ("{;reserved}", ";reserved=a%2Fb%3Fc%2520d%20e%21");
      ("{?reserved}", "?reserved=a%2Fb%3Fc%2520d%20e%21");
      ("{&reserved}", "&reserved=a%2Fb%3Fc%2520d%20e%21");
      ("{+badpct}", "x%25zz%25");
      ("{#badpct}", "#x%25zz%25");
    ]

let test_list_no_modifier_by_operator () =
  check_cases
    [
      ("{list}", "red,green,blue");
      ("{+list}", "red,green,blue");
      ("{#list}", "#red,green,blue");
      ("{.list}", ".red,green,blue");
      ("{/list}", "/red,green,blue");
      ("{;list}", ";list=red,green,blue");
      ("{?list}", "?list=red,green,blue");
      ("{&list}", "&list=red,green,blue");
    ]

let test_list_explode_by_operator () =
  check_cases
    [
      ("{list*}", "red,green,blue");
      ("{+list*}", "red,green,blue");
      ("{#list*}", "#red,green,blue");
      ("{.list*}", ".red.green.blue");
      ("{/list*}", "/red/green/blue");
      ("{;list*}", ";list=red;list=green;list=blue");
      ("{?list*}", "?list=red&list=green&list=blue");
      ("{&list*}", "&list=red&list=green&list=blue");
    ]

let test_assoc_no_modifier_by_operator () =
  check_cases
    [
      ("{keys}", "semi,%3B,dot,.,comma,%2C");
      ("{+keys}", "semi,;,dot,.,comma,,");
      ("{#keys}", "#semi,;,dot,.,comma,,");
      ("{.keys}", ".semi,%3B,dot,.,comma,%2C");
      ("{/keys}", "/semi,%3B,dot,.,comma,%2C");
      ("{;keys}", ";keys=semi,%3B,dot,.,comma,%2C");
      ("{?keys}", "?keys=semi,%3B,dot,.,comma,%2C");
      ("{&keys}", "&keys=semi,%3B,dot,.,comma,%2C");
    ]

let test_assoc_explode_by_operator () =
  check_cases
    [
      ("{keys*}", "semi=%3B,dot=.,comma=%2C");
      ("{+keys*}", "semi=;,dot=.,comma=,");
      ("{#keys*}", "#semi=;,dot=.,comma=,");
      ("{.keys*}", ".semi=%3B.dot=..comma=%2C");
      ("{/keys*}", "/semi=%3B/dot=./comma=%2C");
      ("{;keys*}", ";semi=%3B;dot=.;comma=%2C");
      ("{?keys*}", "?semi=%3B&dot=.&comma=%2C");
      ("{&keys*}", "&semi=%3B&dot=.&comma=%2C");
    ]

let test_undefined_composite_members_by_operator () =
  check_cases
    [
      ("{partial_list}", "red,blue");
      ("{;partial_list}", ";partial_list=red,blue");
      ("{?partial_list}", "?partial_list=red,blue");
      ("{&partial_list}", "&partial_list=red,blue");
      ("{.partial_list*}", ".red.blue");
      ("{/partial_list*}", "/red/blue");
      ("{;partial_list*}", ";partial_list=red;partial_list=blue");
      ("{?partial_list*}", "?partial_list=red&partial_list=blue");
      ("{partial_keys}", "b,2");
      ("{.partial_keys*}", ".b=2");
      ("{/partial_keys*}", "/b=2");
      ("{;partial_keys*}", ";b=2");
      ("{?partial_keys*}", "?b=2");
      ("{undefined_list,var}", "value");
      ("{/undefined_list,var}", "/value");
      ("{;undefined_list,var}", ";var=value");
      ("{?undefined_keys,var}", "?var=value");
      ("{&empty_list,empty_keys,var}", "&var=value");
      ("{undefined_list}", "");
      ("{+undefined_list}", "");
      ("{#undefined_list}", "");
      ("{.undefined_list}", "");
      ("{/undefined_list}", "");
      ("{;undefined_list}", "");
      ("{?undefined_list}", "");
      ("{&undefined_list}", "");
      ("{undefined_keys}", "");
      ("{+undefined_keys}", "");
      ("{#undefined_keys}", "");
      ("{.undefined_keys}", "");
      ("{/undefined_keys}", "");
      ("{;undefined_keys}", "");
      ("{?undefined_keys}", "");
      ("{&undefined_keys}", "");
      ("{empty_list}", "");
      ("{+empty_list}", "");
      ("{#empty_list}", "");
      ("{.empty_list}", "");
      ("{/empty_list}", "");
      ("{;empty_list}", "");
      ("{?empty_list}", "");
      ("{&empty_list}", "");
      ("{empty_keys}", "");
      ("{+empty_keys}", "");
      ("{#empty_keys}", "");
      ("{.empty_keys}", "");
      ("{/empty_keys}", "");
      ("{;empty_keys}", "");
      ("{?empty_keys}", "");
      ("{&empty_keys}", "");
    ]

let test_repeated_query_variable_is_stable_per_expansion () =
  let check_stable label template expected =
    let calls = ref 0 in
    let bindings = function
      | "var" ->
          incr calls;
          Rfc6570.String (Printf.sprintf "v%d" !calls)
      | _ -> Rfc6570.Undefined
    in
    Alcotest.(check string) label expected (expand_with bindings template);
    Alcotest.(check int) (label ^ " lookup count") 1 !calls
  in
  check_stable "query repeated value" "{?var,var}" "?var=v1&var=v1";
  check_stable "continuation repeated value" "{&var,var}" "&var=v1&var=v1"

let () =
  Alcotest.run "rfc6570-operator-cross-product"
    [
      ( "operator cross product",
        [
          Alcotest.test_case "scalar defined empty missing" `Quick
            test_scalar_defined_empty_missing_by_operator;
          Alcotest.test_case "all undefined omits prefix" `Quick
            test_all_undefined_omits_operator_prefix;
          Alcotest.test_case "scalar encoding" `Quick
            test_scalar_encoding_by_operator;
          Alcotest.test_case "list no modifier" `Quick
            test_list_no_modifier_by_operator;
          Alcotest.test_case "list explode" `Quick test_list_explode_by_operator;
          Alcotest.test_case "assoc no modifier" `Quick
            test_assoc_no_modifier_by_operator;
          Alcotest.test_case "assoc explode" `Quick
            test_assoc_explode_by_operator;
          Alcotest.test_case "undefined composite members" `Quick
            test_undefined_composite_members_by_operator;
          Alcotest.test_case "repeated query variable stable" `Quick
            test_repeated_query_variable_is_stable_per_expansion;
        ] );
    ]
