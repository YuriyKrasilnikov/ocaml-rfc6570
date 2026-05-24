let read_file path =
  let ic = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr ic)
    (fun () ->
      let len = in_channel_length ic in
      really_input_string ic len)

type row = {
  id : string;
  requirement_refs : string;
  operator_axis : string;
  value_axis : string;
  modifier_axis : string;
  options : string;
  template : string;
  expected : string;
}

let parse_row line =
  match String.split_on_char '\t' line with
  | [
   id;
   requirement_refs;
   operator_axis;
   value_axis;
   modifier_axis;
   options;
   template;
   expected;
  ] ->
      {
        id;
        requirement_refs;
        operator_axis;
        value_axis;
        modifier_axis;
        options;
        template;
        expected;
      }
  | fields ->
      Alcotest.failf "malformed TSV row with %d fields: %S" (List.length fields)
        line

let rows () =
  read_file "generated/operator-matrix.tsv"
  |> String.split_on_char '\n'
  |> List.filter (fun line -> not (String.equal line ""))
  |> function
  | [] -> Alcotest.fail "operator-matrix.tsv is empty"
  | header :: rows ->
      Alcotest.(check string)
        "header"
        "id\trequirement_refs\toperator_axis\tvalue_axis\tmodifier_axis\toptions\ttemplate\texpected"
        header;
      List.map parse_row rows

let bindings = function
  | "var" -> Rfc6570.String "value"
  | "empty" -> Rfc6570.String ""
  | "reserved" -> Rfc6570.String "a/b?c%20d e!"
  | "badpct" -> Rfc6570.String "x%zz%"
  | "long" -> Rfc6570.String "abcdefghij"
  | "decomposed" -> Rfc6570.String "e\204\129"
  | "list" ->
      Rfc6570.List
        [
          Rfc6570.Item_string "red";
          Rfc6570.Item_string "green";
          Rfc6570.Item_string "blue";
        ]
  | "list_empty_item" ->
      Rfc6570.List
        [
          Rfc6570.Item_string "red";
          Rfc6570.Item_string "";
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
  | "empty_value_keys" ->
      Rfc6570.Assoc
        [ ("a", Rfc6570.Item_string ""); ("b", Rfc6570.Item_string "2") ]
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

let options = function
  | "default" -> Rfc6570.default_options
  | "preserve" -> { Rfc6570.normalization = Rfc6570.Preserve }
  | options -> Alcotest.failf "unknown options %S" options

let check_row row =
  if String.equal row.requirement_refs "" then
    Alcotest.failf "%s has empty requirement refs" row.id;
  if
    String.equal row.operator_axis ""
    || String.equal row.value_axis ""
    || String.equal row.modifier_axis ""
  then Alcotest.failf "%s has empty matrix axes" row.id;
  match Rfc6570.parse row.template with
  | Error errors ->
      Alcotest.failf "%s parse failed with %d error(s)" row.id
        (List.length errors)
  | Ok template -> (
      match Rfc6570.expand_with (options row.options) template bindings with
      | Error errors ->
          Alcotest.failf "%s expand failed with %d error(s)" row.id
            (List.length errors)
      | Ok actual -> Alcotest.(check string) row.id row.expected actual)

let test_generated_operator_matrix () = List.iter check_row (rows ())

let () =
  Alcotest.run "rfc6570-generated-operator-matrix"
    [
      ( "generated operator matrix",
        [
          Alcotest.test_case "operator/value/modifier expectations" `Quick
            test_generated_operator_matrix;
        ] );
    ]
