let read_file path =
  let ic = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr ic)
    (fun () ->
      let len = in_channel_length ic in
      really_input_string ic len)

let hex_value = function
  | '0' .. '9' as c -> Char.code c - Char.code '0'
  | 'a' .. 'f' as c -> 10 + Char.code c - Char.code 'a'
  | 'A' .. 'F' as c -> 10 + Char.code c - Char.code 'A'
  | c -> Alcotest.failf "invalid hex digit %C" c

let hex_byte hi lo = (hex_value hi lsl 4) lor hex_value lo

let decode_template text =
  let len = String.length text in
  let buf = Buffer.create len in
  let rec loop i =
    if i >= len then Buffer.contents buf
    else
      match text.[i] with
      | '\\' when i + 1 < len -> (
          match text.[i + 1] with
          | '\\' ->
              Buffer.add_char buf '\\';
              loop (i + 2)
          | 'n' ->
              Buffer.add_char buf '\n';
              loop (i + 2)
          | 't' ->
              Buffer.add_char buf '\t';
              loop (i + 2)
          | 'x' when i + 3 < len ->
              Buffer.add_char buf
                (Char.chr (hex_byte text.[i + 2] text.[i + 3]));
              loop (i + 4)
          | 'u' when i + 2 < len && text.[i + 2] = '{' ->
              let close =
                match String.index_from_opt text (i + 3) '}' with
                | Some close -> close
                | None ->
                    Alcotest.failf "unterminated unicode escape in %S" text
              in
              let raw = String.sub text (i + 3) (close - i - 3) in
              Buffer.add_utf_8_uchar buf
                (Uchar.of_int (int_of_string ("0x" ^ raw)));
              loop (close + 1)
          | c -> Alcotest.failf "unsupported escape \\%c in %S" c text)
      | c ->
          Buffer.add_char buf c;
          loop (i + 1)
  in
  loop 0

let split_semicolon = function
  | "" | "-" -> []
  | text -> String.split_on_char ';' text

type row = {
  id : string;
  requirement_refs : string;
  case_kind : string;
  template : string;
  expected_error : string;
  expected_variables : string list;
  expected_expansion : string option;
}

let parse_row line =
  match String.split_on_char '\t' line with
  | [
   id;
   requirement_refs;
   case_kind;
   template;
   expected_error;
   expected_variables;
   expected_expansion;
  ] ->
      {
        id;
        requirement_refs;
        case_kind;
        template = decode_template template;
        expected_error;
        expected_variables = split_semicolon expected_variables;
        expected_expansion =
          (match expected_expansion with "-" -> None | value -> Some value);
      }
  | fields ->
      Alcotest.failf "malformed TSV row with %d fields: %S" (List.length fields)
        line

let rows () =
  read_file "generated/abnf-corpus.tsv"
  |> String.split_on_char '\n'
  |> List.filter (fun line -> not (String.equal line ""))
  |> function
  | [] -> Alcotest.fail "abnf-corpus.tsv is empty"
  | header :: rows ->
      Alcotest.(check string)
        "header"
        "id\trequirement_refs\tcase_kind\ttemplate\texpected_error\texpected_variables\texpected_expansion"
        header;
      List.map parse_row rows

let error_name = function
  | Rfc6570.Invalid_literal _ -> "Invalid_literal"
  | Rfc6570.Unmatched_open_brace _ -> "Unmatched_open_brace"
  | Rfc6570.Unmatched_close_brace _ -> "Unmatched_close_brace"
  | Rfc6570.Empty_expression _ -> "Empty_expression"
  | Rfc6570.Reserved_operator _ -> "Reserved_operator"
  | Rfc6570.Invalid_operator _ -> "Invalid_operator"
  | Rfc6570.Invalid_varname _ -> "Invalid_varname"
  | Rfc6570.Invalid_percent_triplet _ -> "Invalid_percent_triplet"
  | Rfc6570.Invalid_prefix_modifier _ -> "Invalid_prefix_modifier"
  | Rfc6570.Prefix_on_composite _ -> "Prefix_on_composite"
  | Rfc6570.Invalid_value_utf8 _ -> "Invalid_value_utf8"

let bindings = function
  | "var" -> Rfc6570.String "x"
  | "empty" -> Rfc6570.String ""
  | "long" -> Rfc6570.String "abcdefghij"
  | "list" ->
      Rfc6570.List [ Rfc6570.Item_string "red"; Rfc6570.Item_string "green" ]
  | "keys" ->
      Rfc6570.Assoc
        [ ("a", Rfc6570.Item_string "1"); ("b", Rfc6570.Item_string "2") ]
  | "%24id" -> Rfc6570.String "pct"
  | "semi.dot" -> Rfc6570.String "dotted"
  | "a.%62" -> Rfc6570.String "pctdot"
  | _ -> Rfc6570.Undefined

let check_parse_ok row =
  match Rfc6570.parse row.template with
  | Error errors ->
      Alcotest.failf "%s parse failed with %d error(s)" row.id
        (List.length errors)
  | Ok template -> (
      Alcotest.(check (list string))
        (row.id ^ " variables") row.expected_variables
        (Rfc6570.variables template);
      match row.expected_expansion with
      | None -> ()
      | Some expected -> (
          match Rfc6570.expand template bindings with
          | Ok actual -> Alcotest.(check string) row.id expected actual
          | Error errors ->
              Alcotest.failf "%s expand failed with %d error(s)" row.id
                (List.length errors)))

let check_parse_error row =
  match Rfc6570.parse row.template with
  | Ok _ -> Alcotest.failf "%s unexpectedly parsed" row.id
  | Error [ error ] ->
      Alcotest.(check string) row.id row.expected_error (error_name error)
  | Error errors ->
      Alcotest.failf "%s returned %d errors" row.id (List.length errors)

let check_row row =
  if String.equal row.requirement_refs "" then
    Alcotest.failf "%s has empty requirement refs" row.id;
  match row.case_kind with
  | "parse_ok" -> check_parse_ok row
  | "parse_error" -> check_parse_error row
  | kind -> Alcotest.failf "%s has unknown kind %S" row.id kind

let test_generated_abnf_corpus () = List.iter check_row (rows ())

let () =
  Alcotest.run "rfc6570-generated-abnf-corpus"
    [
      ( "generated ABNF corpus",
        [
          Alcotest.test_case "parse and expansion expectations" `Quick
            test_generated_abnf_corpus;
        ] );
    ]
