type operator =
  | Simple
  | Reserved
  | Fragment
  | Label
  | Path
  | Path_parameter
  | Query
  | Query_continuation

type position = { offset : int; line : int; column : int }
type prefix_modifier = { position : position; max_chars : int }
type modifier = No_modifier | Explode | Prefix of prefix_modifier
type varspec = { position : position; name : string; modifier : modifier }
type part = Literal of string | Expression of operator * varspec list
type t = { raw : string; parts : part list; variables : string list }
type item = Item_undefined | Item_string of string

type value =
  | Undefined
  | String of string
  | List of item list
  | Assoc of (string * item) list

type bindings = string -> value
type normalization = Preserve | Normalize_nfc
type options = { normalization : normalization }

let default_options = { normalization = Normalize_nfc }

type value_component =
  | Scalar_value
  | List_item of int
  | Assoc_key of int
  | Assoc_value of int

type error =
  | Invalid_literal of { position : position; character : Uchar.t }
  | Unmatched_open_brace of { position : position }
  | Unmatched_close_brace of { position : position }
  | Empty_expression of { position : position }
  | Reserved_operator of { position : position; operator : char }
  | Invalid_operator of { position : position; operator : Uchar.t }
  | Invalid_varname of { position : position; raw : string }
  | Invalid_percent_triplet of { position : position; raw : string }
  | Invalid_prefix_modifier of { position : position; raw : string }
  | Prefix_on_composite of { position : position; variable : string }
  | Invalid_value_utf8 of {
      position : position;
      variable : string;
      component : value_component;
      offset : int;
    }

type diagnostic = { output : string; errors : error list }

let position source offset =
  let line = ref 1 in
  let column = ref 1 in
  for i = 0 to min offset (String.length source) - 1 do
    if source.[i] = '\n' then (
      incr line;
      column := 1)
    else incr column
  done;
  { offset; line = !line; column = !column }

let uchar_of_byte c = Uchar.of_int (Char.code c)

let is_hex = function
  | '0' .. '9' | 'a' .. 'f' | 'A' .. 'F' -> true
  | _ -> false

let is_alpha = function 'A' .. 'Z' | 'a' .. 'z' -> true | _ -> false
let is_digit = function '0' .. '9' -> true | _ -> false
let is_varchar_byte = function '_' -> true | c -> is_alpha c || is_digit c

let add_unique acc value =
  if List.exists (String.equal value) acc then acc else acc @ [ value ]

let literal_ascii_allowed c =
  let code = Char.code c in
  code >= 0x21 && code <= 0x7e && not (String.contains "\"%<>\\^`{|}" c)

let in_ranges ranges code =
  List.exists (fun (low, high) -> code >= low && code <= high) ranges

let ucschar_ranges =
  [
    (0xA0, 0xD7FF);
    (0xF900, 0xFDCF);
    (0xFDF0, 0xFFEF);
    (0x10000, 0x1FFFD);
    (0x20000, 0x2FFFD);
    (0x30000, 0x3FFFD);
    (0x40000, 0x4FFFD);
    (0x50000, 0x5FFFD);
    (0x60000, 0x6FFFD);
    (0x70000, 0x7FFFD);
    (0x80000, 0x8FFFD);
    (0x90000, 0x9FFFD);
    (0xA0000, 0xAFFFD);
    (0xB0000, 0xBFFFD);
    (0xC0000, 0xCFFFD);
    (0xD0000, 0xDFFFD);
    (0xE1000, 0xEFFFD);
  ]

let iprivate_ranges =
  [ (0xE000, 0xF8FF); (0xF0000, 0xFFFFD); (0x100000, 0x10FFFD) ]

let is_ucschar code = in_ranges ucschar_ranges code
let is_iprivate code = in_ranges iprivate_ranges code
let literal_unicode_allowed code = is_ucschar code || is_iprivate code
let byte text i = Char.code text.[i]
let utf8_continuation byte = byte land 0xC0 = 0x80

let continuation text i =
  i < String.length text && utf8_continuation (byte text i)

let utf8_decode_at text i =
  let len = String.length text in
  let b0 = byte text i in
  if b0 >= 0xC2 && b0 <= 0xDF && i + 1 < len && continuation text (i + 1) then
    let b1 = byte text (i + 1) in
    Some (((b0 land 0x1F) lsl 6) lor (b1 land 0x3F), i + 2)
  else if b0 >= 0xE0 && b0 <= 0xEF && i + 2 < len && continuation text (i + 2)
  then
    let b1 = byte text (i + 1) in
    let b2 = byte text (i + 2) in
    if
      (b0 = 0xE0 && b1 >= 0xA0 && b1 <= 0xBF)
      || (b0 >= 0xE1 && b0 <= 0xEC && utf8_continuation b1)
      || (b0 = 0xED && b1 >= 0x80 && b1 <= 0x9F)
      || (b0 >= 0xEE && b0 <= 0xEF && utf8_continuation b1)
    then
      Some
        ( ((b0 land 0x0F) lsl 12) lor ((b1 land 0x3F) lsl 6) lor (b2 land 0x3F),
          i + 3 )
    else None
  else if
    b0 >= 0xF0 && b0 <= 0xF4
    && i + 3 < len
    && continuation text (i + 2)
    && continuation text (i + 3)
  then
    let b1 = byte text (i + 1) in
    let b2 = byte text (i + 2) in
    let b3 = byte text (i + 3) in
    if
      (b0 = 0xF0 && b1 >= 0x90 && b1 <= 0xBF)
      || (b0 >= 0xF1 && b0 <= 0xF3 && utf8_continuation b1)
      || (b0 = 0xF4 && b1 >= 0x80 && b1 <= 0x8F)
    then
      Some
        ( ((b0 land 0x07) lsl 18)
          lor ((b1 land 0x3F) lsl 12)
          lor ((b2 land 0x3F) lsl 6)
          lor (b3 land 0x3F),
          i + 4 )
    else None
  else None

let validate_literal ~source ~start text =
  let len = String.length text in
  let invalid i character =
    Error
      (Invalid_literal { position = position source (start + i); character })
  in
  let rec loop i =
    if i >= len then Ok ()
    else
      match text.[i] with
      | '%' ->
          if i + 2 < len && is_hex text.[i + 1] && is_hex text.[i + 2] then
            loop (i + 3)
          else
            Error
              (Invalid_percent_triplet
                 {
                   position = position source (start + i);
                   raw = String.sub text i (min 3 (String.length text - i));
                 })
      | c when Char.code c < 0x80 && literal_ascii_allowed c -> loop (i + 1)
      | c when Char.code c < 0x80 -> invalid i (uchar_of_byte c)
      | c -> (
          match utf8_decode_at text i with
          | Some (code, next) when literal_unicode_allowed code -> loop next
          | Some (code, _) -> invalid i (Uchar.of_int code)
          | None -> invalid i (uchar_of_byte c))
  in
  loop 0

let operator_of_char = function
  | '+' -> Some Reserved
  | '#' -> Some Fragment
  | '.' -> Some Label
  | '/' -> Some Path
  | ';' -> Some Path_parameter
  | '?' -> Some Query
  | '&' -> Some Query_continuation
  | _ -> None

let is_future_operator = function
  | '=' | ',' | '!' | '@' | '|' -> true
  | _ -> false

let split_commas text =
  let rec loop acc start i =
    if i = String.length text then
      List.rev (String.sub text start (i - start) :: acc)
    else if text.[i] = ',' then
      loop (String.sub text start (i - start) :: acc) (i + 1) (i + 1)
    else loop acc start (i + 1)
  in
  loop [] 0 0

let validate_varname ~source ~start raw =
  let len = String.length raw in
  if len = 0 then
    Error (Invalid_varname { position = position source start; raw })
  else
    let rec segment start_i i =
      if i >= len || raw.[i] = '.' then
        if i = start_i then
          Error (Invalid_varname { position = position source start; raw })
        else Ok i
      else
        match raw.[i] with
        | '%' ->
            if i + 2 < len && is_hex raw.[i + 1] && is_hex raw.[i + 2] then
              segment start_i (i + 3)
            else
              Error
                (Invalid_percent_triplet
                   {
                     position = position source (start + i);
                     raw = String.sub raw i (min 3 (String.length raw - i));
                   })
        | c when is_varchar_byte c -> segment start_i (i + 1)
        | _ -> Error (Invalid_varname { position = position source start; raw })
    in
    let rec loop i =
      match segment i i with
      | Error _ as error -> error
      | Ok segment_end ->
          if segment_end >= len then Ok () else loop (segment_end + 1)
    in
    loop 0

let parse_prefix ~source ~start raw =
  let len = String.length raw in
  let rec digits i =
    if i >= len then true else is_digit raw.[i] && digits (i + 1)
  in
  let error () =
    Error (Invalid_prefix_modifier { position = position source start; raw })
  in
  if len = 0 then error ()
  else if len > 4 then error ()
  else if raw.[0] = '0' then error ()
  else if not (digits 0) then error ()
  else Ok { position = position source start; max_chars = int_of_string raw }

let parse_varspec ~source ~start raw =
  let len = String.length raw in
  if len = 0 then
    Error (Invalid_varname { position = position source start; raw })
  else
    let parse_name name modifier =
      match validate_varname ~source ~start name with
      | Ok () -> Ok { position = position source start; name; modifier }
      | Error _ as error -> error
    in
    if raw.[len - 1] = '*' then
      let name = String.sub raw 0 (len - 1) in
      if String.contains name ':' then
        Error
          (Invalid_prefix_modifier { position = position source start; raw })
      else parse_name name Explode
    else
      match String.index_opt raw ':' with
      | None -> parse_name raw No_modifier
      | Some colon -> (
          let name = String.sub raw 0 colon in
          let prefix_raw = String.sub raw (colon + 1) (len - colon - 1) in
          match
            ( validate_varname ~source ~start name,
              parse_prefix ~source ~start:(start + colon + 1) prefix_raw )
          with
          | Ok (), Ok prefix ->
              Ok
                {
                  position = position source start;
                  name;
                  modifier = Prefix prefix;
                }
          | (Error _ as error), _ -> error
          | _, (Error _ as error) -> error)

let parse_expression_body ~source ~start body =
  if body = "" then
    Error (Empty_expression { position = position source start })
  else
    let operator, var_start =
      match operator_of_char body.[0] with
      | Some operator -> (operator, 1)
      | None -> (Simple, 0)
    in
    if var_start = 0 && (not (is_varchar_byte body.[0])) && body.[0] <> '%' then
      Error (Invalid_varname { position = position source start; raw = body })
    else if var_start >= String.length body then
      Error (Empty_expression { position = position source start })
    else
      let vars_raw =
        String.sub body var_start (String.length body - var_start)
        |> split_commas
      in
      let rec parse_all acc offset = function
        | [] -> Ok (Expression (operator, List.rev acc))
        | raw :: rest -> (
            match
              parse_varspec ~source ~start:(start + var_start + offset) raw
            with
            | Ok spec ->
                parse_all (spec :: acc) (offset + String.length raw + 1) rest
            | Error _ as error -> error)
      in
      parse_all [] 0 vars_raw

let parse_expression ~source ~start body =
  if body <> "" && is_future_operator body.[0] then
    Error
      (Reserved_operator
         { position = position source start; operator = body.[0] })
  else parse_expression_body ~source ~start body

let parse raw =
  let len = String.length raw in
  let add_literal start stop acc =
    if stop = start then Ok acc
    else
      let literal = String.sub raw start (stop - start) in
      match validate_literal ~source:raw ~start literal with
      | Ok () -> Ok (Literal literal :: acc)
      | Error _ as error -> error
  in
  let rec literal start i acc =
    if i >= len then
      match add_literal start i acc with
      | Ok parts -> Ok (List.rev parts)
      | Error _ as error -> error
    else
      match raw.[i] with
      | '{' -> (
          match add_literal start i acc with
          | Error _ as error -> error
          | Ok acc -> expression i (i + 1) acc)
      | '}' -> Error (Unmatched_close_brace { position = position raw i })
      | _ -> literal start (i + 1) acc
  and expression expr_start i acc =
    if i >= len then
      Error (Unmatched_open_brace { position = position raw expr_start })
    else
      match raw.[i] with
      | '{' -> Error (Unmatched_open_brace { position = position raw i })
      | '}' -> (
          let body = String.sub raw (expr_start + 1) (i - expr_start - 1) in
          match parse_expression ~source:raw ~start:(expr_start + 1) body with
          | Error _ as error -> error
          | Ok part -> literal (i + 1) (i + 1) (part :: acc))
      | _ -> expression expr_start (i + 1) acc
  in
  match literal 0 0 [] with
  | Error error -> Error [ error ]
  | Ok parts ->
      let variables =
        List.fold_left
          (fun acc -> function
            | Literal _ -> acc
            | Expression (_, specs) ->
                List.fold_left
                  (fun acc spec -> add_unique acc spec.name)
                  acc specs)
          [] parts
      in
      Ok { raw; parts; variables }

let of_string = parse
let to_string t = t.raw
let variables t = t.variables

type allow = Allow_unreserved | Allow_unreserved_reserved

type behavior = {
  first : string;
  sep : string;
  named : bool;
  if_empty : string;
  allow : allow;
}

let behavior = function
  | Simple ->
      {
        first = "";
        sep = ",";
        named = false;
        if_empty = "";
        allow = Allow_unreserved;
      }
  | Reserved ->
      {
        first = "";
        sep = ",";
        named = false;
        if_empty = "";
        allow = Allow_unreserved_reserved;
      }
  | Fragment ->
      {
        first = "#";
        sep = ",";
        named = false;
        if_empty = "";
        allow = Allow_unreserved_reserved;
      }
  | Label ->
      {
        first = ".";
        sep = ".";
        named = false;
        if_empty = "";
        allow = Allow_unreserved;
      }
  | Path ->
      {
        first = "/";
        sep = "/";
        named = false;
        if_empty = "";
        allow = Allow_unreserved;
      }
  | Path_parameter ->
      {
        first = ";";
        sep = ";";
        named = true;
        if_empty = "";
        allow = Allow_unreserved;
      }
  | Query ->
      {
        first = "?";
        sep = "&";
        named = true;
        if_empty = "=";
        allow = Allow_unreserved;
      }
  | Query_continuation ->
      {
        first = "&";
        sep = "&";
        named = true;
        if_empty = "=";
        allow = Allow_unreserved;
      }

let is_unreserved = function
  | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '-' | '.' | '_' | '~' -> true
  | _ -> false

let is_reserved = function
  | ':' | '/' | '?' | '#' | '[' | ']' | '@' | '!' | '$' | '&' | '\'' | '(' | ')'
  | '*' | '+' | ',' | ';' | '=' ->
      true
  | _ -> false

let pct_triplet_at text i =
  i + 2 < String.length text
  && text.[i] = '%'
  && is_hex text.[i + 1]
  && is_hex text.[i + 2]

let hex = "0123456789ABCDEF"

let add_pct_encoded_byte buffer byte =
  Buffer.add_char buffer '%';
  Buffer.add_char buffer hex.[byte lsr 4];
  Buffer.add_char buffer hex.[byte land 0x0f]

let encode allow text =
  let len = String.length text in
  let buffer = Buffer.create len in
  let rec loop i =
    if i >= len then Buffer.contents buffer
    else
      let c = text.[i] in
      if is_unreserved c then (
        Buffer.add_char buffer c;
        loop (i + 1))
      else if
        match allow with
        | Allow_unreserved -> false
        | Allow_unreserved_reserved -> is_reserved c
      then (
        Buffer.add_char buffer c;
        loop (i + 1))
      else if allow = Allow_unreserved_reserved && pct_triplet_at text i then (
        Buffer.add_substring buffer text i 3;
        loop (i + 3))
      else (
        add_pct_encoded_byte buffer (Char.code c);
        loop (i + 1))
  in
  loop 0

let expand_literal text =
  let len = String.length text in
  let buffer = Buffer.create len in
  let rec loop i =
    if i >= len then Buffer.contents buffer
    else if pct_triplet_at text i then (
      Buffer.add_substring buffer text i 3;
      loop (i + 3))
    else
      let c = text.[i] in
      if is_unreserved c || is_reserved c then (
        Buffer.add_char buffer c;
        loop (i + 1))
      else (
        add_pct_encoded_byte buffer (Char.code c);
        loop (i + 1))
  in
  loop 0

let is_utf8_continuation text i =
  i < String.length text
  &&
  let byte = Char.code text.[i] in
  byte land 0xc0 = 0x80

let utf8_step text i =
  let byte = Char.code text.[i] in
  if byte < 0x80 then 1
  else if byte land 0xe0 = 0xc0 && is_utf8_continuation text (i + 1) then 2
  else if
    byte land 0xf0 = 0xe0
    && is_utf8_continuation text (i + 1)
    && is_utf8_continuation text (i + 2)
  then 3
  else if
    byte land 0xf8 = 0xf0
    && is_utf8_continuation text (i + 1)
    && is_utf8_continuation text (i + 2)
    && is_utf8_continuation text (i + 3)
  then 4
  else
    (* Values are validated before prefix slicing; this fallback only prevents a
       private helper from stalling if the invariant is violated internally. *)
    (1 [@coverage off])

let prefix_text max_chars text =
  let len = String.length text in
  let rec loop chars i =
    if chars >= max_chars || i >= len then String.sub text 0 i
    else if pct_triplet_at text i then loop (chars + 1) (i + 3)
    else loop (chars + 1) (i + utf8_step text i)
  in
  loop 0 0

let validate_utf8 text =
  let len = String.length text in
  let rec loop i =
    if i >= len then Ok ()
    else if byte text i < 0x80 then loop (i + 1)
    else
      match utf8_decode_at text i with
      | Some (_, next) -> loop next
      | None -> Error i
  in
  loop 0

let invalid_value_utf8 spec component offset =
  Invalid_value_utf8
    { position = spec.position; variable = spec.name; component; offset }

let validate_value_text spec component text =
  match validate_utf8 text with
  | Ok () -> Ok ()
  | Error offset -> Error [ invalid_value_utf8 spec component offset ]

let normalize_text options text =
  match options.normalization with
  | Preserve -> text
  | Normalize_nfc -> Uunf_string.normalize_utf_8 `NFC text

let prepare_value_text options spec component text =
  match validate_value_text spec component text with
  | Error _ as error -> error
  | Ok () -> Ok (normalize_text options text)

let item_strings options spec items =
  let rec loop index acc = function
    | [] -> Ok (List.rev acc)
    | Item_undefined :: rest -> loop (index + 1) acc rest
    | Item_string value :: rest -> (
        match prepare_value_text options spec (List_item index) value with
        | Ok value -> loop (index + 1) (value :: acc) rest
        | Error _ as error -> error)
  in
  loop 0 [] items

let assoc_strings options spec items =
  let rec loop index acc = function
    | [] -> Ok (List.rev acc)
    | (_, Item_undefined) :: rest -> loop (index + 1) acc rest
    | (name, Item_string value) :: rest -> (
        match
          ( prepare_value_text options spec (Assoc_key index) name,
            prepare_value_text options spec (Assoc_value index) value )
        with
        | Ok name, Ok value -> loop (index + 1) ((name, value) :: acc) rest
        | (Error _ as error), _ -> error
        | _, (Error _ as error) -> error)
  in
  loop 0 [] items

let named_value behavior name value =
  if value = "" then name ^ behavior.if_empty else name ^ "=" ^ value

let expand_scalar options behavior spec value =
  match prepare_value_text options spec Scalar_value value with
  | Error _ as error -> error
  | Ok value ->
      let value =
        match spec.modifier with
        | Prefix prefix -> prefix_text prefix.max_chars value
        | No_modifier | Explode -> value
      in
      let encoded = encode behavior.allow value in
      if behavior.named then Ok [ named_value behavior spec.name encoded ]
      else Ok [ encoded ]

let expand_list options behavior spec items =
  match item_strings options spec items with
  | Error _ as error -> error
  | Ok values -> (
      if values = [] then Ok []
      else
        match spec.modifier with
        | Prefix prefix ->
            Error
              [
                Prefix_on_composite
                  { position = prefix.position; variable = spec.name };
              ]
        | No_modifier ->
            let encoded = List.map (encode behavior.allow) values in
            let joined = String.concat "," encoded in
            if behavior.named then Ok [ named_value behavior spec.name joined ]
            else Ok [ joined ]
        | Explode ->
            let encoded = List.map (encode behavior.allow) values in
            if behavior.named then
              Ok (List.map (named_value behavior spec.name) encoded)
            else Ok encoded)

let expand_assoc options behavior spec items =
  match assoc_strings options spec items with
  | Error _ as error -> error
  | Ok values -> (
      if values = [] then Ok []
      else
        match spec.modifier with
        | Prefix prefix ->
            Error
              [
                Prefix_on_composite
                  { position = prefix.position; variable = spec.name };
              ]
        | No_modifier ->
            let encoded =
              List.concat_map
                (fun (name, value) ->
                  [ encode behavior.allow name; encode behavior.allow value ])
                values
            in
            let joined = String.concat "," encoded in
            if behavior.named then Ok [ named_value behavior spec.name joined ]
            else Ok [ joined ]
        | Explode ->
            Ok
              (List.map
                 (fun (name, value) ->
                   let name = encode behavior.allow name in
                   let value = encode behavior.allow value in
                   if behavior.named then named_value behavior name value
                   else name ^ "=" ^ value)
                 values))

let expand_var options behavior lookup spec =
  match lookup spec.name with
  | Undefined -> Ok []
  | String value -> expand_scalar options behavior spec value
  | List items -> expand_list options behavior spec items
  | Assoc items -> expand_assoc options behavior spec items

let cached_lookup t bindings =
  let values = List.map (fun name -> (name, bindings name)) t.variables in
  fun name -> List.assoc name values

let expand_expression options lookup operator specs =
  let behavior = behavior operator in
  let rec loop acc = function
    | [] ->
        if acc = [] then Ok ""
        else Ok (behavior.first ^ String.concat behavior.sep (List.rev acc))
    | spec :: rest -> (
        match expand_var options behavior lookup spec with
        | Error _ as error -> error
        | Ok values -> loop (List.rev_append values acc) rest)
  in
  loop [] specs

let expand_with options t bindings =
  let lookup = cached_lookup t bindings in
  let rec loop acc = function
    | [] -> Ok (String.concat "" (List.rev acc))
    | Literal text :: rest -> loop (expand_literal text :: acc) rest
    | Expression (operator, specs) :: rest -> (
        match expand_expression options lookup operator specs with
        | Error _ as error -> error
        | Ok text -> loop (text :: acc) rest)
  in
  loop [] t.parts

let expand t bindings = expand_with default_options t bindings

type diagnostic_literal_scan =
  | Diagnostic_literal_end of int
  | Diagnostic_expression_start of int
  | Diagnostic_unmatched_close of int
  | Diagnostic_literal_error of error * int

let scan_diagnostic_literal source start =
  let len = String.length source in
  let invalid i character =
    Diagnostic_literal_error
      (Invalid_literal { position = position source i; character }, i)
  in
  let rec loop i =
    if i >= len then Diagnostic_literal_end i
    else
      match source.[i] with
      | '{' -> Diagnostic_expression_start i
      | '}' -> Diagnostic_unmatched_close i
      | '%' ->
          if i + 2 < len && is_hex source.[i + 1] && is_hex source.[i + 2] then
            loop (i + 3)
          else
            Diagnostic_literal_error
              ( Invalid_percent_triplet
                  {
                    position = position source i;
                    raw = String.sub source i (min 3 (String.length source - i));
                  },
                i )
      | c when Char.code c < 0x80 && literal_ascii_allowed c -> loop (i + 1)
      | c when Char.code c < 0x80 -> invalid i (uchar_of_byte c)
      | c -> (
          match utf8_decode_at source i with
          | Some (code, next) when literal_unicode_allowed code -> loop next
          | Some (code, _) -> invalid i (Uchar.of_int code)
          | None -> invalid i (uchar_of_byte c))
  in
  loop start

let find_char source start stop target =
  let rec loop i =
    if i >= stop then None
    else if source.[i] = target then Some i
    else loop (i + 1)
  in
  loop start

let diagnostic_lookup bindings =
  let cache = Hashtbl.create 16 in
  fun name ->
    match Hashtbl.find_opt cache name with
    | Some value -> value
    | None ->
        let value = bindings name in
        Hashtbl.add cache name value;
        value

let expand_diagnostic_with options raw bindings =
  let len = String.length raw in
  let output = Buffer.create len in
  let errors = ref [] in
  let lookup = diagnostic_lookup bindings in
  let add_error error = errors := error :: !errors in
  let add_errors new_errors = errors := List.rev_append new_errors !errors in
  let add_raw start stop =
    if stop > start then Buffer.add_substring output raw start (stop - start)
  in
  let add_expanded_literal start stop =
    if stop > start then
      Buffer.add_string output
        (expand_literal (String.sub raw start (stop - start)))
  in
  let rec scan_literal start =
    match scan_diagnostic_literal raw start with
    | Diagnostic_literal_end stop -> add_expanded_literal start stop
    | Diagnostic_expression_start expr_start ->
        add_expanded_literal start expr_start;
        scan_expression expr_start
    | Diagnostic_unmatched_close close ->
        add_expanded_literal start close;
        add_raw close len;
        add_error (Unmatched_close_brace { position = position raw close })
    | Diagnostic_literal_error (error, invalid_start) ->
        add_expanded_literal start invalid_start;
        add_raw invalid_start len;
        add_error error
  and scan_expression expr_start =
    match find_char raw (expr_start + 1) len '}' with
    | None ->
        add_raw expr_start len;
        add_error (Unmatched_open_brace { position = position raw expr_start })
    | Some expr_stop -> (
        let after_expr = expr_stop + 1 in
        match find_char raw (expr_start + 1) expr_stop '{' with
        | Some nested_start ->
            add_raw expr_start after_expr;
            add_error
              (Unmatched_open_brace { position = position raw nested_start });
            scan_literal after_expr
        | None -> (
            let body =
              String.sub raw (expr_start + 1) (expr_stop - expr_start - 1)
            in
            match parse_expression ~source:raw ~start:(expr_start + 1) body with
            | Error error ->
                add_raw expr_start after_expr;
                add_error error;
                scan_literal after_expr
            | Ok (Literal _) -> assert false
            | Ok (Expression (operator, specs)) -> (
                match expand_expression options lookup operator specs with
                | Ok text ->
                    Buffer.add_string output text;
                    scan_literal after_expr
                | Error expression_errors ->
                    add_raw expr_start after_expr;
                    add_errors expression_errors;
                    scan_literal after_expr)))
  in
  scan_literal 0;
  { output = Buffer.contents output; errors = List.rev !errors }

let expand_diagnostic raw bindings =
  expand_diagnostic_with default_options raw bindings
