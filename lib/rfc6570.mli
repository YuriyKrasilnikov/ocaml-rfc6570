(** RFC 6570 URI Template parsing and expansion.

    This library implements URI Template processing only. It does not parse
    produced URI references, resolve them, apply IDNA, or implement reverse
    matching. *)

type t
(** Parsed URI Template. *)

type item =
  | Item_undefined
  | Item_string of string  (** Composite value item. *)

type value =
  | Undefined
  | String of string
  | List of item list
  | Assoc of (string * item) list  (** RFC 6570 variable value model. *)

type bindings = string -> value
(** Variable lookup used during one expansion. *)

type normalization =
  | Preserve
  | Normalize_nfc
      (** Unicode normalization policy for supplied values. [Preserve] keeps
          values unchanged after UTF-8 validation. [Normalize_nfc] normalizes
          scalar, list item, assoc key, and assoc value strings before prefix
          slicing and percent-encoding. *)

type options = { normalization : normalization }
(** Expansion options. *)

val default_options : options
(** Default expansion options. [normalization] is [Normalize_nfc]. *)

type position = { offset : int; line : int; column : int }
(** Source position. [offset] is byte-based. [line] and [column] are one-based.
*)

type value_component =
  | Scalar_value
  | List_item of int
  | Assoc_key of int
  | Assoc_value of int
      (** Component of a supplied value. Composite indexes are zero-based and
          refer to the caller-supplied list order. *)

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
      (** Typed parse and expansion errors. [Invalid_value_utf8.position] points
          to the variable occurrence in the template; [offset] is the invalid
          byte offset inside the caller-supplied value component. *)

type diagnostic = { output : string; errors : error list }
(** Diagnostic expansion result. [output] is the partial expansion described by
    RFC 6570 Section 3. [errors] are reported in source order. *)

val parse : string -> (t, error list) result
(** Parse a URI Template. *)

val of_string : string -> (t, error list) result
(** Alias for {!parse}. *)

val to_string : t -> string
(** Return the original template string. *)

val variables : t -> string list
(** Return variable names in first-seen order. *)

val expand_with : options -> t -> bindings -> (string, error list) result
(** Expand a URI Template with explicit options. *)

val expand : t -> bindings -> (string, error list) result
(** Expand a URI Template with {!default_options}. *)

val expand_diagnostic_with : options -> string -> bindings -> diagnostic
(** Expand a raw template in diagnostic mode with explicit options. *)

val expand_diagnostic : string -> bindings -> diagnostic
(** Expand a raw template in diagnostic mode with {!default_options}. Malformed
    expressions are preserved in [output] and reported in [errors]; malformed
    literal text stops processing and leaves the remainder unexpanded. *)
