type parse_error = { pos : Pos.t; message : string }

exception Parse_error of parse_error

type t =
  [ `Assoc of (string * t) list
  | `Tuple of t list
  | `String of string
  | `Bool of bool
  | `Float of float
  | `Int of int
  | `Null ]
