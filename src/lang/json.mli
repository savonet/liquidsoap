type t =
  [ `Assoc of (string * t) list
  | `Tuple of t list
  | `String of string
  | `Bool of bool
  | `Float of float
  | `Int of int
  | `Null ]

type parse_error = { pos : Pos.t; message : string }

exception Parse_error of parse_error

val from_string : ?pos:Pos.t list -> ?json5:bool -> string -> t
val to_string : ?compact:bool -> ?json5:bool -> t -> string
