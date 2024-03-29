type t =
  [ `Assoc of (string * t) list
  | `Tuple of t list
  | `String of string
  | `Bool of bool
  | `Float of float
  | `Int of int
  | `Null ]

(** A position. *)
type pos = Lexing.position * Lexing.position

type parse_error = { pos : pos; message : string }

exception Parse_error of parse_error

val from_string : ?pos:pos list -> ?json5:bool -> string -> t
val to_string : ?compact:bool -> ?json5:bool -> t -> string
