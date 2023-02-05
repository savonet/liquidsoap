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

val from_string : ?pos:pos list -> ?json5:bool -> string -> t
val to_string : ?compact:bool -> ?json5:bool -> t -> string
val get_string : t -> string
val get_int : t -> int
val get_bool : t -> bool
val get_tuple : t -> t list
val get_null : t -> t option
