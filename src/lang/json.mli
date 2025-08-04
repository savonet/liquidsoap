type t =
  [ `Assoc of (string * t) list
  | `Tuple of t list
  | `String of string
  | `Bool of bool
  | `Float of float
  | `Int of int
  | `Null ]

val from_string : ?pos:Pos.t list -> ?json5:bool -> string -> t
val to_string : ?compact:bool -> ?json5:bool -> t -> string
