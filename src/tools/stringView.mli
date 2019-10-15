(** Strings with view on a substring. *)

type t

val of_string : string -> t

val of_substring : string -> int -> int -> t

val to_string : t -> string

val to_substring : t -> string * int * int

val is_empty : t -> bool

val length : t -> int

val sub : t -> int -> int -> t

val blit : t -> bytes -> int -> unit
