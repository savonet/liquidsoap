(** Strings with view on a substring. *)

type t

val of_string : string -> t

val to_string : t -> string

val length : t -> int

val sub : t -> int -> int -> t

val blit : t -> bytes -> int -> unit
