(** Streaming XXH3-64. *)

type state

val create : unit -> state
val update : state -> string -> int -> int -> unit
val digest : state -> int64
