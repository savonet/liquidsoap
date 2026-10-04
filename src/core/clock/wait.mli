type t

val create : unit -> t

(** The waits blocking a thread, which the global stop must end. *)
val blocked : t list Atomic.t

val signal : t -> unit
val until : t -> (unit -> bool) -> unit
