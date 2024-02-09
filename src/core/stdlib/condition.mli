type t

val create : unit -> t
val wait : t -> Mutex.t -> unit
val signal : t -> unit
