type t

val create : unit -> t
val lock : t -> unit
val try_lock : t -> bool
val unlock : t -> unit
val mutexify : t -> ('a -> 'b) -> 'a -> 'b
