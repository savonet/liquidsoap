type 'a t

val from_lazy : 'a Lazy.t -> 'a t
val from_val : 'a -> 'a t
val from_fun : (unit -> 'a) -> 'a t
val force : 'a t -> 'a
val to_fun : 'a t -> unit -> 'a
val map : ('a -> 'b) -> 'a t -> 'b Lazy.t * Mutex.t
val is_val : 'a t -> bool
