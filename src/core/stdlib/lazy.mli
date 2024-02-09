type 'a t

val from_fun : (unit -> 'a) -> 'a t
val from_val : 'a -> 'a t
val force : 'a t -> 'a
val is_val : 'a t -> bool
