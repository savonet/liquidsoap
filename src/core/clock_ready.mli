type 'a t

val make : (unit -> 'a) -> 'a t
val process : 'a t -> 'a
val clock_pool : Moonpool.Ws_pool.t
