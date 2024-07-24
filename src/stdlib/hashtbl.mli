type (!'a, !'b) t = ('a, 'b) Stdlib.Hashtbl.t

val create : ?random:bool -> int -> ('a, 'b) t
val length : ('a, 'b) t -> int
val copy : ('a, 'b) t -> ('a, 'b) t
val mem : ('a, 'b) t -> 'a -> bool
val find : ('a, 'b) t -> 'a -> 'b
val find_opt : ('a, 'b) t -> 'a -> 'b option
val remove : ('a, 'b) t -> 'a -> unit
val replace : ('a, 'b) t -> 'a -> 'b -> unit
val iter : ('a -> 'b -> unit) -> ('a, 'b) t -> unit
val filter_map_inplace : ('a -> 'b -> 'b option) -> ('a, 'b) t -> unit
val fold : ('a -> 'b -> 'acc -> 'acc) -> ('a, 'b) t -> 'acc -> 'acc
val hash : 'a -> int
val reset : ('a, 'b) t -> unit

module Make : module type of Stdlib.Hashtbl.Make
