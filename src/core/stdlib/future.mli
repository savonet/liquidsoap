module Queue = Liquidsoap_lang.Queues.Queue

type 'a t
type after_eval = (unit -> unit) Queue.t

val make : ?after_eval:after_eval -> (unit -> 'a) -> 'a t
val make_list : ?after_eval:after_eval -> (unit -> 'a) list -> unit t
val make_promise : unit -> ('a -> unit) * 'a t
val compute : 'a t -> unit
val await : ?run_after_eval:bool -> 'a t -> 'a
val eval : (unit -> 'a) -> 'a
