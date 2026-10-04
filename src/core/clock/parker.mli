(** What a clock gives its worker back on, or sleeps on when it has a thread. A
    wake-up is advisory: whoever parks checks its own condition again. *)
type t

val create : unit -> t

(** Ends the park in progress, or the next one. Callable from any thread. *)
val unpark : t -> unit

(** Blocks the calling thread, until [delay] has elapsed if given. The deadline
    is a scheduler task. *)
val park_thread : ?delay:float -> t -> unit

(** Gives the scheduler worker back. Only from a computation under [Duppy.run].
*)
val park_task : ?delay:float -> t -> unit
