val set_failure_policy : (State.t -> Status.failure -> unit) -> unit

(** Asks the clock to stop, and winds it down when nothing ticks it. *)
val stop_clock : State.clock -> Status.stop_reason -> unit

val stop : State.t -> unit
val tick : ?pull:bool -> State.t -> unit
val activate_pending : State.t -> unit

(** Starts a sub-clock if it can start, and counts it on its parent's streaming
    state if it blocks. *)
val start_sub : force:bool -> State.streaming -> State.clock -> unit

val start : ?force:bool -> State.t -> unit

(** Returns how many waiting clocks it examined. *)
val start_pass : unit -> int

val start_scope : (unit -> 'a) -> 'a
val application_start : unit -> unit
val register : parent:State.t -> State.t -> unit
val deregister : parent:State.t -> State.t -> unit
val sub_clocks : State.t -> State.t list
