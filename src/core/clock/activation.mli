val attach : State.t -> State.source -> unit
val detach : State.t -> State.source -> unit

(** Applies the queued removals. Only whoever ticks the clock calls this. *)
val apply_removals : State.clock -> State.streaming -> unit

(** The source error rule of spec/clock.md §12. *)
val source_error :
  State.clock -> State.streaming -> State.source -> (unit -> unit) -> unit

(** Activates every pending source, each under the source error rule. *)
val activate : State.clock -> State.streaming -> unit
