val wanted_animator :
  State.clock -> State.streaming -> [> `Task | `Thread ] * string

type _ Effect.t +=
  | Context : (State.clock * State.streaming) option Effect.t
  | Hop : Status.animator -> unit Effect.t

(** The clock whose animator runs the calling computation, if any. *)
val context : unit -> (State.clock * State.streaming) option

(** Applies the sync source changes, then switches and changes animator if due:
    spec/pacing.md §3. *)
val pacing_point : State.clock -> State.streaming -> unit

(** What follows a tick: stop check, rest or lateness, time box. *)
val between_ticks : State.clock -> State.streaming -> unit
