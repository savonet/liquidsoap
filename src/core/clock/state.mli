include module type of struct
  include Types.State
end

val role : source -> [ `Passive | `Active | `Output ]

(** What animates and resets an active or output source. *)
val active : source -> active option

(** The sync error of [clock], or of an operator, whose sources report these
    distinct sync sources. *)
val sync_error :
  clock:string ->
  (< id : string ; stack : Pos.t list ; .. > * Sync_source.t) list ->
  exn

module Transition : sig
  val run : (unit -> 'a) -> 'a
end

val global_stop : bool Atomic.t
val application_started : bool Atomic.t
val window_length : float

(** The handle a clock made for itself: it follows merges like any other. *)
val handle : clock -> t

val get : t -> clock
val equal : t -> t -> bool
val compare : t -> t -> int
val lifecycle : clock -> lifecycle
val state : clock -> [ `Started | `Stopped | `Stopping ]

(** The current run of the clock, if it has one. *)
val streaming : clock -> streaming option

val clock_id : clock -> string option
val running : clock -> bool
val stream_time : streaming -> float

module Registry : sig
  val identities : int Atomic.t
  val watch : clock -> unit
  val wait : clock -> unit
  val run : clock -> unit
  val remove : clock -> unit
  val by_creation : clock list -> clock list
  val waiting_clocks : unit -> clock list
  val running_clocks : unit -> clock list
  val move_id : from:clock -> into:clock -> string -> unit
  val drop_id : clock -> unit
end

val clock_name : clock -> string
val string_of_controller : clock -> string
val set_clock_id : clock -> string -> unit

(** The clock's own logger, labelled [clock.<name>]. *)
val logger : clock -> Log.t

(** Logs an event under the clock's name. *)
val emit : clock -> Event.event_kind -> unit

(** Whether a debug event is worth building. *)
val wants_debug : clock -> bool

val entry : source -> Status.entry
val members : streaming -> (source * member) list
val active_members : streaming -> (source * member) list
val passive_sources : streaming -> source list
val measures : clock -> streaming -> bool
val now : streaming -> float
val lateness : clock -> streaming -> float option
val span_figures : span -> Status.figures -> Status.figures
val restart_span : span -> mark:Status.figures -> since:float -> unit
val recent_figures : streaming -> Status.figures
val stop_check : unit -> unit
val not_running : clock -> 'a
val started_streaming : clock -> streaming

(** The streaming state of a clock that is started or stopping. *)
val running_streaming : clock -> streaming

val quietly : clock -> string -> (unit -> unit) -> unit
val latency : streaming -> float
val max_latency : streaming -> float

(** How much of the clock's thread lease is left: spec/pacing.md §8. *)
val lease_left : streaming -> float

val update_figures : streaming -> (Status.figures -> Status.figures) -> unit

(** Tells the parent's streaming state that this clock's answer to "blocks"
    changed. *)
val tell_parent : clock -> bool -> unit

val interrupt : streaming -> unit

(** Returns whether this call is the one that stopped the clock. *)
val request_stop : clock -> Status.stop_reason -> bool

val is_top_level : clock -> bool
