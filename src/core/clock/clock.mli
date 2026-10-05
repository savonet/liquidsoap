type activation = < id : string >
type active = < id : string ; reset : unit ; output : unit >
type source_type = [ `Passive | `Active of active | `Output of active ]

(** What a clock requires of a source: spec/clock.md §15.

    The clock wakes an output with the output itself as the requester, and keeps
    the activation until it winds down. [on_sync_source] subscribes to the
    changes of [sync_source] and returns what unsubscribes. *)
type source =
  < id : string
  ; stack : Pos.t list
  ; source_type : [ `Passive | `Active of active | `Output of active ]
  ; wake_up : source -> activation
  ; sleep : activation -> unit
  ; sync_source : Sync_source.t option
  ; on_sync_source : (Sync_source.t option -> unit) -> unit -> unit
  ; activations : activation list >

module Settings = Settings
module Status = Status
module Sync_source = Sync_source

type sync_mode = Status.sync_mode

val string_of_sync_mode : sync_mode -> string

(** Raises [Invalid_argument] on anything but [auto], [cpu], [none] and
    [passive]. *)
val sync_mode_of_string : string -> sync_mode

type owner = Status.owner = { kind : string; id : string }
type animator = Status.animator

type failure = Status.failure = {
  error : exn;
  backtrace : Printexc.raw_backtrace;
  source : string option;
}

type stop_reason = Status.stop_reason
type activity = Status.activity
type figures = Status.figures
type reported = { sync_source : string; source : string; stack : Pos.t list }

exception Conflict of { pos : Pos.t option; left : string; right : string }
exception Loop of { pos : Pos.t option; left : string; right : string }

exception
  Controller_conflict of {
    pos : Pos.t option;
    left : string;
    left_controller : string;
    right : string;
    right_controller : string;
  }

exception Sync_error of { clock : string; reported : reported list }

(** The sync error of a clock, or of an operator named in its place, whose
    sources report these distinct sync sources. *)
val sync_error :
  clock:string ->
  (< id : string ; stack : Pos.t list ; .. > * Sync_source.t) list ->
  exn

exception Not_running of string

exception
  Cannot_start of {
    clock : string;
    reason :
      [ `Not_stopped | `Global_stop | `Application_not_started | `No_output ];
  }

exception Not_a_sub_clock of { clock : string; parent : string }

(** Abandons a tick when the application stops. Never an error. *)
exception Stop_signal

(** A handle. Several handles designate one clock after {!unify}. *)
type t

(** Raises [Invalid_argument] for a passive clock with neither [parent] nor
    [owner], and for any other clock with either. *)
val create :
  ?stack:Pos.t list ->
  ?on_error:(exn -> Printexc.raw_backtrace -> unit) ->
  ?id:string ->
  ?sync:sync_mode ->
  ?parent:t ->
  ?owner:owner ->
  unit ->
  t

val equal : t -> t -> bool
val compare : t -> t -> int
val name : t -> string
val id : t -> string option
val set_id : t -> string -> unit
val sync_mode : t -> sync_mode
val owner : t -> owner option
val parent : t -> t option
val stack : t -> Pos.t list

(** Does nothing when the stack is already set. *)
val set_stack : t -> Pos.t list -> unit

val on_error : t -> (exn -> Printexc.raw_backtrace -> unit) -> unit

(** Raises [Cannot_start]. *)
val start : ?force:bool -> t -> unit

val stop : t -> unit
val started : t -> bool

(** [None] unless the clock is stopped. *)
val stop_reason : t -> stop_reason option

(** Starts every waiting clock that can start. Returns how many it examined. *)
val start_pass : unit -> int

(** Runs the function, then a start pass unless it raised. *)
val start_scope : (unit -> 'a) -> 'a

val application_start : unit -> unit

(** Sets the global stop, stops every running clock and waits for them, up to
    [clock.shutdown_wait]. *)
val shutdown : unit -> unit

(** Called once per failed clock, after it was wound down. The default asks for
    the application to shut down. *)
val set_failure_policy : (t -> failure -> unit) -> unit

val attach : t -> source -> unit
val detach : t -> source -> unit
val pending : t -> source list

(** [None] on a clock that is not running, like {!time}. *)
val ticks : t -> int option

val time : t -> float option

(** The [Liq_time] implementation that [clock.preferred] names, for callers that
    keep time on their own. *)
val time_implementation : unit -> Liq_time.implementation

(** [ticks], with 0 for a clock that is not running. *)
val tick_count : t -> int

val self_sync : t -> bool
val pulled : t -> bool

(** Ticks a passive clock. Raises [Not_running], or [Stop_signal] once the
    application stops. *)
val tick : ?pull:bool -> t -> unit

(** Activates the pending sources of a passive clock outside of a tick. *)
val activate_pending : t -> unit

val on_tick : t -> (unit -> unit) -> unit
val after_tick : t -> (unit -> unit) -> unit

(** Counted. Raises [Not_a_sub_clock]. *)
val register : parent:t -> t -> unit

val deregister : parent:t -> t -> unit
val sub_clocks : t -> t list

(** Raises [Conflict], [Loop] or [Controller_conflict], and then changes
    nothing. *)
val unify : pos:Pos.t option -> t -> t -> unit

(** What hands work to the scheduler and waits for it goes through this, so that
    a tick running on a scheduler worker gives it back meanwhile. *)
module Wait : sig
  type t

  val create : unit -> t

  (** To be called after what [until] checks has changed. *)
  val signal : t -> unit

  (** Returns once the condition holds, or the clock or the application is asked
      to stop. *)
  val until : t -> (unit -> bool) -> unit
end

val status : t -> Status.t

(** The clocks of the application, in creation order. *)
val clocks : unit -> t list

val statuses : unit -> Status.t list

module Event = Event

(** Every log event, in structured form. *)
val on_event : (Event.event -> unit) -> unit
