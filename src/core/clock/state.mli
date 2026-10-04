type source =
  < activations : string list
  ; animate : unit
  ; id : string
  ; on_sync_source : (Sync_source.t option -> unit) -> unit -> unit
  ; reset : unit
  ; source_type : [ `Active | `Output | `Passive ]
  ; stack : Liquidsoap_lang_prelude.Pos.t list
  ; sync_source : Sync_source.t option
  ; wake_up : unit -> unit -> unit >

module Sources : sig
  type key = source
  type !'a t

  val create : int -> 'a t
  val clear : 'a t -> unit
  val reset : 'a t -> unit
  val copy : 'a t -> 'a t
  val add : 'a t -> key -> 'a -> unit
  val remove : 'a t -> key -> unit
  val find : 'a t -> key -> 'a
  val find_opt : 'a t -> key -> 'a option
  val find_all : 'a t -> key -> 'a list
  val replace : 'a t -> key -> 'a -> unit
  val mem : 'a t -> key -> bool
  val length : 'a t -> int
  val stats : 'a t -> Hashtbl.statistics
  val add_seq : 'a t -> (key * 'a) Seq.t -> unit
  val replace_seq : 'a t -> (key * 'a) Seq.t -> unit
  val of_seq : (key * 'a) Seq.t -> 'a t
  val clean : 'a t -> unit
  val stats_alive : 'a t -> Hashtbl.statistics
end

type reported = {
  sync_source : string;
  source : string;
  stack : Liquidsoap_lang_prelude.Pos.t list;
}

exception Conflict of { left : string; right : string }
exception Loop of { left : string; right : string }

exception
  Controller_conflict of {
    left : string;
    left_controller : string;
    right : string;
    right_controller : string;
  }

exception Sync_error of { clock : string; reported : reported list }
exception Not_running of string

exception
  Cannot_start of {
    clock : string;
    reason :
      [ `Application_not_started | `Global_stop | `No_output | `Not_stopped ];
  }

exception Not_a_sub_clock of { clock : string; parent : string }
exception Stop_signal

type member = {
  role : [ `Active | `Output ];
  removed : bool Atomic.t;
  mutable sync : Sync_source.t option;
  mutable change : int;
  mutable unsubscribe : unit -> unit;
}

type output = { sleep : unit -> unit; source : source; member : member }

type pace = {
  time_source : Sync_source.time_source;
  offset : float;
  followed : Sync_source.t option;
}

type streaming = {
  frame_duration : float;
  forced : bool;
  default_time_source : Sync_source.time_source;
  ticks : int Atomic.t;
  m : Mutex.t;
  outputs : output list Atomic.t;
  animated : member Sources.t;
  animated_sources : source Queues.WeakQueue.t;
  passive : source Queues.WeakQueue.t;
  removals : source list Atomic.t;
  changes : (source * Sync_source.t option) list Atomic.t;
  dirty : bool Atomic.t;
  on_tick : (unit -> unit) list Atomic.t;
  after_tick : (unit -> unit) list Atomic.t;
  pulled : bool Atomic.t;
  animator : (Status.animator * string) option Atomic.t;
  pace : pace Atomic.t;
  tracked : Sync_source.t option Atomic.t;
  blocking : bool Atomic.t;
  sub_blocking : int Atomic.t;
  parker : Parker.t;
  interrupt_wait : (unit -> unit) Atomic.t;
  activity : Status.activity Atomic.t;
  life : Status.figures Atomic.t;
  recent : Status.figures option Atomic.t;
  mutable changes_applied : int;
  mutable failing : string option;
  mutable worker_since : float;
  mutable tick_waited : float;
  tick_slowest : Status.slowest;
  mutable last_warning : float;
  mutable last_long_tick : float;
  mutable warning_mark : Status.figures;
  warning_slowest : Status.slowest;
  mutable window_mark : Status.figures;
  mutable window_started : float;
  window_slowest : Status.slowest;
}

type clock = {
  identity : int;
  id : string option Atomic.t;
  sync_mode : Status.sync_mode;
  parent : t option Atomic.t;
  owner : Status.owner option;
  stack : Liquidsoap_lang_prelude.Pos.t list Atomic.t;
  state : [ `Started | `Stopped | `Stopping ] Atomic.t;
  stop_reason : Status.stop_reason Atomic.t;
  pending : source list Atomic.t;
  subs : sub list Atomic.t;
  error_handlers : (exn -> Printexc.raw_backtrace -> unit) list Atomic.t;
  streaming : streaming option Atomic.t;
  ticking : bool Atomic.t;
  activated : int Atomic.t;
  self : t option Atomic.t;
}

and sub = { sub : t; registrants : int }
and t = clock Unifier.t

module Transition : sig
  val m : Mutex.t
  val holder : int Atomic.t
  val held : unit -> bool
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
val state : clock -> [ `Started | `Stopped | `Stopping ]
val running : clock -> bool
val stream_time : streaming -> float

module Registry : sig
  val m : Mutex.t
  val waiting : (int, clock Weak.t) Hashtbl.t
  val running : (int, clock) Hashtbl.t
  val ids : (string, int) Hashtbl.t
  val dead : (int * string option) list Atomic.t
  val identities : int Atomic.t
  val release_id : int -> string -> unit
  val drain : unit -> unit
  val locked : (unit -> 'a) -> 'a
  val watch : clock -> unit
  val wait : clock -> unit
  val run : clock -> unit
  val remove : clock -> unit
  val by_creation : clock list -> clock list
  val waiting_clocks : unit -> clock list
  val running_clocks : unit -> clock list
  val claim_id : clock -> string -> string
  val move_id : from:clock -> into:clock -> string -> unit
  val drop_id : clock -> unit
end

val clock_name : clock -> string
val string_of_controller : clock -> string
val set_clock_id : clock -> string -> unit

(** Logs an event under the clock's name. *)
val emit : clock -> Event.event_kind -> unit

val entry : source -> Status.entry
val members : streaming -> (Sources.key * member) list
val active_members : streaming -> (Sources.key * member) list
val passive_sources : streaming -> source list
val measures : clock -> streaming -> bool
val now : streaming -> float
val lateness : clock -> streaming -> float option
val recent_figures : streaming -> Status.figures
val stop_check : unit -> unit
val not_running : clock -> 'a
val started_streaming : clock -> streaming
val quietly : clock -> string -> (unit -> unit) -> unit
val latency : streaming -> float
val max_latency : streaming -> float
val update_figures : streaming -> (Status.figures -> Status.figures) -> unit

(** Tells the parent's streaming state that this clock's answer to "blocks"
    changed. *)
val tell_parent : clock -> bool -> unit

val interrupt : streaming -> unit

(** Returns whether this call is the one that stopped the clock. *)
val request_stop : clock -> Status.stop_reason -> bool

val is_top_level : clock -> bool
