type sync_mode = [ `Automatic | `Cpu | `Passive | `Unsynced ]
type source_type = [ `Active | `Output | `Passive ]
type owner = { kind : string; id : string }
type animator = [ `Task | `Thread ]

type failure = {
  error : exn;
  backtrace : Printexc.raw_backtrace;
  source : string option;
}

type stop_reason =
  [ `Failed of failure
  | `Global_stop
  | `Never_started
  | `No_sources
  | `Parent_stopped
  | `Requested
  | `Sync_source_ended ]

type figures = {
  ticks : int;
  producing : float;
  waiting : float;
  resting : float;
  rests : int;
  released : float;
  releases : int;
  no_worker : float;
  resets : int;
  switches : int;
  animator_changes : int;
  longest_tick : float;
  slowest_source : string option;
}

type entry = {
  id : string;
  source_type : source_type;
  activations : string list;
}

type controller = { parent : string option; owner : owner option }
type sync = { sync_source : string; pacing : string; followed : bool }
type statistics = { life : figures; recent : figures }

type t = {
  name : string;
  state : [ `Started | `Stopped of stop_reason | `Stopping ];
  sync_mode : sync_mode;
  controller : controller option;
  animator : (animator * string) option;
  sync : sync option;
  ticks : int option;
  stream_time : float option;
  lateness : float option;
  pending : entry list;
  outputs : entry list;
  active : entry list;
  passive : entry list;
  sub_clocks : t list;
  statistics : statistics option;
}

type activity = [ `Idle | `Released | `Resting | `Ticking of float | `Waiting ]

val no_figures : figures

type slowest = { mutable duration : float; mutable culprit : string option }

(** The figures accumulated since [mark]. The maxima of a span cannot be had by
    subtraction: they are those of [slowest]. *)
val figures_since : slowest:slowest -> figures -> figures -> figures

val sync_mode_of_string : string -> [> `Automatic | `Cpu | `Passive | `Unsynced ]
val or_none : string option -> string

val string_of_sync_mode :
  [< `Automatic | `Cpu | `Passive | `Unsynced ] -> string

val string_of_source_type : [< `Active | `Output | `Passive ] -> string
val string_of_animator : [< `Task | `Thread ] -> string
val string_of_owner : owner -> string
val string_of_stop_reason : stop_reason -> string

(** The status report of spec/observability.md §3. *)
val report : t list -> string

(** The source graph of spec/observability.md §4, one per clock and sub-clock.
*)
val source_graph : t list -> string
