(** The types of the library. This file has no interface, which would repeat
    every definition: each module includes its part. *)

module Status = struct
  type sync_mode = [ `Automatic | `Cpu | `Unsynced | `Passive ]
  type source_type = [ `Passive | `Active | `Output ]
  type owner = { kind : string; id : string }
  type animator = [ `Task | `Thread ]

  type failure = {
    error : exn;
    backtrace : Printexc.raw_backtrace;
    source : string option;
  }

  type stop_reason =
    [ `Never_started
    | `Requested
    | `No_sources
    | `Global_stop
    | `Sync_source_ended
    | `Parent_stopped
    | `Failed of failure ]

  type figures = {
    ticks : int;
    producing : float;
    (* Seconds spent resting ahead of the time source. *)
    resting : float;
    rests : int;
    (* Seconds spent away from a worker after a time box. *)
    released : float;
    releases : int;
    (* Seconds a rest lasted beyond its delay, waiting for a worker. *)
    no_worker : float;
    resets : int;
    switches : int;
    animator_changes : int;
    longest_tick : float;
    (* Slowest source of the longest tick. *)
    slowest_source : string option;
  }

  (* A source in a status: its id, its kind and the ids of what keeps it awake. *)
  type entry = {
    id : string;
    source_type : source_type;
    activations : string list;
  }

  type controller = { parent : string option; owner : owner option }
  type sync = { sync_source : string; pacing : string; followed : bool }
  type statistics = { life : figures; recent : figures }

  (* What a clock tells only while it has a run; [animator] is [None] for a
     passive clock. *)
  type run = {
    animator : (animator * string) option;
    sync : sync option;
    ticks : int;
    stream_time : float;
    lateness : float option;
    outputs : entry list;
    active : entry list;
    passive : entry list;
    statistics : statistics;
  }

  (* A snapshot of a clock and of its sub-clocks. *)
  type t = {
    name : string;
    state : [ `Stopped of stop_reason | `Started of run | `Stopping of run ];
    sync_mode : sync_mode;
    controller : controller option;
    pending : entry list;
    sub_clocks : t list;
  }

  (* What a clock's loop is doing; [`Ticking] carries the tick's start time. *)
  type activity = [ `Idle | `Ticking of float | `Resting | `Released ]
  type slowest = { mutable duration : float; mutable culprit : string option }
end

module Event = struct
  open Status

  type event_kind =
    | Start of {
        top_level : bool;
        controller : string option;
        sync_mode : sync_mode;
        sources : (string * [ `Passive | `Active | `Output ]) list;
        animator : (animator * string) option;
      }
    | Animator_change of { from : animator; into : animator; why : string }
    | Flap of { gap : float; lease : float }
    | Thread_lease of { lease : float }
    | Sync_source_switch of {
        from : string option;
        into : string option;
        pacing : string option;
        latency : float;
        max_latency : float;
      }
    | Stop of { reason : stop_reason; ticks : int; stream_time : float }
    | Failure of failure
    | Source_failure of {
        source : string;
        error : exn;
        backtrace : Printexc.raw_backtrace;
        handled : bool;
      }
    | Latency_warning of { lateness : float; since_last : figures }
    | Latency_reset of {
        lateness : float;
        since_last : figures;
        ticks_before : int;
        ticks_after : int;
      }
    | Long_tick of { duration : float; slowest_source : string option }
    | Park of {
        what : [ `Rest | `Release ];
        delay : float option;
        spent : float;
      }
    | Leak_warning of { activated : int; status : Status.t }
    | Id_kept of { kept : string; dropped : string }
    | Sync_source_ended of { sync_source : string }
    | Still_running of { activity : activity; slowest_source : string option }
    | Unknown_time_source of { wanted : string; used : string }
    | Time_source of { used : string }

  type event = { clock : string; kind : event_kind }
end

module State = struct
  open Status

  (* Token a source returns when it is woken up, given back to put it to sleep. *)
  type activation = < id : string >

  (* What the clock calls on a source it animates: [output] produces its frame of
     the tick, [reset] follows a latency reset. *)
  type active = < id : string ; reset : unit ; output : unit >

  type source =
    < id : string
    ; stack : Pos.t list
    ; source_type : [ `Passive | `Active of active | `Output of active ]
    ; wake_up : source -> activation
    ; sleep : activation -> unit
    ; sync_source : Sync_source.t option
    ; activations : activation list >

  (* Tables keyed on sources by physical identity, which let a source be
     collected. *)
  module Sources = Ephemeron.K1.Make (struct
    type t = source

    let equal = ( == )
    let hash = Oo.id
  end)

  type reported = { sync_source : string; source : string; stack : Pos.t list }

  (* Neither clock is a stopped clock whose sync mode fits the other. *)
  exception Conflict of { pos : Pos.t option; left : string; right : string }

  (* The unification would nest a clock inside itself. *)
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
  exception Not_running of string

  exception
    Cannot_start of {
      clock : string;
      reason :
        [ `Not_stopped | `Global_stop | `Application_not_started | `No_output ];
    }

  exception Not_a_sub_clock of { clock : string; parent : string }

  (* Raised inside a tick once the global stop is set, to unwind it. *)
  exception Stop_signal

  (* A sync source read from a source; [rank] orders the answers of a run, which
     tells the latest of two conflicting sources. *)
  type answer = { pacer : Sync_source.t; rank : int }

  type member = {
    role : [ `Output | `Active ];
    (* Set when the source is detached, so that the rest of the tick skips it. *)
    removed : bool Atomic.t;
    (* The source's answer at the last pacing point, when it reported a sync
       source. *)
    mutable sync : answer option;
  }

  type output = { sleep : unit -> unit; source : source; member : member }

  (* How the stream relates to time, replaced as a whole when the pacing changes. *)
  type pace = {
    time_source : Sync_source.time_source;
    (* The time source's reading that stands for stream time 0. *)
    offset : float;
    (* The sync source the clock paces on, if any. *)
    followed : Sync_source.t option;
  }

  (* A stretch of a run over which figures are taken: the figures and the time at
     its start, and its longest tick with that tick's slowest source. *)
  type span = {
    mutable mark : figures;
    mutable since : float;
    longest : slowest;
  }

  (* The state of one run of a clock, created at start and dropped at stop. *)
  type streaming = {
    frame_duration : float;
    (* Whether the run started with [~force], which the sub-clocks it starts
       inherit. *)
    forced : bool;
    (* Time source used while no sync source brings its own. *)
    default_time_source : Sync_source.time_source;
    (* Number of ticks completed, moved by a latency reset. *)
    ticks : int Atomic.t;
    (* Protects the four collections of sources below and the marking of a
       removal. *)
    m : Mutex.t;
    (* Woken outputs, in activation order. *)
    outputs : output list Atomic.t;
    animated : member Sources.t;
    (* The keys of [animated], which a table of ephemerons cannot list. *)
    animated_sources : source Queues.WeakQueue.t;
    passive : source Queues.WeakQueue.t;
    (* Sources detached since removals were last applied, newest first. *)
    removals : source list Atomic.t;
    (* Set when the next pacing point must recompute the pacing: an animated
       source left or a sub-clock's blocking changed. *)
    dirty : bool Atomic.t;
    (* One-shot callbacks run inside the next tick, after the sources are
       animated. *)
    on_tick : (unit -> unit) list Atomic.t;
    (* One-shot callbacks run once the next tick is counted. *)
    after_tick : (unit -> unit) list Atomic.t;
    (* True while a tick asked with [~pull] animates its sources. *)
    pulled : bool Atomic.t;
    (* What runs the clock's loop and why; [None] for a passive clock. *)
    animator : (animator * string) option Atomic.t;
    pace : pace Atomic.t;
    (* The single sync source of the clock's sources, whether or not the sync mode
       follows it. *)
    tracked : Sync_source.t option Atomic.t;
    (* Whether the tracked sync source or a sub-clock blocks, which calls for a
       thread. *)
    blocking : bool Atomic.t;
    sub_blocking : int Atomic.t;
    (* Where the loop rests and where a stop wakes it. *)
    parker : Parker.t;
    (* Interrupts the blocking wait of a time source while the loop rests in one. *)
    interrupt_wait : (unit -> unit) Atomic.t;
    (* What the loop is doing, for the status and the shutdown report. *)
    activity : activity Atomic.t;
    life : figures Atomic.t;
    (* Figures of the last complete window; [None] during the first one. *)
    recent : figures option Atomic.t;
    (* Number of sync changes seen, the source of each answer's [rank]. *)
    mutable changes_applied : int;
    (* Id of the source whose unhandled error is failing the clock. *)
    mutable failing : string option;
    (* When the clock stopped blocking, until it blocks again or its lease ends. *)
    mutable unblocked_at : float option;
    (* When the clock last stopped blocking, kept to detect a flap. *)
    mutable last_unblocked : float option;
    (* Whether the clock keeps its thread for a lease after it stops blocking. *)
    mutable leased : bool;
    (* When the loop last took its worker, the start of its time box. *)
    mutable worker_since : float;
    mutable last_long_tick : float;
    (* The span since the last latency warning or reset was logged. *)
    warning : span;
    (* The current statistics window. *)
    window : span;
  }

  (* A clock's lifecycle: a run exists exactly while the clock is started or
     stopping, and a stop reason exactly while it is stopping or stopped. *)
  type lifecycle =
    [ `Stopped of stop_reason
    | `Started of streaming
    | `Stopping of streaming * stop_reason ]

  (* A clock. Handles designate it through the unifier, and a merge moves all of
     them to the surviving clock. *)
  type clock = {
    identity : int;
    (* The id set by the user or taken at start, with the logger named after it;
       [None] before that. *)
    id : (string * Log.t) option Atomic.t;
    sync_mode : sync_mode;
    (* The clock that ticks this passive clock in the ticks where its controller
       does not. *)
    parent : t option Atomic.t;
    (* The operator controlling this passive clock; clocks with different owners
       never merge. *)
    owner : owner option;
    stack : Pos.t list Atomic.t;
    (* The lifecycle, changed under the transition lock. *)
    state : lifecycle Atomic.t;
    (* Attached sources waiting for activation, in attachment order. *)
    pending : source list Atomic.t;
    subs : sub list Atomic.t;
    (* Handlers of source errors; with none, a source error fails the clock. *)
    error_handlers : (exn -> Printexc.raw_backtrace -> unit) list Atomic.t;
    (* Raised while a tick or an activation of a passive clock runs: it rejects a
       concurrent one and leaves the wind-down to the one in progress. *)
    ticking : bool Atomic.t;
    (* Sources activated since creation, for the leak warning. *)
    activated : int Atomic.t;
    self : t option Atomic.t;
  }

  and sub = { sub : t; registrants : int }
  and t = clock Unifier.t
end
