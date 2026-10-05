type event_kind =
  | Start of {
      top_level : bool;
      controller : string option;
      sync_mode : Status.sync_mode;
      sources : (string * [ `Active | `Output | `Passive ]) list;
      animator : (Status.animator * string) option;
    }
  | Animator_change of {
      from : Status.animator;
      into : Status.animator;
      why : string;
    }
  | Flap of { gap : float; lease : float }
  | Thread_lease of { lease : float }
  | Sync_source_switch of {
      from : string option;
      into : string option;
      pacing : string option;
      latency : float;
      max_latency : float;
    }
  | Stop of { reason : Status.stop_reason; ticks : int; stream_time : float }
  | Failure of Status.failure
  | Source_failure of {
      source : string;
      error : exn;
      backtrace : Printexc.raw_backtrace;
      handled : bool;
    }
  | Latency_warning of { lateness : float; since_last : Status.figures }
  | Latency_reset of {
      lateness : float;
      since_last : Status.figures;
      ticks_before : int;
      ticks_after : int;
    }
  | Long_tick of { duration : float; slowest_source : string option }
  | Park of { what : [ `Release | `Rest ]; delay : float option; spent : float }
  | Leak_warning of { activated : int; status : Status.t }
  | Id_kept of { kept : string; dropped : string }
  | Sync_source_ended of { sync_source : string }
  | Still_running of {
      activity : Status.activity;
      slowest_source : string option;
    }
  | Unknown_time_source of { wanted : string; used : string }

type event = { clock : string; kind : event_kind }

val on_event : (event -> unit) -> unit

(** Logs the event and hands it to the subscribers. *)
val emit : log:Log.t -> clock:string -> event_kind -> unit

(** Whether a debug event is worth building. *)
val wants_debug : log:Log.t -> bool
