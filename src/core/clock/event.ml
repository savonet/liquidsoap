open Settings
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
      what : [ `Rest | `Release | `Wait ];
      delay : float option;
      spent : float;
    }
  | Leak_warning of { activated : int; status : Status.t }
  | Id_kept of { kept : string; dropped : string }
  | Sync_source_ended of { sync_source : string }
  | Still_running of { activity : activity; slowest_source : string option }
  | Unknown_time_source of { wanted : string; used : string }

type event = { clock : string; kind : event_kind }

let string_of_activity = function
  | `Idle -> "idle"
  | `Ticking since ->
      Printf.sprintf "in a tick for %.02fs" (Duppy.time () -. since)
  | `Resting -> "resting"
  | `Waiting -> "waiting"
  | `Released -> "released"

let string_of_breakdown (figures : figures) =
  Printf.sprintf
    "%d ticks, producing %.03fs, waiting %.03fs, resting %.03fs, released \
     %.03fs, no worker %.03fs, slowest source: %s"
    figures.ticks figures.producing figures.waiting figures.resting
    figures.released figures.no_worker
    (or_none figures.slowest_source)

let level_and_text = function
  | Start { top_level; controller; sync_mode; sources; animator } ->
      ( 3,
        Printf.sprintf "Starting %s clock%s, sync: %s, sources: %s%s"
          (if top_level then "top-level" else "nested")
          (match controller with
            | Some controller -> ", controlled by " ^ controller
            | None -> "")
          (string_of_sync_mode sync_mode)
          (String.concat ", "
             (List.map
                (fun (id, source_type) ->
                  Printf.sprintf "%s (%s)" id
                    (string_of_source_type source_type))
                sources))
          (match animator with
            | Some (animator, why) ->
                Printf.sprintf ", animated by a %s (%s)"
                  (string_of_animator animator)
                  why
            | None -> "") )
  | Animator_change { from; into; why } ->
      ( 3,
        Printf.sprintf "Animator changes from %s to %s (%s)"
          (string_of_animator from) (string_of_animator into) why )
  | Sync_source_switch { from; into; pacing; latency; max_latency } ->
      ( 3,
        Printf.sprintf
          "Sync source changes from %s to %s (%s), latency: %.02fs, maximum: \
           %.02fs"
          (or_none from) (or_none into) (or_none pacing) latency max_latency )
  | Stop { reason; ticks; stream_time } ->
      ( 3,
        Printf.sprintf "Stopped: %s, after %d ticks (%.02fs)"
          (string_of_stop_reason reason)
          ticks stream_time )
  | Failure { error; backtrace; source } ->
      ( 1,
        Printf.sprintf "Clock failed%s: %s\n%s"
          (match source with
            | Some source -> " in source " ^ source
            | None -> "")
          (Printexc.to_string error)
          (Printexc.raw_backtrace_to_string backtrace) )
  | Source_failure { source; error; backtrace; handled } ->
      ( 2,
        Printf.sprintf "Source %s failed (%s): %s\n%s" source
          (if handled then "handled" else "no handler")
          (Printexc.to_string error)
          (Printexc.raw_backtrace_to_string backtrace) )
  | Latency_warning { lateness; since_last } ->
      ( 2,
        Printf.sprintf "Late by %.02fs. Since the last warning: %s" lateness
          (string_of_breakdown since_last) )
  | Latency_reset { lateness; since_last; ticks_before; ticks_after } ->
      ( 2,
        Printf.sprintf
          "Late by %.02fs: resetting from tick %d to tick %d. Since the last \
           warning: %s"
          lateness ticks_before ticks_after
          (string_of_breakdown since_last) )
  | Long_tick { duration; slowest_source } ->
      ( 3,
        Printf.sprintf "A tick took %.03fs, slowest source: %s" duration
          (or_none slowest_source) )
  | Park { what; delay; spent } ->
      ( 5,
        Printf.sprintf "%s%s: %.03fs"
          (match what with
            | `Rest -> "Rest"
            | `Release -> "Release"
            | `Wait -> "Wait")
          (match delay with
            | Some delay -> Printf.sprintf " for %.03fs" delay
            | None -> "")
          spent )
  | Leak_warning { activated; status } ->
      ( 2,
        Printf.sprintf
          "%d sources were activated on this clock: sources may be leaking.\n%s"
          activated (Status.report [status]) )
  | Id_kept { kept; dropped } ->
      (4, Printf.sprintf "Keeping id %s over %s after unification" kept dropped)
  | Sync_source_ended { sync_source } ->
      (3, Printf.sprintf "Sync source %s ended" sync_source)
  | Still_running { activity; slowest_source } ->
      ( 1,
        Printf.sprintf "Still running at shutdown: %s, slowest source: %s"
          (string_of_activity activity)
          (or_none slowest_source) )
  | Unknown_time_source { wanted; used } ->
      (2, Printf.sprintf "Unknown time source %s, using %s" wanted used)

let subscribers : (event -> unit) list Atomic.t = Atomic.make []
let on_event fn = push subscribers fn

let emit ~clock kind =
  let level, text = level_and_text kind in
  log#f level "[%s] %s" clock text;
  List.iter (fun fn -> fn { clock; kind }) (Atomic.get subscribers)

let wants_debug () = log#active 5 || Atomic.get subscribers <> []
