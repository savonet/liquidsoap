open Settings
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

let role (source : source) =
  match source#source_type with
    | `Passive -> `Passive
    | `Active _ -> `Active
    | `Output _ -> `Output

let active (source : source) =
  match source#source_type with
    | `Active active | `Output active -> Some active
    | `Passive -> None

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

let rec first count = function
  | value :: rest when count > 0 -> value :: first (count - 1) rest
  | _ -> []

let sync_error ~clock reporting =
  Sync_error
    {
      clock;
      reported =
        List.map
          (fun (source, (sync : Sync_source.t)) ->
            {
              sync_source = sync.name;
              source = source#id;
              stack = first 3 source#stack;
            })
          reporting;
    }

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

let string_of_reported { sync_source; source; stack } =
  Printf.sprintf "  %s, from %s%s" sync_source source
    (String.concat ""
       (List.map (fun pos -> "\n    " ^ Pos.to_string pos) stack))

let () =
  Printexc.register_printer (function
    | Conflict { left; right } ->
        Some
          (Printf.sprintf
             "Clocks %s and %s cannot be unified: a source cannot belong to \
              two clocks."
             left right)
    | Loop { left; right } ->
        Some
          (Printf.sprintf
             "Clocks %s and %s cannot be unified: one is nested in the other."
             left right)
    | Controller_conflict { left; left_controller; right; right_controller } ->
        Some
          (Printf.sprintf
             "Clocks %s (controlled by %s) and %s (controlled by %s) cannot be \
              unified."
             left left_controller right right_controller)
    | Sync_error { clock; reported } ->
        Some
          (Printf.sprintf
             "Clock %s has multiple synchronization sources. Do you need to \
              set self_sync=false?\n\
              Sync sources:\n\
              %s"
             clock
             (String.concat "\n" (List.map string_of_reported reported)))
    | Not_running clock ->
        Some (Printf.sprintf "Clock %s is not running." clock)
    | Cannot_start { clock; reason } ->
        Some
          (Printf.sprintf "Clock %s cannot start: %s." clock
             (match reason with
               | `Not_stopped -> "it is not stopped"
               | `Global_stop -> "the application is stopping"
               | `Application_not_started -> "the application has not started"
               | `No_output -> "it has no output"))
    | Not_a_sub_clock { clock; parent } ->
        Some
          (Printf.sprintf "Clock %s is not a passive clock whose parent is %s."
             clock parent)
    | _ -> None)

let numbered ~formatter number pos error =
  Runtime.error_header ~formatter number pos;
  Format.fprintf formatter "%s@]@." (Printexc.to_string error);
  true

let () =
  Runtime.on_error_print (fun ~formatter error ->
      match error with
        | Conflict { pos } -> numbered ~formatter 10 pos error
        | Loop { pos } -> numbered ~formatter 11 pos error
        | Controller_conflict { pos } -> numbered ~formatter 16 pos error
        | Sync_error _ -> numbered ~formatter 17 None error
        | _ -> false)

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
type span = { mutable mark : figures; mutable since : float; longest : slowest }

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
  tick_slowest : slowest;
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

(** Every state transition and every unification run under this one lock. It is
    never held across a tick, which can resume on another thread.

    The lock belongs to the thread that took it: a callback that waits under it
    has to come back on that thread, hence the blocking section. *)
module Transition = struct
  let m = Mutex.create ()
  let holder = Atomic.make (-1)
  let held () = Atomic.get holder = Thread.id (Thread.self ())

  (* Runs [fn] under the lock, directly when the caller already holds it. *)
  let run fn =
    if held () then fn ()
    else
      Mutex.protect m (fun () ->
          Atomic.set holder (Thread.id (Thread.self ()));
          Fun.protect
            ~finally:(fun () -> Atomic.set holder (-1))
            (fun () -> Duppy.blocking fn))
end

(* Set once the application stops: clocks stop, none starts, and failures go
   unreported. *)
let global_stop = Atomic.make false

(* Set at application start; before it only a forced start succeeds. *)
let application_started = Atomic.make false
let window_length = 10.
let get = Unifier.deref

(* The handle a clock made for itself: it follows merges like any other. *)
let handle c = Option.get (Atomic.get c.self)

(* A handle never designates a clock again once it left it: if [a] still
   designates the same one after [b] was read, they differed at that moment. *)
let rec equal a b =
  let clock_a = get a in
  let clock_b = get b in
  clock_a == clock_b || (get a != clock_a && equal a b)

let compare a b = Int.compare (get a).identity (get b).identity
let lifecycle c = Atomic.get c.state

let state c =
  match lifecycle c with
    | `Stopped _ -> `Stopped
    | `Started _ -> `Started
    | `Stopping _ -> `Stopping

let streaming c =
  match lifecycle c with
    | `Started st | `Stopping (st, _) -> Some st
    | `Stopped _ -> None

let clock_id c = Option.map fst (Atomic.get c.id)
let running c = state c = `Started && not (Atomic.get global_stop)
let stream_time st = float (Atomic.get st.ticks) *. st.frame_duration

(** The registry never keeps a stopped clock alive: [waiting] is weak, and a
    collected clock leaves it through [dead], since a finaliser cannot lock. *)
module Registry = struct
  let m = Mutex.create ()

  (* Stopped top-level clocks, which the start pass examines. *)
  let waiting : (int, clock Weak.t) Hashtbl.t = Hashtbl.create 16

  (* Started top-level clocks, which this table keeps alive. *)
  let running : (int, clock) Hashtbl.t = Hashtbl.create 16
  let ids : (string, int) Hashtbl.t = Hashtbl.create 16

  (* Collected clocks whose entries are still in the tables. *)
  let dead : (int * string option) list Atomic.t = Atomic.make []
  let identities = Atomic.make 0

  let release_id identity id =
    if Hashtbl.find_opt ids id = Some identity then Hashtbl.remove ids id

  let drain () =
    List.iter
      (fun (identity, id) ->
        Hashtbl.remove waiting identity;
        Option.iter (release_id identity) id)
      (Atomic.exchange dead [])

  (* Runs [fn] under the registry lock, on drained tables. *)
  let locked fn =
    Mutex.protect m (fun () ->
        drain ();
        fn ())

  (* Queues a clock's entries for removal when it is collected. *)
  let watch c = Gc.finalise (fun c -> push dead (c.identity, clock_id c)) c

  let wait c =
    locked (fun () ->
        Hashtbl.remove running c.identity;
        let weak = Weak.create 1 in
        Weak.set weak 0 (Some c);
        Hashtbl.replace waiting c.identity weak)

  let run c =
    locked (fun () ->
        Hashtbl.remove waiting c.identity;
        Hashtbl.replace running c.identity c)

  (* Takes a clock out, when it merges away or gets a parent. *)
  let remove c =
    locked (fun () ->
        Hashtbl.remove waiting c.identity;
        Hashtbl.remove running c.identity)

  let by_creation clocks =
    List.sort (fun a b -> Int.compare a.identity b.identity) clocks

  let waiting_clocks () =
    locked (fun () ->
        by_creation
          (Hashtbl.fold
             (fun _ weak clocks ->
               match Weak.get weak 0 with
                 | Some c -> c :: clocks
                 | None -> clocks)
             waiting []))

  let running_clocks () =
    locked (fun () ->
        by_creation (Hashtbl.fold (fun _ c clocks -> c :: clocks) running []))

  (* Reserves [wanted] for the clock, with a numeric suffix when it is taken,
     and returns the id reserved. *)
  let claim_id c wanted =
    locked (fun () ->
        let rec free attempt =
          let candidate =
            if attempt = 1 then wanted
            else Printf.sprintf "%s.%d" wanted attempt
          in
          match Hashtbl.find_opt ids candidate with
            | Some identity when identity <> c.identity -> free (attempt + 1)
            | _ -> candidate
        in
        Option.iter (release_id c.identity) (clock_id c);
        let id = free 1 in
        Hashtbl.replace ids id c.identity;
        id)

  let move_id ~from ~into id =
    locked (fun () ->
        release_id from.identity id;
        Hashtbl.replace ids id into.identity)

  let drop_id c =
    locked (fun () -> Option.iter (release_id c.identity) (clock_id c))
end

(* The pending source that names an unnamed clock: an output first, then an
   active source, then any. *)
let significant_pending c =
  let pending = Atomic.get c.pending in
  let first source_type =
    List.find_opt (fun (s : source) -> role s = source_type) pending
  in
  List.find_map first [`Output; `Active; `Passive]

(* The clock's id, or the name it would take at start. *)
let clock_name c =
  match clock_id c with
    | Some id -> id
    | None -> (
        match significant_pending c with
          | Some source -> source#id
          | None -> "generic")

let string_of_controller c =
  match (c.owner, Atomic.get c.parent) with
    | Some owner, _ -> Status.string_of_owner owner
    | None, Some parent -> clock_name (get parent)
    | None, None -> "none"

let set_clock_id c id =
  if clock_id c <> Some id then begin
    let id = Registry.claim_id c id in
    Atomic.set c.id (Some (id, Log.make ["clock"; id]))
  end

let logger c =
  match Atomic.get c.id with
    | Some (_, log) -> log
    | None -> Log.make ["clock"; clock_name c]

let emit c kind = Event.emit ~log:(logger c) ~clock:(clock_name c) kind
let wants_debug c = Event.wants_debug ~log:(logger c)

let entry (source : source) =
  {
    Status.id = source#id;
    source_type = role source;
    activations = List.map (fun (a : activation) -> a#id) source#activations;
  }

(* Ephemeron tables cannot be walked, so the sources are also kept in a weak
   queue. *)
let members st =
  Mutex.protect st.m (fun () ->
      List.filter_map
        (fun source ->
          Option.map
            (fun member -> (source, member))
            (Sources.find_opt st.animated source))
        (Queues.WeakQueue.elements st.animated_sources))

let active_members st =
  List.filter (fun (_, member) -> member.role = `Active) (members st)

let passive_sources st = Queues.WeakQueue.elements st.passive

(* Whether the clock compares its stream to a time source; a self-paced sync
   source, [none] and [passive] leave the pace to something else. *)
let measures c st =
  match c.sync_mode with
    | `Cpu -> true
    | `Automatic -> (
        match (Atomic.get st.pace).followed with
          | Some { pacing = `Self_paced } -> false
          | _ -> true)
    | `Unsynced | `Passive -> false

(* The time source's reading, on the stream's time scale. *)
let now st =
  let pace = Atomic.get st.pace in
  pace.time_source.now () -. pace.offset

let lateness c st =
  if measures c st then Some (now st -. stream_time st) else None

let span_figures span figures =
  figures_since ~slowest:span.longest span.mark figures

let restart_span span ~mark ~since =
  span.mark <- mark;
  span.since <- since;
  span.longest.duration <- 0.;
  span.longest.culprit <- None

let recent_figures st =
  match Atomic.get st.recent with
    | Some figures -> figures
    | None -> span_figures st.window (Atomic.get st.life)

(* Unwinds the calling tick once the global stop is set. *)
let stop_check () = if Atomic.get global_stop then raise Stop_signal

let not_running c =
  stop_check ();
  raise (Not_running (clock_name c))

let started_streaming c =
  match lifecycle c with `Started st -> st | _ -> not_running c

(* A source may register its next callback from inside the tick that a stop
   landed in: the callback is accepted, and dropped by the wind-down. *)
let running_streaming c =
  match streaming c with Some st -> st | None -> not_running c

(* Runs [fn] and logs its error, for cleanup steps that must all run. *)
let quietly c what fn =
  try fn ()
  with error ->
    (logger c)#severe "Error while %s: %s" what (Printexc.to_string error)

(* Lead the stream builds before the clock rests: the followed sync source's
   value, or the setting. *)
let latency st =
  match (Atomic.get st.pace).followed with
    | Some { pacing = `Timed { latency = Some latency } } -> latency
    | _ -> conf_latency#get

let max_latency st =
  match (Atomic.get st.pace).followed with
    | Some { pacing = `Timed { max_latency = Some max_latency } } -> max_latency
    | _ -> conf_max_latency#get

(* How much of the clock's thread lease is left: spec/pacing.md §8. *)
let lease_left st =
  match st.unblocked_at with
    | Some since when st.leased ->
        Float.max 0. (conf_thread_lease#get -. (Duppy.time () -. since))
    | _ -> 0.

let update_figures st (fn : figures -> figures) =
  Atomic.set st.life (fn (Atomic.get st.life))

(* Tells the parent's run that this clock started or stopped blocking. *)
let tell_parent c blocking =
  match
    Option.bind (Atomic.get c.parent) (fun parent -> streaming (get parent))
  with
    | Some parent ->
        ignore
          (Atomic.fetch_and_add parent.sub_blocking
             (if blocking then 1 else -1));
        Atomic.set parent.dirty true
    | None -> ()

(* Precedence of stop reasons: of two given to a stopping clock, the lower rank
   stays. *)
let reason_rank : stop_reason -> int = function
  | `Failed _ -> 0
  | `Global_stop -> 1
  | `Never_started -> 2
  | `Requested -> 3
  | `No_sources -> 4
  | `Sync_source_ended -> 5
  | `Parent_stopped -> 6

(* Wakes a loop that rests, in a park or in a time source's wait. *)
let interrupt st =
  Parker.unpark st.parker;
  (Atomic.get st.interrupt_wait) ()

(* Moves a started clock to stopping with [reason], and returns whether this
   call is the one that did. *)
let request_stop c reason =
  Transition.run (fun () ->
      match lifecycle c with
        | `Started st ->
            Atomic.set c.state (`Stopping (st, reason));
            true
        | `Stopping (st, current) ->
            if reason_rank reason < reason_rank current then
              Atomic.set c.state (`Stopping (st, reason));
            false
        | `Stopped _ -> false)

(* Whether the clock has no parent, which puts it in the registry. *)
let is_top_level c = Atomic.get c.parent = None
