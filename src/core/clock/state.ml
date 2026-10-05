open Settings
open Status

type activation = < id : string >
type active = < id : string ; reset : unit ; output : unit >

type source =
  < id : string
  ; stack : Pos.t list
  ; source_type : [ `Passive | `Active of active | `Output of active ]
  ; wake_up : source -> activation
  ; sleep : activation -> unit
  ; sync_source : Sync_source.t option
  ; on_sync_source : (Sync_source.t option -> unit) -> unit -> unit
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

module Sources = Ephemeron.K1.Make (struct
  type t = source

  let equal = ( == )
  let hash = Oo.id
end)

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
exception Not_running of string

exception
  Cannot_start of {
    clock : string;
    reason :
      [ `Not_stopped | `Global_stop | `Application_not_started | `No_output ];
  }

exception Not_a_sub_clock of { clock : string; parent : string }
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

type member = {
  role : [ `Output | `Active ];
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
  animator : (animator * string) option Atomic.t;
  pace : pace Atomic.t;
  tracked : Sync_source.t option Atomic.t;
  blocking : bool Atomic.t;
  sub_blocking : int Atomic.t;
  parker : Parker.t;
  interrupt_wait : (unit -> unit) Atomic.t;
  activity : activity Atomic.t;
  life : figures Atomic.t;
  recent : figures option Atomic.t;
  mutable changes_applied : int;
  mutable failing : string option;
  mutable unblocked_at : float option;
  mutable last_unblocked : float option;
  mutable leased : bool;
  mutable worker_since : float;
  mutable tick_waited : float;
  tick_slowest : slowest;
  mutable last_warning : float;
  mutable last_long_tick : float;
  mutable warning_mark : figures;
  warning_slowest : slowest;
  mutable window_mark : figures;
  mutable window_started : float;
  window_slowest : slowest;
}

type clock = {
  identity : int;
  id : string option Atomic.t;
  sync_mode : sync_mode;
  parent : t option Atomic.t;
  owner : owner option;
  stack : Pos.t list Atomic.t;
  state : [ `Stopped | `Started | `Stopping ] Atomic.t;
  stop_reason : stop_reason Atomic.t;
  pending : source list Atomic.t;
  subs : sub list Atomic.t;
  error_handlers : (exn -> Printexc.raw_backtrace -> unit) list Atomic.t;
  streaming : streaming option Atomic.t;
  ticking : bool Atomic.t;
  activated : int Atomic.t;
  self : t option Atomic.t;
  log : Log.t option Atomic.t;
}

and sub = { sub : t; registrants : int }
and t = clock Unifier.t

(** Every state transition and every unification run under this one lock. It is
    never held across a tick, which can resume on another thread. *)
module Transition = struct
  let m = Mutex.create ()
  let holder = Atomic.make (-1)
  let held () = Atomic.get holder = Thread.id (Thread.self ())

  let run fn =
    if held () then fn ()
    else
      Mutex.protect m (fun () ->
          Atomic.set holder (Thread.id (Thread.self ()));
          Fun.protect ~finally:(fun () -> Atomic.set holder (-1)) fn)
end

let global_stop = Atomic.make false
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
let state c = Atomic.get c.state
let running c = state c = `Started && not (Atomic.get global_stop)
let stream_time st = float (Atomic.get st.ticks) *. st.frame_duration

(** The registry never keeps a stopped clock alive: [waiting] is weak, and a
    collected clock leaves it through [dead], since a finaliser cannot lock. *)
module Registry = struct
  let m = Mutex.create ()
  let waiting : (int, clock Weak.t) Hashtbl.t = Hashtbl.create 16
  let running : (int, clock) Hashtbl.t = Hashtbl.create 16
  let ids : (string, int) Hashtbl.t = Hashtbl.create 16
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

  let locked fn =
    Mutex.protect m (fun () ->
        drain ();
        fn ())

  let watch c = Gc.finalise (fun c -> push dead (c.identity, Atomic.get c.id)) c

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
        Option.iter (release_id c.identity) (Atomic.get c.id);
        let id = free 1 in
        Hashtbl.replace ids id c.identity;
        id)

  let move_id ~from ~into id =
    locked (fun () ->
        release_id from.identity id;
        Hashtbl.replace ids id into.identity)

  let drop_id c =
    locked (fun () -> Option.iter (release_id c.identity) (Atomic.get c.id))
end

let significant_pending c =
  let pending = Atomic.get c.pending in
  let first source_type =
    List.find_opt (fun (s : source) -> role s = source_type) pending
  in
  List.find_map first [`Output; `Active; `Passive]

let clock_name c =
  match Atomic.get c.id with
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
  if Atomic.get c.id <> Some id then begin
    Atomic.set c.id (Some (Registry.claim_id c id));
    Atomic.set c.log None
  end

(* A clock logs under its own name. The logger is kept once the name is an id,
   which only [set_clock_id] and a merge change. *)
let logger c =
  match Atomic.get c.log with
    | Some log -> log
    | None ->
        let log = Log.make ["clock"; clock_name c] in
        if Atomic.get c.id <> None then Atomic.set c.log (Some log);
        log

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

let measures c st =
  match c.sync_mode with
    | `Cpu -> true
    | `Automatic -> (
        match (Atomic.get st.pace).followed with
          | Some { pacing = `Self_paced } -> false
          | _ -> true)
    | `Unsynced | `Passive -> false

let now st =
  let pace = Atomic.get st.pace in
  pace.time_source.now () -. pace.offset

let lateness c st =
  if measures c st then Some (now st -. stream_time st) else None

let recent_figures st =
  match Atomic.get st.recent with
    | Some figures -> figures
    | None ->
        figures_since ~slowest:st.window_slowest st.window_mark
          (Atomic.get st.life)

let stop_check () = if Atomic.get global_stop then raise Stop_signal

let not_running c =
  stop_check ();
  raise (Not_running (clock_name c))

let started_streaming c =
  match (state c, Atomic.get c.streaming) with
    | `Started, Some st -> st
    | _ -> not_running c

(* A source may register its next callback from inside the tick that a stop
   landed in: the callback is accepted, and dropped by the wind-down. *)
let running_streaming c =
  match Atomic.get c.streaming with Some st -> st | None -> not_running c

let quietly c what fn =
  try fn ()
  with error ->
    (logger c)#severe "Error while %s: %s" what (Printexc.to_string error)

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

let tell_parent c blocking =
  match Option.map get (Atomic.get c.parent) with
    | Some { streaming } -> (
        match Atomic.get streaming with
          | Some parent ->
              ignore
                (Atomic.fetch_and_add parent.sub_blocking
                   (if blocking then 1 else -1));
              Atomic.set parent.dirty true
          | None -> ())
    | None -> ()

let reason_rank : stop_reason -> int = function
  | `Failed _ -> 0
  | `Global_stop -> 1
  | `Never_started -> 2
  | `Requested -> 3
  | `No_sources -> 4
  | `Sync_source_ended -> 5
  | `Parent_stopped -> 6

let interrupt st =
  Parker.unpark st.parker;
  (Atomic.get st.interrupt_wait) ()

(* Returns whether this call is the one that stopped the clock. *)
let request_stop c reason =
  Transition.run (fun () ->
      if Atomic.compare_and_set c.state `Started `Stopping then begin
        Atomic.set c.stop_reason reason;
        true
      end
      else begin
        if
          state c = `Stopping
          && reason_rank reason < reason_rank (Atomic.get c.stop_reason)
        then Atomic.set c.stop_reason reason;
        false
      end)

let is_top_level c = Atomic.get c.parent = None
