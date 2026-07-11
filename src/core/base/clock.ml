(*****************************************************************************

  Liquidsoap, a programmable stream generator.
  Copyright 2003-2026 Savonet team

  This program is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details, fully stated in the COPYING
  file at the root of the liquidsoap distribution.

  You should have received a copy of the GNU General Public License
  along with this program; if not, write to the Free Software
  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301  USA

 *****************************************************************************)

(* Clocks animate the sources attached to them by performing regular ticks:
   each tick asks every animated source to produce one frame of data. Clocks
   are the only entities driving data production.

   The public type [t] is a unifier handle ([clock Unifier.t]) pointing to a
   [clock] record. Two clocks can be unified: after [unify], both handles
   dereference to a single shared record. Naming conventions in this file:
   - [c] is a handle of type [t],
   - [clock] is a dereferenced [clock] record,
   - [params] is the [active_params] of a started clock,
   - functions prefixed with [_] operate on dereferenced values ([clock] or
     [active_params]); their unprefixed counterparts take handles.

   A clock is in one of three states:
   - [`Stopped]: not ticking. The record's [sync] field gives the sync mode
     to use once started.
   - [`Started params]: ticking, either animated by its own thread (see
     [_clock_thread]) or externally for passive clocks (see [tick]).
   - [`Stopping params]: stop was requested, the clock thread winds down and
     transitions to [`Stopped] at the end of the current tick.

   Sources are attached via [attach], which places them in
   [pending_activations]. When the clock starts (and at the beginning of every
   tick), pending sources are activated and dispatched to [outputs],
   [active_sources] or [passive_sources] according to their source type.
   Outputs and active sources are animated on each tick; passive sources only
   produce data when pulled by a downstream source. *)

include Clock_base

(* {1 Sync modes} *)

type active_sync_mode = [ `Automatic | `CPU | `Unsynced | `Passive ]
type sync_mode = [ active_sync_mode | `Stopping | `Stopped ]

let string_of_sync_mode = function
  | `Stopped -> "stopped"
  | `Stopping -> "stopping"
  | `Automatic -> "auto"
  | `CPU -> "cpu"
  | `Unsynced -> "none"
  | `Passive -> "passive"

let active_sync_mode_of_string = function
  | "auto" -> `Automatic
  | "cpu" -> `CPU
  | "none" -> `Unsynced
  | "passive" -> `Passive
  | _ -> raise Not_found

(* {1 Configuration} *)

let log = Log.make ["clock"]

let conf_clock =
  Dtools.Conf.void ~p:(Configure.conf#plug "clock") "Clock settings"

let conf_log_delay =
  Dtools.Conf.float
    ~p:(conf_clock#plug "log_delay")
    ~d:1. "How often (in seconds) we should notify latency issues."

let conf_log_delay_threshold =
  Dtools.Conf.float
    ~p:(conf_clock#plug "log_delay_threshold")
    ~d:0.2 "Notify latency issues after delay exceeds this threshold."

let conf_max_latency =
  Dtools.Conf.float
    ~p:(conf_clock#plug "max_latency")
    ~d:60. "Maximum latency in seconds"
    ~comments:
      [
        "If the latency gets higher than this value, the outputs will be reset,";
        "instead of trying to catch it up second by second.";
        "The reset is typically only useful to reconnect icecast mounts.";
      ]

let conf_preferred =
  Dtools.Conf.string ~d:"posix"
    ~p:(conf_clock#plug "preferred")
    "Preferred clock implementation. One if: \"posix\" or \"ocaml\"."

let conf_latency =
  Dtools.Conf.float
    ~p:(conf_clock#plug "latency")
    ~d:0.1
    "How much time ahead (in seconds) we should be until we let the streaming \
     loop rest."
    ~comments:
      [
        "Once we have computed the given amount of time time in advance,";
        "we wait until re-starting the streaming loop.";
      ]

let conf_leak_warning =
  Dtools.Conf.int
    ~p:(conf_clock#plug "leak_warning")
    ~d:50 "Number of sources at which a leak warning should be issued."

(* {1 Time implementations} *)

let time_implementation () =
  try Hashtbl.find Liq_time.implementations conf_preferred#get
  with Not_found -> Liq_time.unix

let unconstrained_time : Liq_time.implementation =
  let module U = (val Liq_time.unix : Liq_time.T) in
  (module struct
    include U

    let time () = U.of_float 0.
    let sleep_until _ = ()
  end)

let () =
  Lifecycle.on_init ~name:"Clock initialization" (fun () ->
      let module Time = (val time_implementation () : Liq_time.T) in
      log#important "Using %s implementation for latency control"
        Time.implementation)

(* {1 Types} *)

module Pos = Liquidsoap_lang.Pos
module Unifier = Liquidsoap_lang.Unifier

(* Current sync source of a started clock, along with the latency parameters
   it imposes on the streaming loop. *)
type sync_source_state = {
  sync_source : sync_source;
  latency : float;
  max_latency : float;
}

(* Streaming state of a started clock. *)
type active_params = {
  sync : active_sync_mode;
  (* Current sync source, if any. When set, the clock is in self-sync mode:
     the sync source controls the pace of the streaming loop. *)
  mutable current_sync_source : sync_source_state option;
  (* All sync sources currently declared by the clock's sources. At most one
     unique sync source is allowed at any given time (see
     [_update_sync_source_entries]). *)
  mutable sync_source_entries : sync_source_entry list;
  log : Log.t;
  (* Time implementation used for latency control. Re-anchored whenever the
     sync source changes so that [time () = 0] at tick [0]. *)
  mutable time_implementation : Liq_time.implementation;
  log_delay : float;
  log_delay_threshold : float;
  frame_duration : float;
  last_catchup_log : float Atomic.t;
  outputs : (activation * source) Queue.t;
  active_sources : source WeakQueue.t;
  passive_sources : source WeakQueue.t;
  (* Callbacks deregistering the [on_sync_source_change] handlers installed
     when each source was activated. Flushed when the clock stops; entries
     are also removed when a source is detached. *)
  sync_source_deregisters : (source * (unit -> unit)) Queue.t;
  on_tick : (unit -> unit) Queue.t;
  after_tick : (unit -> unit) Queue.t;
  ticks : int Atomic.t;
}

type state =
  [ `Stopping of active_params | `Started of active_params | `Stopped ]

(* The clock record is the single point of unification: handles of type [t]
   dereference to it and unifying two clocks merges their records into one.
   All other fields hold plain values. *)
type clock = {
  (* Unique, monotonically increasing identity, used to compare and
     deduplicate clocks. *)
  unique_id : int;
  id : string option Atomic.t;
  (* The clock's sync mode, i.e. how it controls latency once started.
     Immutable: unification requires compatible sync modes and keeps the
     record of the started (or about-to-start) clock. *)
  sync : active_sync_mode;
  (* The entity animating the clock: another clock (for sub-clocks, e.g.
     crossfade child clocks), the clock's own thread once started, or [`None]
     when the clock is not animated yet. *)
  controller : controller Atomic.t;
  stack : Pos.t list Atomic.t;
  state : state Atomic.t;
  (* Sources attached to the clock but not yet activated. Activation happens
     when the clock starts and at the beginning of each tick. *)
  pending_activations : source Queue.t;
  (* Clocks animated by this clock: they are ticked as part of this clock's
     tick (unless already ticked during it, e.g. by an operator pulling data
     from them) and stopped when this clock stops. *)
  sub_clocks : t Queue.t;
  on_error : (exn -> Printexc.raw_backtrace -> unit) Queue.t;
}

and t = clock Unifier.t
and controller = [ `None | `Clock of t | `Other of string * < id : string > ]

let clock_counter = Atomic.make 0

exception Has_stopped

(* {1 State accessors} *)

let string_of_state = function
  | `Stopping _ -> "stopping"
  | `Started _ -> "started"
  | `Stopped -> "stopped"

(* Return the active params of a clock that is started or stopping. *)
let _params clock =
  match Atomic.get clock.state with
    | `Started params | `Stopping params -> Some params
    | `Stopped -> None

(* When a clock has no explicit id, it is derived from the most significant
   pending source: outputs first, then active sources, then passive ones. *)
let meaningful_pending_id pending =
  match
    List.sort
      (fun s s' ->
        match (s#source_type, s'#source_type) with
          | `Output _, `Output _ -> 0
          | `Output _, _ -> -1
          | _, `Output _ -> 1
          | `Active _, `Active _ -> 0
          | `Active _, _ -> -1
          | _, `Active _ -> 1
          | `Passive, `Passive -> 0)
      pending
  with
    | el :: _ -> Some el#id
    | _ -> None

let get_id ~pending_activations id =
  match
    (Atomic.get id, meaningful_pending_id (Queue.elements pending_activations))
  with
    | Some id, _ -> id
    | None, Some id -> id
    | _ -> "generic"

let _id { id; pending_activations } = get_id ~pending_activations id
let id c = _id (Unifier.deref c)
let generate_id = Lang_string.generate_id ~category:"clock"

let _set_id clock new_id =
  if Atomic.get clock.id <> Some new_id then
    Atomic.set clock.id (Some (generate_id new_id))

let set_id c new_id = _set_id (Unifier.deref c) new_id

(* Return the clock's effective sync mode. Stopped clocks report [`Stopped]:
   they can be unified with active clocks of any sync mode. *)
let _sync clock =
  match Atomic.get clock.state with
    | `Stopped -> `Stopped
    | `Stopping _ -> `Stopping
    | `Started _ -> (clock.sync :> sync_mode)

let sync c = _sync (Unifier.deref c)

(* Sync mode the clock will have once started (or already has). *)
let _pending_sync clock =
  match Atomic.get clock.state with
    | `Stopped | `Started _ -> (clock.sync :> sync_mode)
    | `Stopping _ -> `Stopping

let _descr clock =
  Printf.sprintf "clock(id=%s,sync=%s%s)" (_id clock)
    (string_of_sync_mode (_sync clock))
    (match Atomic.get clock.state with
      | `Stopped ->
          Printf.sprintf ",pending=%s"
            (string_of_sync_mode (clock.sync :> sync_mode))
      | _ -> "")

let descr c = _descr (Unifier.deref c)

(* Direct state matches below: these accessors are in the streaming hot path
   so we avoid the option allocated by [_params]. *)

let started c =
  match Atomic.get (Unifier.deref c).state with
    | `Stopping _ | `Started _ -> true
    | `Stopped -> false

let _ticks clock =
  match Atomic.get clock.state with
    | `Stopped -> 0
    | `Stopping { ticks } | `Started { ticks } -> Atomic.get ticks

let ticks c = _ticks (Unifier.deref c)

(* A clock is in self-sync mode when it has a current sync source. Stopping
   clocks are considered out of self-sync mode. *)
let self_sync c =
  match Atomic.get (Unifier.deref c).state with
    | `Started params -> params.current_sync_source <> None
    | _ -> false

let active_sources c =
  match _params (Unifier.deref c) with
    | Some { active_sources } -> WeakQueue.elements active_sources
    | None -> []

let outputs c =
  match _params (Unifier.deref c) with
    | Some { outputs } -> List.map snd (Queue.elements outputs)
    | None -> []

let passive_sources c =
  match _params (Unifier.deref c) with
    | Some { passive_sources } -> WeakQueue.elements passive_sources
    | None -> []

let pending_activations c = Queue.elements (Unifier.deref c).pending_activations

let sources c =
  let clock = Unifier.deref c in
  Queue.elements clock.pending_activations
  @
    match _params clock with
    | Some { passive_sources; active_sources; outputs } ->
        WeakQueue.elements passive_sources
        @ WeakQueue.elements active_sources
        @ List.map snd (Queue.elements outputs)
    | None -> []

let _animated_sources { outputs; active_sources } =
  List.map snd (Queue.elements outputs) @ WeakQueue.elements active_sources

(* Compare clocks by identity. Used to deduplicate collections of clock
   handles modulo dereferencing. *)
let compare_clock_identity clock clock' =
  Int.compare clock.unique_id clock'.unique_id

(* {1 Registry}

   Global registries tracking the clocks of the application:
   - [pending] is the weak set of clocks waiting to be started by the next
     call to [start_pending]. Clocks are returned to it when they stop. Being
     weak, it lets unused clocks be garbage collected.
   - [retained] holds strong references to freshly created clocks so that
     they survive at least until the next call to [flush_pending], even when
     nothing else references them yet.
   - [started] holds strong references to the clocks started by
     [start_pending], removed when they stop.

   Passive clocks are outside the lifecycle registries above: they are
   started, ticked and stopped by their controller. [all_clocks] is the weak
   set of every clock, passive ones included, used when unification needs a
   complete view (sub-clock handle deduplication). *)

module Registry = struct
  let started : t Queue.t = Queue.create ()
  let pending : t WeakQueue.t = WeakQueue.create ()
  let retained : t Queue.t = Queue.create ()
  let all_clocks : t WeakQueue.t = WeakQueue.create ()

  (* Register a freshly created handle: no dedup scan needed. *)
  let register c = WeakQueue.push_raw all_clocks c

  let add_pending c =
    Queue.push retained c;
    WeakQueue.push pending c

  (* Re-add a clock that could not be started by [start_pending]. No strong
     retention: if nothing else references it, it can be collected. *)
  let readd_pending c = WeakQueue.push_raw pending c

  let flush_pending () =
    let elements = WeakQueue.flush_elements pending in
    Queue.clear retained;
    elements

  let mark_started c = Queue.push started c

  let mark_stopped ~clock c =
    Queue.filter_out started (fun c' -> Unifier.deref c' == clock);
    WeakQueue.push pending c

  (* Remove a handle that was unified into another one. The clock it now
     dereferences to remains registered through its own handle. *)
  let forget_handle c =
    Queue.filter_out started (fun el -> el == c);
    WeakQueue.filter_out pending (fun el -> el == c);
    WeakQueue.filter_out all_clocks (fun el -> el == c)

  let iter_started fn = Queue.iter started fn
  let managed () = WeakQueue.elements pending @ Queue.elements started
  let all () = WeakQueue.elements all_clocks
end

let clocks () =
  List.sort_uniq
    (fun c c' -> compare_clock_identity (Unifier.deref c) (Unifier.deref c'))
    (Registry.managed ())

(* {1 Sub-clocks} *)

(* Deduplicate a clock's sub-clocks modulo dereferencing: after a
   unification, two handles previously pointing to different clocks can end
   up dereferencing to the same one. *)
let _compact_sub_clocks clock =
  let all = ref [] in
  Queue.flush_iter clock.sub_clocks (fun c ->
      all := (c, Unifier.deref c) :: !all);
  let deduped =
    List.sort_uniq
      (fun (_, clock) (_, clock') -> compare_clock_identity clock clock')
      !all
  in
  List.iter (fun (c, _) -> Queue.push clock.sub_clocks c) deduped

let _sub_clocks clock =
  List.map (fun c -> (c, Unifier.deref c)) (Queue.elements clock.sub_clocks)

let sub_clocks c = List.map fst (_sub_clocks (Unifier.deref c))

let register_sub_clock parent sub =
  let parent = Unifier.deref parent in
  let clock = Unifier.deref sub in
  if not (Queue.exists parent.sub_clocks (fun c -> Unifier.deref c == clock))
  then Queue.push parent.sub_clocks sub

let deregister_sub_clock parent sub =
  let clock = Unifier.deref sub in
  Queue.filter_out (Unifier.deref parent).sub_clocks (fun c ->
      Unifier.deref c == clock)

(* {1 Source attachment} *)

let attach c s =
  let clock = Unifier.deref c in
  Queue.push clock.pending_activations s

let _detach clock s =
  Queue.filter_out clock.pending_activations (fun s' -> s == s');
  let do_detach
      { outputs; active_sources; passive_sources; sync_source_deregisters } =
    Queue.filter_out outputs (fun (a, s') ->
        if s == s' then (
          s#sleep a;
          true)
        else false);
    WeakQueue.filter_out active_sources (fun s' -> s == s');
    WeakQueue.filter_out passive_sources (fun s' -> s == s');
    Queue.filter_out sync_source_deregisters (fun (s', deregister) ->
        if s == s' then (
          (try deregister () with _ -> ());
          true)
        else false)
  in
  match Atomic.get clock.state with
    | `Stopped -> ()
    | `Stopping params -> do_detach params
    | `Started params ->
        (* Detaching while the clock is ticking would mutate the collections
           the tick is iterating on: defer to the end of the tick. *)
        Queue.push params.after_tick (fun () -> do_detach params)

let detach c s = _detach (Unifier.deref c) s

(* {1 Global stop} *)

let clocks_started = Atomic.make false
let global_stop = Atomic.make false
let[@inline] check_stopped () = if Atomic.get global_stop then raise Has_stopped

(* {1 Stopping} *)

let rec has_stopped ~clear_controller ~clock ~c params =
  (* Snapshot sub_clocks before sleeping outputs: on_sleep callbacks may
     deregister sub-clocks (e.g. ffmpeg filter graphs), which would prevent
     them from being stopped here. *)
  let sub_clocks = List.map fst (_sub_clocks clock) in
  Queue.iter params.outputs (fun (a, o) -> try o#sleep a with _ -> ());
  Queue.flush_iter params.sync_source_deregisters (fun (_, deregister) ->
      try deregister () with _ -> ());
  List.iter stop sub_clocks;
  if clear_controller then Atomic.set clock.controller `None;
  Atomic.set clock.state `Stopped;
  Registry.mark_stopped ~clock c;
  params.log#important "Clock stopped"

and stop c =
  let clock = Unifier.deref c in
  match Atomic.get clock.state with
    | `Stopped | `Stopping _ -> ()
    | `Started ({ sync = `Passive } as params) ->
        has_stopped ~clear_controller:false ~clock ~c params
    | `Started params ->
        params.log#important "Clock stopping";
        (* The clock thread detects the state change at the end of the
           current tick and calls [has_stopped]. *)
        Atomic.set clock.state (`Stopping params)

let () =
  Lifecycle.before_core_shutdown ~name:"Clocks stop" (fun () ->
      Atomic.set global_stop true;
      Registry.iter_started (fun c -> if sync c <> `Passive then stop c))

(* {1 Unification}

   Two clocks can be unified when their controllers are compatible and their
   states allow it:
   - two stopped clocks with the same sync mode always unify;
   - a stopped clock unifies with any clock when its sync mode is
     [`Automatic] or matches the other clock's pending sync mode.
   The stopped clock is always merged into the other one, so that started
   clocks (and the closures capturing their record) are never merged away. *)

let string_of_controller = function
  | `None -> "none"
  | `Clock c -> Printf.sprintf "clock %s" (id c)
  | `Other (c, o) -> Printf.sprintf "%s %s" c o#id

let unifiable_controller ~unify c c' =
  match (c, c') with
    | `None, _ | _, `None -> true
    | `Other (c, o), `Other (c', o') -> c = c' && o == o'
    | `Clock c, `Clock c' -> (
        try
          unify c c';
          true
        with
        | Liquidsoap_lang.Error.Clock_conflict _
        | Liquidsoap_lang.Error.Clock_loop _
        | Liquidsoap_lang.Error.Clock_main _
        ->
          false)
    | _ -> false

(* Make sure that [clock'] is not a transitive sub-clock of [clock]:
   unifying them would create a cycle in the tick recursion. *)
let rec check_sub_clocks ~pos clock clock' =
  Queue.iter clock.sub_clocks (fun c ->
      let sub = Unifier.deref c in
      if sub == clock' then
        raise
          (Liquidsoap_lang.Error.Clock_loop (pos, _descr clock, _descr clock'));
      check_sub_clocks ~pos sub clock')

let unify =
  (* Merge [c] into [c']: after this, both handles dereference to [c']'s
     record, which inherits [c]'s pending sources, sub-clocks, error handlers
     and, when it does not have its own, id and controller. *)
  let merge ~pos c c' =
    let clock = Unifier.deref c in
    let clock' = Unifier.deref c' in
    check_sub_clocks ~pos clock clock';
    check_sub_clocks ~pos clock' clock;
    (match (Atomic.get clock.controller, Atomic.get clock'.controller) with
      | controller, `None -> Atomic.set clock'.controller controller
      | _ -> ());
    Queue.flush_iter clock.pending_activations
      (Queue.push clock'.pending_activations);
    Queue.flush_iter clock.sub_clocks (Queue.push clock'.sub_clocks);
    Queue.flush_iter clock.on_error (Queue.push clock'.on_error);
    (match (Atomic.get clock.id, Atomic.get clock'.id) with
      | Some _, Some id ->
          log#info "Clocks %s and %s both have id already set. Setting id to %s"
            (descr c) (descr c') id
      | (Some _ as id), None -> Atomic.set clock'.id id
      | None, _ -> ());
    Unifier.(c <-- c');
    (* Any clock in the application can hold sub-clock handles that now
       dereference to the same clock: deduplicate them all. This must happen
       after the handles are unified above, when duplicates become visible. *)
    _compact_sub_clocks clock';
    List.iter (fun c -> _compact_sub_clocks (Unifier.deref c)) (Registry.all ());
    Registry.forget_handle c
  in
  let rec unify ~pos c c' =
    let clock = Unifier.deref c in
    let clock' = Unifier.deref c' in
    let controller = Atomic.get clock.controller in
    let controller' = Atomic.get clock'.controller in
    if not (unifiable_controller ~unify:(unify ~pos) controller controller')
    then
      raise
        Liquidsoap_lang.Error.(
          Clock_main
            {
              pos;
              left_main = string_of_controller controller;
              left_child = descr c;
              right_main = string_of_controller controller';
              right_child = descr c';
            });
    match
      (clock == clock', Atomic.get clock.state, Atomic.get clock'.state)
    with
      | true, _, _ -> ()
      | _, `Stopped, `Stopped when clock.sync = clock'.sync -> merge ~pos c c'
      | _, `Stopped, _
        when clock.sync = `Automatic
             || (clock.sync :> sync_mode) = _pending_sync clock' ->
          merge ~pos c c'
      | _, _, `Stopped
        when clock'.sync = `Automatic
             || _pending_sync clock = (clock'.sync :> sync_mode) ->
          merge ~pos c' c
      | _ ->
          raise (Liquidsoap_lang.Error.Clock_conflict (pos, descr c, descr c'))
  in
  unify

(* {1 Sync source tracking}

   Sources report their sync source when they are activated and whenever it
   changes afterward (via their [on_sync_source_change] callback). The clock
   maintains the set of all currently declared sync sources: at most one
   unique sync source is allowed at any given time. When it changes, the
   clock switches in or out of self-sync mode. *)

(* Update the sync source entries with source [name]'s new sync source.
   Raise [Sync_error] when this results in more than one unique sync source.
   Return the unique sync source, if any. *)
let _update_sync_source_entries ~clock params ~name ~stack new_sync =
  let entries =
    List.filter
      (fun (e : sync_source_entry) -> e.name <> name)
      params.sync_source_entries
  in
  let entries =
    match new_sync with
      | None -> entries
      | Some sync_source -> { sync_source; name; stack } :: entries
  in
  let deduped =
    List.sort_uniq
      (fun ({ sync_source = a } : sync_source_entry) { sync_source = b } ->
        Stdlib.compare a b)
      entries
  in
  if List.length deduped > 1 then
    raise
      (Sync_error
         {
           name = Printf.sprintf "clock %s" (_id clock);
           stack = Atomic.get clock.stack;
           sync_sources = deduped;
         });
  params.sync_source_entries <- entries;
  match deduped with [{ sync_source }] -> Some sync_source | _ -> None

(* Switch the clock's current sync source, resolving the latency parameters
   it imposes and re-anchoring the time implementation. *)
let _switch_sync_source params ~old_sync_source new_sync_source =
  (match (old_sync_source, new_sync_source) with
    | None, Some s ->
        params.log#important "Switching to self-sync mode (%s)"
          (string_of_sync_source s)
    | Some _, None -> params.log#important "Switching to non-self-sync mode"
    | Some _, Some s ->
        params.log#important "Switching self-sync source to %s"
          (string_of_sync_source s)
    | None, None -> ());
  params.current_sync_source <-
    Option.map
      (fun sync_source ->
        {
          sync_source;
          latency =
            Option.value ~default:conf_latency#get
              (latency_of_sync_source sync_source);
          max_latency =
            Option.value ~default:conf_max_latency#get
              (max_latency_of_sync_source sync_source);
        })
      new_sync_source;
  let time_implementation =
    match new_sync_source with
      | Some s -> time_of_sync_source s
      | None -> time_implementation ()
  in
  let module TimeImplementation = (val time_implementation) in
  (* Re-anchor the new time implementation so that Time.time () returns time
     relative to when this clock started ticking, i.e. time () = 0 at tick 0. *)
  let current_time =
    params.frame_duration *. float_of_int (Atomic.get params.ticks)
  in
  params.time_implementation <-
    Liq_time.set_offset time_implementation
      TimeImplementation.(time () |-| of_float current_time)

let _update_clock_sync_source ~clock params ~name ~stack new_sync =
  let new_sync_source =
    _update_sync_source_entries ~clock params ~name ~stack new_sync
  in
  let old_sync_source =
    Option.map (fun s -> s.sync_source) params.current_sync_source
  in
  if old_sync_source <> new_sync_source && params.sync = `Automatic then
    _switch_sync_source params ~old_sync_source new_sync_source

let _register_clock_callback ~clock params s =
  let name = s#id and stack = s#stack in
  let deregister =
    s#on_sync_source_change (fun ~old:_ new_sync_source ->
        _update_clock_sync_source ~clock params ~name ~stack new_sync_source)
  in
  Queue.push params.sync_source_deregisters ((s :> source), deregister);
  _update_clock_sync_source ~clock params ~name ~stack s#source_state

(* {1 Time and latency control}

   The clock tracks an absolute target time, [ticks * frame_duration], and
   compares it with the current time at the end of each tick: when ahead, it
   sleeps until the target time; when behind, it catches up by ticking as
   fast as possible, logging latency issues and resetting the sources when
   latency exceeds the maximum. *)

let _target_time { time_implementation; frame_duration; ticks } =
  let module Time = (val time_implementation : Liq_time.T) in
  Time.of_float (frame_duration *. float_of_int (Atomic.get ticks))

let _set_time { time_implementation; frame_duration; ticks } t =
  let module Time = (val time_implementation : Liq_time.T) in
  Atomic.set ticks (int_of_float (Time.to_float t /. frame_duration))

(* The clock is ahead of its target time: rest until the target time, but
   only when far enough ahead. *)
let _sleep_until_target params ~end_time ~target_time =
  let module Time = (val params.time_implementation : Liq_time.T) in
  let latency =
    match params.current_sync_source with
      | Some { latency } -> latency
      | None -> conf_latency#get
  in
  if Time.(of_float latency |<=| (target_time |-| end_time)) then
    Time.sleep_until target_time

(* The clock is behind its target time: reset the sources when latency
   exceeds the maximum, otherwise log periodic catchup warnings. *)
let _handle_latency params ~end_time ~target_time =
  let module Time = (val params.time_implementation : Liq_time.T) in
  let latency = Time.(end_time |-| target_time) in
  let max_latency =
    match params.current_sync_source with
      | Some { max_latency } -> max_latency
      | None -> conf_max_latency#get
  in
  if Time.(of_float max_latency |<=| latency) then (
    params.log#severe "Too much latency! Resetting active sources...";
    _set_time params end_time;
    List.iter
      (fun s ->
        match s#source_type with
          | `Passive -> assert false
          | `Active s -> s#reset
          | `Output s -> s#reset)
      (_animated_sources params))
  else if
    Time.(
      of_float params.log_delay_threshold |<=| latency
      && of_float params.log_delay
         |<=| (end_time |-| of_float (Atomic.get params.last_catchup_log)))
  then (
    Atomic.set params.last_catchup_log (Time.to_float end_time);
    params.log#severe
      "Latency is too high: we must catchup %.2f seconds! Check if your system \
       can process your stream fast enough (CPU usage, disk access, etc) or if \
       your stream should be self-sync (can happen when using `input.ffmpeg`). \
       Refer to the latency control section of the documentation for more \
       info."
      Time.(to_float (end_time |-| target_time)))

let _after_tick params =
  (* [Queue.flush_iter] takes the queue's mutation lock even when empty:
     check emptiness lock-free first, this runs on every tick. *)
  if Queue.elements params.after_tick <> [] then
    Queue.flush_iter params.after_tick (fun fn ->
        check_stopped ();
        fn ());
  let module Time = (val params.time_implementation : Liq_time.T) in
  let end_time = Time.time () in
  let target_time = _target_time params in
  check_stopped ();
  match (params.sync, Time.(end_time |<| target_time)) with
    | `Unsynced, _ | `Passive, _ -> ()
    | `Automatic, true | `CPU, true ->
        _sleep_until_target params ~end_time ~target_time
    | _ -> _handle_latency params ~end_time ~target_time

(* {1 Streaming loop} *)

(* Run [fn s], detaching source [s] and reporting the error via the clock's
   error handlers (or re-raising when there are none) on exception. *)
let wrap_errors clock fn s =
  check_stopped ();
  try fn s
  with exn when exn <> Has_stopped ->
    let bt = Printexc.get_raw_backtrace () in
    log#severe "Source %s failed while streaming: %s!\n%s" s#id
      (Printexc.to_string exn)
      (Printexc.raw_backtrace_to_string bt);
    _detach clock s;
    if Queue.length clock.on_error > 0 then
      Queue.iter clock.on_error (fun fn -> fn exn bt)
    else Printexc.raise_with_backtrace exn bt

let pretty_sources ~clock params =
  let rec build_entry clock params =
    let sub_clocks =
      List.filter_map
        (fun (_, sub_clock) ->
          match _params sub_clock with
            | Some sub_params -> Some (build_entry sub_clock sub_params)
            | None -> None)
        (_sub_clocks clock)
    in
    let src s =
      Clock_utils.
        { id = s#id; activations = List.map (fun a -> a#id) s#activations }
    in
    Clock_utils.
      {
        name = _id clock;
        outputs = List.map (fun (_, s) -> src s) (Queue.elements params.outputs);
        active = List.map src (WeakQueue.elements params.active_sources);
        passive = List.map src (WeakQueue.elements params.passive_sources);
        sub_clocks;
      }
  in
  Clock_utils.format_clock (build_entry clock params)

let active_params c =
  let clock = Unifier.deref c in
  match Atomic.get clock.state with
    | `Stopping params | `Started params -> params
    | `Stopped when Atomic.get global_stop -> raise Has_stopped
    | state ->
        log#critical "Clock %s has invalid state: %s" (_id clock)
          (string_of_state state);
        raise Invalid_state

let _activate_pending_sources ~clock params =
  (* This runs on every tick: bail out early on the common, empty case. *)
  let pending_sources = Queue.length clock.pending_activations in
  if 0 < pending_sources then (
    Queue.flush_iter clock.pending_activations
      (wrap_errors clock (fun s ->
           match s#source_type with
             | `Active _ ->
                 WeakQueue.push params.active_sources s;
                 _register_clock_callback ~clock params s
             | `Output _ ->
                 let a = s#wake_up (s :> source) in
                 Queue.push params.outputs (a, s);
                 _register_clock_callback ~clock params s
             | `Passive -> WeakQueue.push params.passive_sources s));
    let total_sources =
      Queue.length params.outputs
      + WeakQueue.length params.active_sources
      + WeakQueue.length params.passive_sources
    in
    if
      (total_sources - pending_sources) / conf_leak_warning#get
      < total_sources / conf_leak_warning#get
    then (
      params.log#severe
        "There are currently %d sources, possible source leak! Please check \
         that you don't have a loop creating multiple sources."
        total_sources;
      params.log#important "Current sources:\n%s" (pretty_sources ~clock params)))

let rec _tick ~clock params =
  (* Snapshot sub-clock ticks: sub-clocks that have already been ticked
     during this tick (e.g. by an operator pulling data from them) are not
     ticked again below. *)
  let sub_clocks =
    List.map
      (fun c ->
        let sub = Unifier.deref c in
        (c, _ticks sub, sub))
      (Queue.elements clock.sub_clocks)
  in
  _activate_pending_sources ~clock params;
  let animate =
    wrap_errors clock (fun s ->
        match s#source_type with
          | `Output s | `Active s -> s#output
          | _ -> assert false)
  in
  (* [Queue.iter] iterates lock-free over an immutable snapshot. For the weak
     queue, iterating via [WeakQueue.iter] would hold the queue lock while
     sources stream, so we snapshot its elements instead. *)
  Queue.iter params.outputs (fun (_, s) -> animate s);
  List.iter animate (WeakQueue.elements params.active_sources);
  if Queue.elements params.on_tick <> [] then
    Queue.flush_iter params.on_tick (fun fn ->
        check_stopped ();
        fn ());
  check_stopped ();
  List.iter
    (fun (c, old_ticks, clock) ->
      if ticks c = old_ticks then _tick ~clock (active_params c))
    sub_clocks;
  Atomic.incr params.ticks;
  check_stopped ();
  _after_tick params;
  check_stopped ()

let _clock_thread ~clock ~c params =
  let has_sources_to_process () =
    0 < Queue.length clock.pending_activations
    || 0 < Queue.length params.outputs
    || 0 < WeakQueue.length params.active_sources
  in
  let still_started () =
    match Atomic.get clock.state with `Started _ -> true | _ -> false
  in
  let global_stop () = Atomic.get global_stop in
  let on_stop () =
    let reasons =
      List.filter_map
        (fun (reason, active) -> if active then Some reason else None)
        [
          ("clock stopped", not (still_started ()));
          ("global stop", global_stop ());
          ("no more sources to process", not (has_sources_to_process ()));
        ]
    in
    params.log#important "Clock thread has stopped: %s."
      (String.concat ", " reasons);
    has_stopped ~clear_controller:true ~clock ~c params
  in
  let run () =
    try
      while
        still_started () && (not (global_stop ())) && has_sources_to_process ()
      do
        _tick ~clock params
      done;
      on_stop ()
    with Has_stopped -> on_stop ()
  in
  Tutils.create
    (fun () ->
      params.log#info "Clock thread is starting";
      run ())
    ()
    ("Clock " ^ _id clock)

(* {1 Starting}

   Clocks are started by [start_pending], called when the application starts
   and after each script evaluation. Only clocks with at least one pending
   output are started (unless forced): a clock with no output has nothing to
   animate. Passive clocks are never started by [start_pending]: they are
   started by whoever controls them (see [Child_support]). *)

let _can_start ?(force = false) clock =
  let has_output =
    force
    || Queue.exists clock.pending_activations (fun s ->
        match s#source_type with `Output _ -> true | _ -> false)
  in
  let can_start =
    (not (Atomic.get global_stop)) && (force || Atomic.get clocks_started)
  in
  match Atomic.get clock.state with
    | `Stopped -> can_start && (has_output || clock.sync = `Passive)
    | _ -> false

let rec _start ?force ~c clock =
  _set_id clock (_id clock);
  let id = _id clock in
  let sync = clock.sync in
  let sources =
    List.fold_left
      (fun sources s ->
        let source_type =
          match s#source_type with
            | `Passive -> "passive"
            | `Active _ -> "active"
            | `Output _ -> "output"
        in
        Printf.sprintf "%s (%s)" s#id source_type :: sources)
      []
      (Queue.elements clock.pending_activations)
  in
  let sources = String.concat ", " sources in
  let top_level, sync_mode =
    match sync with
      | `Passive -> ("passive", "")
      | _ ->
          ( "top-level",
            Printf.sprintf " and sync: %s" (string_of_sync_mode sync) )
  in
  let controller = Atomic.get clock.controller in
  let controlled_by =
    if controller = `None then ""
    else Printf.sprintf " controlled by %s" (string_of_controller controller)
  in
  log#important "Starting %s clock %s%s with sources: %s%s" top_level id
    controlled_by sources sync_mode;
  (* Anchor the time implementation so that Time.time () returns time relative
     to when this clock starts ticking, i.e. time () = 0 at tick 0. *)
  let time_implementation =
    let base = time_implementation () in
    let module Time = (val base : Liq_time.T) in
    Liq_time.set_offset base (Time.time ())
  in
  let module Time = (val time_implementation : Liq_time.T) in
  let params =
    {
      frame_duration = Lazy.force Frame.duration;
      current_sync_source = None;
      sync_source_entries = [];
      log_delay = conf_log_delay#get;
      log_delay_threshold = conf_log_delay_threshold#get;
      time_implementation;
      log = Log.make (["clock"] @ String.split_on_char '.' id);
      last_catchup_log = Atomic.make (Time.to_float (Time.time ()));
      sync;
      active_sources = WeakQueue.create ();
      passive_sources = WeakQueue.create ();
      sync_source_deregisters = Queue.create ();
      on_tick = Queue.create ();
      after_tick = Queue.create ();
      outputs = Queue.create ();
      ticks = Atomic.make 0;
    }
  in
  Queue.iter clock.sub_clocks (fun c -> start ?force c);
  Atomic.set clock.state (`Started params);
  if sync <> `Passive then (
    let th = _clock_thread ~clock ~c params in
    match controller with
      | `None ->
          let controller =
            object
              method id = string_of_int (Thread.id th)
            end
          in
          Atomic.set clock.controller (`Other ("thread", controller))
      | _ -> raise Invalid_state)

and start ?force c =
  let clock = Unifier.deref c in
  if _can_start ?force clock then _start ?force ~c clock

let create ?(stack = []) ?(controller = `None) ?on_error ?id
    ?(sync = `Automatic) () =
  let on_error_queue = Queue.create () in
  (match on_error with None -> () | Some fn -> Queue.push on_error_queue fn);
  let c =
    Unifier.make
      {
        unique_id = Atomic.fetch_and_add clock_counter 1;
        id = Atomic.make (Option.map generate_id id);
        sync;
        controller = Atomic.make controller;
        stack = Atomic.make stack;
        pending_activations = Queue.create ();
        sub_clocks = Queue.create ();
        state = Atomic.make `Stopped;
        on_error = on_error_queue;
      }
  in
  Registry.register c;
  if sync <> `Passive then Registry.add_pending c;
  c

let start_pending () =
  let pending = Registry.flush_pending () in
  let pending = List.map (fun c -> (c, Unifier.deref c)) pending in
  let pending =
    List.sort_uniq
      (fun (_, clock) (_, clock') -> compare_clock_identity clock clock')
      pending
  in
  List.iter
    (fun (c, clock) ->
      match Atomic.get clock.state with
        | `Stopped ->
            if _can_start clock then (
              if clock.sync <> `Passive then (
                _start ~c clock;
                Registry.mark_started c))
            else Registry.readd_pending c
        | _ -> ())
    pending

let () =
  Lifecycle.before_start ~name:"Clocks start" (fun () ->
      Atomic.set clocks_started true;
      start_pending ())

let after_eval () = if not (Atomic.get global_stop) then start_pending ()

(* {1 Ticking API} *)

let on_tick c fn =
  let params = active_params c in
  Queue.push params.on_tick fn

let after_tick c fn =
  let params = active_params c in
  Queue.push params.after_tick fn

let activate_pending_sources c =
  _activate_pending_sources ~clock:(Unifier.deref c) (active_params c)

let tick c = _tick ~clock:(Unifier.deref c) (active_params c)

let time c =
  match active_params c with
    | { time_implementation } as params ->
        let module Time = (val time_implementation : Liq_time.T) in
        Time.to_float (_target_time params)
    | exception Invalid_state -> -1.

let set_stack c stack =
  ignore (Atomic.compare_and_set (Unifier.deref c).stack [] stack)

(* {1 Introspection} *)

let dump () =
  let rec build_entry c =
    let src s =
      Clock_utils.
        { id = s#id; activations = List.map (fun a -> a#id) s#activations }
    in
    let sources ss = List.map src ss in
    Clock_utils.
      {
        clock_name = id c;
        ticks = ticks c;
        time = Lazy.force Frame.duration *. float (ticks c);
        self_sync = self_sync c;
        outputs = sources (outputs c);
        active = sources (active_sources c);
        passive = sources (passive_sources c);
        sub_clocks = List.map build_entry (sub_clocks c);
      }
  in
  Clock_utils.format_dump (List.map build_entry (clocks ()))

let dump_sources c =
  let to_graph_source s =
    let source_kind =
      match s#source_type with
        | `Output _ -> `Output
        | `Active _ -> `Active
        | `Passive -> `Passive
    in
    Clock_utils.
      {
        source_name = s#id;
        source_kind;
        source_activations = List.map (fun a -> a#id) s#activations;
      }
  in
  let all_sources = sources c in
  Clock_utils.format_source_graph (List.map to_graph_source all_sources)

let dump_all_sources () =
  let rec dump_clock ~controlled_by c =
    let sub_dumps =
      List.map
        (dump_clock
           ~controlled_by:(Printf.sprintf " (controlled by %s)" (id c)))
        (sub_clocks c)
    in
    let dumps = dump_sources c :: sub_dumps in
    let dump = String.concat "\n\n" (List.filter (fun s -> s <> "") dumps) in
    Printf.sprintf "Clock %s%s:\n%s" (id c) controlled_by dump
  in
  let dumps = List.map (dump_clock ~controlled_by:"") (clocks ()) in
  String.concat "\n\n" (List.filter (fun s -> s <> "") dumps)

let global_stop () = Atomic.set global_stop true
