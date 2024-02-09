(*****************************************************************************

  Liquidsoap, a programmable stream generator.
  Copyright 2003-2024 Savonet team

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

exception Invalid_state

type active_source = < reset : unit >
type output_source = < reset : unit ; output : unit >

type source_type =
  [ `Passive | `Active of active_source | `Output of output_source ]

type sync_source = ..
type self_sync = [ `Static | `Dynamic ] * sync_source option

module Queue = Liquidsoap_lang.Queues.Queue

exception Sync_source_name of string

let sync_sources_handlers = Queue.create ()

let string_of_sync_source s =
  try
    Queue.iter sync_sources_handlers (fun fn -> fn s);
    assert false
  with Sync_source_name s -> s

module type SyncSource = sig
  type t

  val to_string : t -> string
end

module MkSyncSource (S : SyncSource) = struct
  type sync_source += Sync_source of S.t

  let make v = Sync_source v

  let () =
    Queue.push sync_sources_handlers (function
      | Sync_source v -> raise (Sync_source_name (S.to_string v))
      | _ -> ())
end

type source =
  < id : string
  ; self_sync : self_sync
  ; source_type : source_type
  ; active : bool
  ; wake_up : unit
  ; sleep : unit
  ; before_streaming_cycle : unit
  ; is_ready : bool
  ; get_frame : Frame.t
  ; after_streaming_cycle : unit >

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

let log = Log.make ["clock"]

let conf_clock =
  Dtools.Conf.void ~p:(Configure.conf#plug "clock") "Clock settings"

let conf_log_delay =
  Dtools.Conf.float
    ~p:(conf_clock#plug "log_delay")
    ~d:1. "How often (in seconds) we should indicate catchup errors."

let conf =
  Dtools.Conf.void ~p:(Configure.conf#plug "root") "Streaming clock settings"

let conf_max_latency =
  Dtools.Conf.float ~p:(conf#plug "max_latency") ~d:60.
    "Maximum latency in seconds"
    ~comments:
      [
        "If the latency gets higher than this value, the outputs will be reset,";
        "instead of trying to catch it up second by second.";
        "The reset is typically only useful to reconnect icecast mounts.";
      ]

let conf_clock_preferred =
  Dtools.Conf.string ~d:"posix"
    ~p:(conf_clock#plug "preferred")
    "Preferred clock implementation. One if: \"posix\" or \"ocaml\"."

let time_implementation () =
  try Hashtbl.find Liq_time.implementations conf_clock_preferred#get
  with Not_found -> Liq_time.unix

let () =
  Lifecycle.on_init ~name:"Clock initialization" (fun () ->
      let module Time = (val time_implementation () : Liq_time.T) in
      log#important "Using %s implementation for latency control"
        Time.implementation)

module Pos = Liquidsoap_lang.Pos
module Unifier = Liquidsoap_lang.Unifier
module WeakQueue = Liquidsoap_lang.Queues.WeakQueue

type active_params = {
  sync : active_sync_mode;
  log : Log.t;
  time_implementation : Liq_time.implementation;
  t0 : Liq_time.t;
  log_delay : Liq_time.t;
  frame_duration : Liq_time.t;
  max_latency : Liq_time.t;
  last_catchup_log : Liq_time.t Atomic.t;
  outputs : source Queue.t;
  active_sources : source WeakQueue.t;
  passive_sources : source WeakQueue.t;
  on_tick : (unit -> unit) Queue.t;
  ticks : int Atomic.t;
}

type state = [ `Stopping | `Started of active_params | `Stopped ]

type clock = {
  id : string;
  pos : Pos.Option.t Atomic.t;
  state : state Atomic.t;
  pending_activations : source WeakQueue.t;
  sub_clocks : t WeakQueue.t;
}

and t = clock Unifier.t

let clocks : t WeakQueue.t = WeakQueue.create ()

let create ?pos ?(id = "generic") () =
  let id = Lang_string.generate_id ("clock." ^ id) in
  let c =
    Unifier.make
      {
        id;
        pos = Atomic.make pos;
        pending_activations = WeakQueue.create ();
        sub_clocks = WeakQueue.create ();
        state = Atomic.make `Stopped;
      }
  in
  WeakQueue.push clocks c;
  c

let id c = (Unifier.deref c).id

let sync c =
  match Atomic.get (Unifier.deref c).state with
    | `Stopped -> `Stopped
    | `Stopping -> `Stopping
    | `Started { sync } -> (sync :> sync_mode)

let pending_activations c = (Unifier.deref c).pending_activations

let unify =
  let unify c c' =
    log#debug "%s becomes %s" (id c) (id c');
    let q = pending_activations c in
    let q' = pending_activations c' in
    WeakQueue.iter q (WeakQueue.push q');
    Unifier.(c <-- c')
  in
  fun ~pos c c' ->
    match
      (Atomic.get (Unifier.deref c).state, Atomic.get (Unifier.deref c').state)
    with
      | `Stopped, _ -> unify c c'
      | _, `Stopped -> unify c' c
      | _ -> raise (Liquidsoap_lang.Error.Clock_conflict (pos, id c, id c'))

let attach c s = WeakQueue.push (pending_activations c) s

let detach c s =
  let x = Unifier.deref c in
  WeakQueue.filter x.pending_activations (fun s' -> s == s');
  match Atomic.get x.state with
    | `Stopped | `Stopping -> ()
    | `Started { outputs; active_sources; passive_sources } ->
        Queue.filter outputs (fun s' -> s == s');
        WeakQueue.filter active_sources (fun s' -> s == s');
        WeakQueue.filter passive_sources (fun s' -> s == s')

let active_params c =
  match Atomic.get (Unifier.deref c).state with
    | `Started s -> s
    | _ -> raise Invalid_state

let on_tick c fn =
  let x = active_params c in
  Queue.push x.on_tick fn

let _active_sources { outputs; active_sources } =
  Queue.elements outputs @ WeakQueue.elements active_sources

let _self_sync x =
  let self_sync_sources =
    List.(
      sort_uniq Stdlib.compare
        (filter (fun s -> snd s#self_sync <> None) (_active_sources x)))
  in
  if List.length self_sync_sources > 1 then
    x.log#critical
      "More than one active source has control over their latency. This is \
       bound to create issues!";
  List.length self_sync_sources >= 1

let self_sync c = _self_sync (active_params c)

let ticks c =
  match Atomic.get (Unifier.deref c).state with
    | `Stopped | `Stopping -> 0
    | `Started { ticks } -> Atomic.get ticks

let _target_time { time_implementation; t0; frame_duration; ticks } =
  let module Time = (val time_implementation : Liq_time.T) in
  Time.(t0 |+| (frame_duration |*| of_float (float_of_int (Atomic.get ticks))))

let _after_tick x =
  let module Time = (val x.time_implementation : Liq_time.T) in
  let end_time = Time.time () in
  let target_time = _target_time x in
  match (x.sync, _self_sync x, Time.(end_time |<| target_time)) with
    | `Unsynced, _, _ | `Passive, _, _ | `Automatic, true, _ -> ()
    | `Automatic, false, true | `CPU, _, true -> Time.sleep_until target_time
    | _ ->
        if Time.(x.max_latency |<=| (end_time |-| target_time)) then (
          x.log#severe "Too much latency! Resetting active sources...";
          List.iter
            (fun s ->
              match s#source_type with
                | `Passive -> assert false
                | `Active s -> s#reset
                | `Output s -> s#reset)
            (_active_sources x))
        else if
          Time.(x.log_delay |<=| (end_time |-| Atomic.get x.last_catchup_log))
        then (
          Atomic.set x.last_catchup_log end_time;
          x.log#severe "We must catchup %.2f seconds!"
            Time.(to_float (end_time |-| target_time)))

let rec _tick ~clock x =
  WeakQueue.flush clock.pending_activations (fun s ->
      x.log#info "%s gets up" s#id;
      s#wake_up;
      match s#source_type with
        | `Active _ -> WeakQueue.push x.active_sources s
        | `Output _ -> Queue.push x.outputs s
        | `Passive -> WeakQueue.push x.passive_sources s);
  let sub_clocks =
    List.map (fun c -> (c, ticks c)) (WeakQueue.elements clock.sub_clocks)
  in
  let active_sources = _active_sources x in
  let all_sources = active_sources @ WeakQueue.elements x.passive_sources in
  List.iter (fun s -> s#before_streaming_cycle) all_sources;
  List.iter
    (fun s ->
      if s#is_ready then (
        match s#source_type with
          | `Output o -> o#output
          | _ -> ignore s#get_frame))
    active_sources;
  Queue.flush x.on_tick (fun fn -> fn ());
  List.iter (fun s -> s#after_streaming_cycle) all_sources;
  List.iter
    (fun (c, old_ticks) ->
      if ticks c = old_ticks then
        _tick ~clock:(Unifier.deref c) (active_params c))
    sub_clocks;
  Atomic.incr x.ticks;
  _after_tick x

let tick clock = _tick ~clock:(Unifier.deref clock) (active_params clock)

let _cleanup =
  let cleanup s = s#sleep in
  fun { outputs; passive_sources; active_sources } ->
    Queue.iter outputs cleanup;
    WeakQueue.iter passive_sources cleanup;
    WeakQueue.iter active_sources cleanup

let stop c =
  let clock = Unifier.deref c in
  match Atomic.get clock.state with
    | `Stopped | `Stopping -> ()
    | `Started ({ sync = `Passive } as x) ->
        _cleanup x;
        Atomic.set clock.state `Stopped
    | `Started _ -> Atomic.set clock.state `Stopping

let global_stop = Atomic.make false

let () =
  Lifecycle.before_core_shutdown ~name:"Clocks stop" (fun () ->
      Atomic.set global_stop true;
      WeakQueue.iter clocks stop)

let _clock_thread ~clock x =
  let has_sources_to_process () =
    0 < WeakQueue.length clock.pending_activations
    || 0 < Queue.length x.outputs
    || 0 < WeakQueue.length x.active_sources
  in
  let rec run () =
    if
      (match Atomic.get clock.state with `Started _ -> true | _ -> false)
      && (not (Atomic.get global_stop))
      && has_sources_to_process ()
    then (
      _tick ~clock x;
      run ())
    else (
      x.log#info "Clock thread has stopped";
      _cleanup x;
      Atomic.set clock.state `Stopped)
  in
  ignore
    (Tutils.create
       (fun () ->
         x.log#info "Clock thread is starting";
         run ())
       () ("Clock " ^ clock.id))

let set_pos c pos = Atomic.set (Unifier.deref c).pos pos

let start ~sync c =
  let clock = Unifier.deref c in
  match Atomic.get clock.state with
    | `Stopped ->
        log#important "Starting clock %s with %d source(s)" clock.id
          (WeakQueue.length clock.pending_activations);
        let time_implementation = time_implementation () in
        let module Time = (val time_implementation : Liq_time.T) in
        let frame_duration = Time.of_float (Lazy.force Frame.duration) in
        let max_latency = Time.of_float conf_max_latency#get in
        let log_delay = Time.of_float conf_log_delay#get in
        let t0 = Time.time () in
        let last_catchup_log = Atomic.make t0 in
        let x =
          {
            frame_duration;
            log_delay;
            max_latency;
            time_implementation;
            t0;
            log = Log.make ["clock"; clock.id];
            last_catchup_log;
            sync;
            active_sources = WeakQueue.create ();
            passive_sources = WeakQueue.create ();
            on_tick = Queue.create ();
            outputs = Queue.create ();
            ticks = Atomic.make 0;
          }
        in
        Atomic.set clock.state (`Started x);
        if sync <> `Passive then _clock_thread ~clock x
    | _ -> raise Invalid_state

let create_sub_clock ~id clock =
  let clock = Unifier.deref clock in
  let sub_clock = create ?pos:(Atomic.get clock.pos) ~id () in
  start ~sync:`Passive sub_clock;
  WeakQueue.push clock.sub_clocks sub_clock;
  sub_clock

let started = Atomic.make false

let start_clocks () =
  WeakQueue.iter clocks (fun c ->
      match Atomic.get (Unifier.deref c).state with
        | `Stopped -> start ~sync:`Automatic c
        | `Stopping | `Started _ -> ())

let () =
  Lifecycle.after_start ~name:"Clocks start" (fun () ->
      Atomic.set started true;
      start_clocks ())

let start_pending_after_eval () =
  if Atomic.get started then
    Liquidsoap_lang.Evaluation.on_after_eval start_clocks

let clocks () = WeakQueue.elements clocks
