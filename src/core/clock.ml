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

module Evaluation = Liquidsoap_lang.Evaluation

type active_source = < reset : unit ; output : unit >

type source_type =
  [ `Passive | `Active of active_source | `Output of active_source ]

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
  ; is_ready : bool
  ; get_frame : Frame.t >

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
  after_tick : (unit -> unit) Queue.t;
  ticks : int Atomic.t;
}

type state =
  [ `Stopping of active_params
  | `Started of active_params
  | `Stopped of active_sync_mode ]

type clock = {
  mutable id : string;
  pos : Pos.Option.t Atomic.t;
  state : state Atomic.t;
  pending_activations : source WeakQueue.t;
  sub_clocks : t Queue.t;
}

and t = clock Unifier.t

let _sync ?(pending = false) x =
  match Atomic.get x.state with
    | `Stopped p when pending -> (p :> sync_mode)
    | `Stopped _ -> `Stopped
    | `Stopping _ -> `Stopping
    | `Started { sync } -> (sync :> sync_mode)

let sync c = _sync (Unifier.deref c)
let cleanup_source s = s#sleep

let rec _cleanup ~clock { outputs; passive_sources; active_sources } =
  Queue.iter outputs cleanup_source;
  WeakQueue.iter passive_sources cleanup_source;
  WeakQueue.iter active_sources cleanup_source;
  Queue.iter clock.sub_clocks stop

and stop c =
  let clock = Unifier.deref c in
  match Atomic.get clock.state with
    | `Stopped _ | `Stopping _ -> ()
    | `Started ({ sync = `Passive } as x) ->
        _cleanup ~clock x;
        x.log#debug "Clock stopped";
        Atomic.set clock.state (`Stopped `Passive)
    | `Started x ->
        x.log#debug "Clock stopping";
        Atomic.set clock.state (`Stopping x)

let clocks : t WeakQueue.t = WeakQueue.create ()
let started = Atomic.make false
let global_stop = Atomic.make false

let descr clock =
  let clock = Unifier.deref clock in
  Printf.sprintf "clock(id=%s,sync=%s%s)" clock.id
    (string_of_sync_mode (_sync clock))
    (match Atomic.get clock.state with
      | `Stopped pending ->
          Printf.sprintf ",pending=%s"
            (string_of_sync_mode (pending :> sync_mode))
      | _ -> "")

let unify =
  let unify c c' =
    let clock = Unifier.deref c in
    let clock' = Unifier.deref c' in
    WeakQueue.iter clock.pending_activations
      (WeakQueue.push clock'.pending_activations);
    Queue.iter clock.sub_clocks (Queue.push clock'.sub_clocks);
    Unifier.(c <-- c')
  in
  fun ~pos c c' ->
    let _c = Unifier.deref c in
    let _c' = Unifier.deref c' in
    match (_c == _c', Atomic.get _c.state, Atomic.get _c'.state) with
      | true, _, _ -> ()
      | _, `Stopped s, _
        when s = `Automatic || (s :> sync_mode) = _sync ~pending:true _c' ->
          unify c c'
      | _, _, `Stopped s'
        when s' = `Automatic || _sync ~pending:true _c = (s' :> sync_mode) ->
          unify c' c
      | _ ->
          raise (Liquidsoap_lang.Error.Clock_conflict (pos, descr c, descr c'))

let () =
  Lifecycle.before_core_shutdown ~name:"Clocks stop" (fun () ->
      Atomic.set global_stop true;
      WeakQueue.iter clocks (fun c -> if sync c <> `Passive then stop c))

let _active_sources { outputs; active_sources } =
  Queue.elements outputs @ WeakQueue.elements active_sources

let _self_sync ~clock x =
  let self_sync_sources =
    List.(
      sort_uniq Stdlib.compare
        (filter (fun s -> snd s#self_sync <> None) (_active_sources x)))
  in
  if List.length self_sync_sources > 1 then
    Runtime_error.raise
      ~pos:(match Atomic.get clock.pos with Some p -> [p] | None -> [])
      ~message:
        (Printf.sprintf "Clock %s has multiple synchronization sources!"
           clock.id)
      "source";
  List.length self_sync_sources = 1

let ticks c =
  match Atomic.get (Unifier.deref c).state with
    | `Stopped _ -> 0
    | `Stopping { ticks } | `Started { ticks } -> Atomic.get ticks

let _target_time { time_implementation; t0; frame_duration; ticks } =
  let module Time = (val time_implementation : Liq_time.T) in
  Time.(t0 |+| (frame_duration |*| of_float (float_of_int (Atomic.get ticks))))

let _after_tick ~clock x =
  Queue.flush x.after_tick (fun fn -> fn ());
  let module Time = (val x.time_implementation : Liq_time.T) in
  let end_time = Time.time () in
  let target_time = _target_time x in
  match (x.sync, _self_sync ~clock x, Time.(end_time |<| target_time)) with
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

let rec active_params c =
  match Atomic.get (Unifier.deref c).state with
    | `Stopping s | `Started s -> s
    | _ -> raise Invalid_state

and _tick ~clock x =
  Evaluation.after_eval (fun () ->
      WeakQueue.flush clock.pending_activations (fun s ->
          s#wake_up;
          match s#source_type with
            | `Active _ -> WeakQueue.push x.active_sources s
            | `Output _ -> Queue.push x.outputs s
            | `Passive -> WeakQueue.push x.passive_sources s);
      let sub_clocks =
        List.map (fun c -> (c, ticks c)) (Queue.elements clock.sub_clocks)
      in
      let sources = _active_sources x in
      List.iter
        (fun s ->
          match s#source_type with
            | `Output s | `Active s -> s#output
            | _ -> assert false)
        sources;
      Queue.flush x.on_tick (fun fn -> fn ());
      List.iter
        (fun (c, old_ticks) ->
          if ticks c = old_ticks then
            _tick ~clock:(Unifier.deref c) (active_params c))
        sub_clocks;
      Atomic.incr x.ticks;
      _after_tick ~clock x)

and _clock_thread ~clock x =
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
      _cleanup ~clock x;
      Atomic.set clock.state (`Stopped x.sync))
  in
  ignore
    (Tutils.create
       (fun () ->
         x.log#info "Clock thread is starting";
         run ())
       () ("Clock " ^ clock.id))

and start ?main ?(force = false) c =
  let clock = Unifier.deref c in
  let sync_sources =
    List.(
      sort_uniq Stdlib.compare
        (map
           (fun s -> s#self_sync)
           (WeakQueue.elements clock.pending_activations)))
  in
  match
    (force || Atomic.get started, Atomic.get clock.state, main, sync_sources)
  with
    | _, _, _, [] -> ()
    | _, `Stopped `Automatic, Some main, [(`Static, None)] ->
        unify ~pos:(Atomic.get clock.pos) c main;
        start main
    | true, `Stopped sync, _, _ ->
        clock.id <- Lang_string.generate_id clock.id;
        log#important "Starting clock %s with %d source(s) and sync: %s"
          clock.id
          (WeakQueue.length clock.pending_activations)
          (string_of_sync_mode sync);
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
            after_tick = Queue.create ();
            outputs = Queue.create ();
            ticks = Atomic.make 0;
          }
        in
        Atomic.set clock.state (`Started x);
        if sync <> `Passive then _clock_thread ~clock x
    | _ -> raise Invalid_state

let create ?pos ?(id = "generic") ?(sync = `Automatic) () =
  let c =
    Unifier.make
      {
        id;
        pos = Atomic.make pos;
        pending_activations = WeakQueue.create ();
        sub_clocks = Queue.create ();
        state = Atomic.make (`Stopped sync);
      }
  in
  WeakQueue.push clocks c;
  c

let main = create ~id:"main" ~sync:`Automatic ()

let () =
  Lifecycle.after_start ~name:"Clocks start" (fun () ->
      Atomic.set started true;
      start main)

let create ?pos ?id ?sync () =
  let c = create ?pos ?id ?sync () in
  Evaluation.on_after_eval (fun () -> start c);
  c

let start = start ~main
let id c = (Unifier.deref c).id

let attach c s =
  let clock = Unifier.deref c in
  WeakQueue.push clock.pending_activations s

let detach c s =
  let x = Unifier.deref c in
  WeakQueue.filter x.pending_activations (fun s' -> s == s');
  match Atomic.get x.state with
    | `Stopped _ | `Stopping _ -> ()
    | `Started { outputs; active_sources; passive_sources } ->
        Queue.filter outputs (fun s' -> s == s');
        WeakQueue.filter active_sources (fun s' -> s == s');
        WeakQueue.filter passive_sources (fun s' -> s == s')

let on_tick c fn =
  let x = active_params c in
  Queue.push x.on_tick fn

let after_tick c fn =
  let x = active_params c in
  Queue.push x.after_tick fn

let self_sync c =
  let clock = Unifier.deref c in
  match Atomic.get clock.state with
    | `Started params -> _self_sync ~clock params
    | _ -> false

let tick clock = _tick ~clock:(Unifier.deref clock) (active_params clock)
let set_pos c pos = Atomic.set (Unifier.deref c).pos pos

let create_sub_clock ~id clock =
  let clock = Unifier.deref clock in
  let sub_clock = create ?pos:(Atomic.get clock.pos) ~id ~sync:`Passive () in
  Queue.push clock.sub_clocks sub_clock;
  sub_clock

let clocks () = WeakQueue.elements clocks
