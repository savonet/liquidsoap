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

include Clock_base

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

(** If true, a clock keeps running when an output fails. Other outputs may
  * still be useful. But there may also be some useless inputs left.
  * If no active output remains, the clock will exit without triggering
  * shutdown. We may need some device to allow this (but active and passive
  * clocks will have to be treated separately). *)
let allow_streaming_errors =
  Dtools.Conf.bool
    ~p:(conf_clock#plug "allow_streaming_errors")
    ~d:false "Handling of streaming errors"
    ~comments:
      [
        "Control the behaviour of clocks when an error occurs during streaming.";
        "This has no effect on errors occurring during source initializations.";
        "By default, any error will cause liquidsoap to shutdown. If errors";
        "are allowed, faulty sources are simply removed and clocks keep \
         running.";
        "Allowing errors can result in complex surprising situations;";
        "use at your own risk!";
      ]

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

let conf_clock_preferred =
  Dtools.Conf.string ~d:"posix"
    ~p:(conf_clock#plug "preferred")
    "Preferred clock implementation. One if: \"posix\" or \"ocaml\"."

let conf_clock_sleep_latency =
  Dtools.Conf.int
    ~p:(conf_clock#plug "sleep_latency")
    ~d:1
    "How much time ahead (in frame duration) we should be until we let the \
     streaming loop sleep."
    ~comments:
      [
        "Once we have computed the given amount of time time in advance,";
        "we wait until re-starting the streaming loop.";
      ]

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

type active_params = {
  sync : active_sync_mode;
  log : Log.t;
  time_implementation : Liq_time.implementation;
  t0 : Liq_time.t;
  log_delay : Liq_time.t;
  log_delay_threshold : Liq_time.t;
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
  id : string Unifier.t;
  sub_ids : string list;
  stack : Pos.t list Atomic.t;
  state : state Atomic.t;
  pending_activations : source Queue.t;
  sub_clocks : t Queue.t;
  on_error : (exn -> Printexc.raw_backtrace -> unit) Queue.t;
}

and t = clock Unifier.t

let _id clock =
  Unifier.deref clock.id
  ^ match clock.sub_ids with [] -> "" | l -> "." ^ String.concat "." l

let id c = _id (Unifier.deref c)

let attach c s =
  let clock = Unifier.deref c in
  Queue.push clock.pending_activations s

let _detach x s =
  Queue.filter x.pending_activations (fun s' -> s == s');
  match Atomic.get x.state with
    | `Stopped _ -> ()
    | `Stopping { outputs; active_sources; passive_sources }
    | `Started { outputs; active_sources; passive_sources } ->
        Queue.filter outputs (fun s' -> s == s');
        WeakQueue.filter active_sources (fun s' -> s == s');
        WeakQueue.filter passive_sources (fun s' -> s == s')

let detach c s = _detach (Unifier.deref c) s

let active_sources c =
  match Atomic.get (Unifier.deref c).state with
    | `Started { active_sources } | `Stopping { active_sources } ->
        WeakQueue.elements active_sources
    | _ -> []

let outputs c =
  match Atomic.get (Unifier.deref c).state with
    | `Started { outputs } | `Stopping { outputs } -> Queue.elements outputs
    | _ -> []

let passive_sources c =
  match Atomic.get (Unifier.deref c).state with
    | `Started { passive_sources } | `Stopping { passive_sources } ->
        WeakQueue.elements passive_sources
    | _ -> []

let pending_activations c = Queue.elements (Unifier.deref c).pending_activations

let sources c =
  let clock = Unifier.deref c in
  Queue.elements clock.pending_activations
  @
  match Atomic.get clock.state with
    | `Started { passive_sources; active_sources; outputs }
    | `Stopping { passive_sources; active_sources; outputs } ->
        WeakQueue.elements passive_sources
        @ WeakQueue.elements active_sources
        @ Queue.elements outputs
    | _ -> []

let _sync ?(pending = false) x =
  match Atomic.get x.state with
    | `Stopped p when pending -> (p :> sync_mode)
    | `Stopped _ -> `Stopped
    | `Stopping _ -> `Stopping
    | `Started { sync } -> (sync :> sync_mode)

let sync c = _sync (Unifier.deref c)
let cleanup_source s = s#force_sleep
let clocks = Queue.create ()

let rec _cleanup ~clock { outputs; passive_sources; active_sources } =
  Queue.iter outputs cleanup_source;
  WeakQueue.iter passive_sources cleanup_source;
  WeakQueue.iter active_sources cleanup_source;
  Queue.iter clock.sub_clocks stop;
  Queue.filter clocks (fun c -> Unifier.deref c == clock)

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

let clocks_started = Atomic.make false
let global_stop = Atomic.make false

exception Has_stopped

let[@inline] check_stopped () = if Atomic.get global_stop then raise Has_stopped

let descr clock =
  let clock = Unifier.deref clock in
  Printf.sprintf "clock(id=%s,sync=%s%s)" (_id clock)
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
    Queue.flush_iter clock.pending_activations
      (Queue.push clock'.pending_activations);
    Queue.flush_iter clock.sub_clocks (Queue.push clock'.sub_clocks);
    Queue.flush_iter clock.on_error (Queue.push clock'.on_error);
    Queue.filter clocks (fun el -> el != c);
    Unifier.(clock.id <-- clock'.id);
    Unifier.(c <-- c')
  in
  fun ~pos c c' ->
    let _c = Unifier.deref c in
    let _c' = Unifier.deref c' in
    match (_c == _c', Atomic.get _c.state, Atomic.get _c'.state) with
      | true, _, _ -> ()
      | _, `Stopped s, `Stopped s' when s = s' -> unify c c'
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
      Queue.iter clocks (fun c -> if sync c <> `Passive then stop c))

let _animated_sources { outputs; active_sources } =
  Queue.elements outputs @ WeakQueue.elements active_sources

let _self_sync ~clock x =
  let self_sync_sources =
    List.fold_left
      (fun self_sync_sources s ->
        match s#self_sync with
          | _, None -> self_sync_sources
          | _, Some sync_source ->
              { sync_source; name = s#id; stack = s#stack } :: self_sync_sources)
      [] (_animated_sources x)
  in
  let self_sync_sources =
    List.sort_uniq
      (fun { sync_source = s } { sync_source = s' } -> Stdlib.compare s s')
      self_sync_sources
  in
  if List.length self_sync_sources > 1 then
    raise
      (Sync_error
         {
           name = Printf.sprintf "clock %s" (_id clock);
           stack = Atomic.get clock.stack;
           sync_sources = self_sync_sources;
         });
  List.length self_sync_sources = 1

let ticks c =
  match Atomic.get (Unifier.deref c).state with
    | `Stopped _ -> 0
    | `Stopping { ticks } | `Started { ticks } -> Atomic.get ticks

let _target_time { time_implementation; t0; frame_duration; ticks } =
  let module Time = (val time_implementation : Liq_time.T) in
  Time.(t0 |+| (frame_duration |*| of_float (float_of_int (Atomic.get ticks))))

let _set_time { time_implementation; t0; frame_duration; ticks } t =
  let module Time = (val time_implementation : Liq_time.T) in
  let delta = Time.(to_float (t |-| t0)) in
  Atomic.set ticks (int_of_float (delta /. Time.to_float frame_duration))

let _after_tick ~clock x =
  Queue.flush_iter x.after_tick (fun fn ->
      check_stopped ();
      fn ());
  let module Time = (val x.time_implementation : Liq_time.T) in
  let end_time = Time.time () in
  let target_time = _target_time x in
  check_stopped ();
  match (x.sync, _self_sync ~clock x, Time.(end_time |<| target_time)) with
    | `Unsynced, _, _ | `Passive, _, _ | `Automatic, true, _ -> ()
    | `Automatic, false, true | `CPU, _, true ->
        if
          Time.(
            of_float (float conf_clock_sleep_latency#get)
            |*| x.frame_duration |<=| (target_time |-| end_time))
        then Time.sleep_until target_time
    | _ ->
        let latency = Time.(end_time |-| target_time) in
        if Time.(x.max_latency |<=| latency) then (
          x.log#severe "Too much latency! Resetting active sources...";
          _set_time x end_time;
          List.iter
            (fun s ->
              match s#source_type with
                | `Passive -> assert false
                | `Active s -> s#reset
                | `Output s -> s#reset)
            (_animated_sources x))
        else if
          Time.(
            x.log_delay_threshold |<=| latency
            && x.log_delay |<=| (end_time |-| Atomic.get x.last_catchup_log))
        then (
          Atomic.set x.last_catchup_log end_time;
          x.log#severe "We must catchup %.2f seconds!"
            Time.(to_float (end_time |-| target_time)))

let started c =
  match Atomic.get (Unifier.deref c).state with
    | `Stopping _ | `Started _ -> true
    | `Stopped _ -> false

let rec active_params c =
  match Atomic.get (Unifier.deref c).state with
    | `Stopping s | `Started s -> s
    | _ when Atomic.get global_stop -> raise Has_stopped
    | _ -> raise Invalid_state

and _activate_pending_sources ~clock x =
  Queue.flush_iter clock.pending_activations (fun s ->
      check_stopped ();
      s#wake_up;
      match s#source_type with
        | `Active _ -> WeakQueue.push x.active_sources s
        | `Output _ -> Queue.push x.outputs s
        | `Passive -> WeakQueue.push x.passive_sources s)

and _tick ~clock x =
  _activate_pending_sources ~clock x;
  let sub_clocks =
    List.map (fun c -> (c, ticks c)) (Queue.elements clock.sub_clocks)
  in
  let sources = _animated_sources x in
  List.iter
    (fun s ->
      check_stopped ();
      try
        match s#source_type with
          | `Output s | `Active s -> s#output
          | _ -> assert false
      with exn when exn <> Has_stopped ->
        let bt = Printexc.get_raw_backtrace () in
        if Queue.is_empty clock.on_error then (
          log#severe "Source %s failed while streaming: %s!\n%s" s#id
            (Printexc.to_string exn)
            (Printexc.raw_backtrace_to_string bt);
          if not allow_streaming_errors#get then Tutils.shutdown 1
          else _detach clock s)
        else Queue.iter clock.on_error (fun fn -> fn exn bt))
    sources;
  Queue.flush_iter x.on_tick (fun fn ->
      check_stopped ();
      fn ());
  List.iter
    (fun (c, old_ticks) ->
      if ticks c = old_ticks then
        _tick ~clock:(Unifier.deref c) (active_params c))
    sub_clocks;
  Atomic.incr x.ticks;
  check_stopped ();
  _after_tick ~clock x;
  check_stopped ();
  Queue.iter clocks start

and _clock_thread ~clock x =
  let has_sources_to_process () =
    0 < Queue.length clock.pending_activations
    || 0 < Queue.length x.outputs
    || 0 < WeakQueue.length x.active_sources
  in
  let on_stop () =
    x.log#info "Clock thread has stopped";
    _cleanup ~clock x;
    Atomic.set clock.state (`Stopped x.sync)
  in
  let run () =
    try
      while
        (match Atomic.get clock.state with `Started _ -> true | _ -> false)
        && (not (Atomic.get global_stop))
        && has_sources_to_process ()
      do
        _tick ~clock x
      done;
      on_stop ()
    with Has_stopped -> on_stop ()
  in
  ignore
    (Tutils.create
       (fun () ->
         x.log#info "Clock thread is starting";
         run ())
       ()
       ("Clock " ^ _id clock))

and _can_start ?(force = false) clock =
  let has_output =
    force
    || Queue.exists clock.pending_activations (fun s ->
           match s#source_type with `Output _ -> true | _ -> false)
  in
  let can_start =
    (not (Atomic.get global_stop)) && (force || Atomic.get clocks_started)
  in
  match (can_start, has_output, Atomic.get clock.state) with
    | true, _, `Stopped (`Passive as sync) | true, true, `Stopped sync ->
        `True sync
    | _ -> `False

and _start ~sync clock =
  Unifier.set clock.id (Lang_string.generate_id (Unifier.deref clock.id));
  let id = _id clock in
  log#important "Starting clock %s with %d source(s) and sync: %s" id
    (Queue.length clock.pending_activations)
    (string_of_sync_mode sync);
  let time_implementation = time_implementation () in
  let module Time = (val time_implementation : Liq_time.T) in
  let frame_duration = Time.of_float (Lazy.force Frame.duration) in
  let max_latency = Time.of_float conf_max_latency#get in
  let log_delay = Time.of_float conf_log_delay#get in
  let log_delay_threshold = Time.of_float conf_log_delay_threshold#get in
  let t0 = Time.time () in
  let last_catchup_log = Atomic.make t0 in
  let x =
    {
      frame_duration;
      log_delay;
      log_delay_threshold;
      max_latency;
      time_implementation;
      t0;
      log = Log.make (["clock"] @ String.split_on_char '.' id);
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
  Queue.iter clock.sub_clocks (fun c -> start c);
  Atomic.set clock.state (`Started x);
  if sync <> `Passive then _clock_thread ~clock x

and start ?force c =
  let clock = Unifier.deref c in
  match _can_start ?force clock with
    | `True sync -> _start ~sync clock
    | `False -> ()

let create ?(stack = []) ?on_error ?(id = "generic") ?(sub_ids = [])
    ?(sync = `Automatic) () =
  let on_error_queue = Queue.create () in
  (match on_error with None -> () | Some fn -> Queue.push on_error_queue fn);
  let c =
    Unifier.make
      {
        id = Unifier.make id;
        sub_ids;
        stack = Atomic.make stack;
        pending_activations = Queue.create ();
        sub_clocks = Queue.create ();
        state = Atomic.make (`Stopped sync);
        on_error = on_error_queue;
      }
  in
  Queue.push clocks c;
  c

let start_pending () =
  let c = Queue.flush_elements clocks in
  let c = List.map (fun c -> (c, Unifier.deref c)) c in
  let c = List.sort_uniq (fun (_, c) (_, c') -> Stdlib.compare c c') c in
  List.iter
    (fun (c, clock) ->
      (match Atomic.get clock.state with
        | `Stopped _ -> (
            match _can_start clock with
              | `True sync -> _start ~sync clock
              | `False -> ())
        | _ -> ());
      Queue.push clocks c)
    c

let () =
  Lifecycle.before_start ~name:"Clocks start" (fun () ->
      Atomic.set clocks_started true;
      start_pending ())

let on_tick c fn =
  let x = active_params c in
  Queue.push x.on_tick fn

let after_tick c fn =
  let x = active_params c in
  Queue.push x.after_tick fn

let after_eval () = if not (Atomic.get global_stop) then start_pending ()

let self_sync c =
  let clock = Unifier.deref c in
  match Atomic.get clock.state with
    | `Started params -> _self_sync ~clock params
    | _ -> false

let activate_pending_sources clock =
  _activate_pending_sources ~clock:(Unifier.deref clock) (active_params clock)

let tick clock = _tick ~clock:(Unifier.deref clock) (active_params clock)

let set_stack c stack =
  ignore (Atomic.compare_and_set (Unifier.deref c).stack [] stack)

let create_sub_clock ~id clock =
  let clock = Unifier.deref clock in
  let sub_clock =
    create ~stack:(Atomic.get clock.stack) ~id
      ~sub_ids:(clock.sub_ids @ ["child"])
      ~sync:`Passive ()
  in
  Queue.push clock.sub_clocks sub_clock;
  sub_clock

let create ?stack ?on_error ?id ?sync () = create ?stack ?on_error ?id ?sync ()

let clocks () =
  List.sort_uniq
    (fun c c' -> Stdlib.compare (Unifier.deref c) (Unifier.deref c'))
    (Queue.elements clocks)
