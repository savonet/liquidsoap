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

let conf_clock_latency =
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
  mutable is_self_sync : bool;
  log : Log.t;
  time_implementation : Liq_time.implementation;
  t0 : Liq_time.t;
  log_delay : Liq_time.t;
  log_delay_threshold : Liq_time.t;
  frame_duration : Liq_time.t;
  max_latency : Liq_time.t;
  last_catchup_log : Liq_time.t Atomic.t;
  outputs : (activation * source) Queue.t;
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
  id : string option Unifier.t;
  controller : controller Unifier.t;
  stack : Pos.t list Atomic.t;
  state : state Atomic.t;
  pending_activations : source Queue.t;
  sub_clocks : t Queue.t;
  on_error : (exn -> Printexc.raw_backtrace -> unit) Queue.t;
}

and t = clock Unifier.t
and controller = [ `None | `Clock of t | `Other of string * < id : string > ]

let string_of_state = function
  | `Stopping _ -> "stopping"
  | `Started _ -> "started"
  | `Stopped _ -> "stopped"

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
    ( Unifier.deref id,
      meaningful_pending_id (Queue.elements pending_activations) )
  with
    | Some id, _ -> id
    | None, Some id -> id
    | _ -> "generic"

let _id { id; pending_activations } = get_id ~pending_activations id
let id c = _id (Unifier.deref c)
let generate_id = Lang_string.generate_id ~category:"clock"

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
        with _ -> false)
    | _ -> false

let _set_id _clock new_id =
  if Unifier.deref _clock.id <> Some new_id then
    Unifier.set _clock.id (Some (generate_id new_id))

let set_id clock new_id = _set_id (Unifier.deref clock) new_id

let attach c s =
  let clock = Unifier.deref c in
  Queue.push clock.pending_activations s

let _detach x s =
  Queue.filter_out x.pending_activations (fun s' -> s == s');
  match Atomic.get x.state with
    | `Stopped _ -> ()
    | `Stopping { outputs; active_sources; passive_sources }
    | `Started { outputs; active_sources; passive_sources } ->
        Queue.filter_out outputs (fun (a, s') ->
            if s == s' then (
              s#sleep a;
              true)
            else false);
        WeakQueue.filter_out active_sources (fun s' -> s == s');
        WeakQueue.filter_out passive_sources (fun s' -> s == s')

let detach c s = _detach (Unifier.deref c) s

let active_sources c =
  match Atomic.get (Unifier.deref c).state with
    | `Started { active_sources } | `Stopping { active_sources } ->
        WeakQueue.elements active_sources
    | _ -> []

let outputs c =
  match Atomic.get (Unifier.deref c).state with
    | `Started { outputs } | `Stopping { outputs } ->
        List.map snd (Queue.elements outputs)
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
        @ List.map snd (Queue.elements outputs)
    | _ -> []

(* Return the clock effective sync. Stopped clocks can
   be unified with any active type clocks so [`Stopped _] returns
   [`Stopped]. *)
let _sync ?(pending = false) x =
  match Atomic.get x.state with
    | `Stopped p when pending -> (p :> sync_mode)
    | `Stopped _ -> `Stopped
    | `Stopping _ -> `Stopping
    | `Started { sync } -> (sync :> sync_mode)

let sync c = _sync (Unifier.deref c)
let pending_clocks = WeakQueue.create ()
let clocks = Queue.create ()

let rec has_stopped ~clear_controller ~clock ~c x =
  Queue.iter x.outputs (fun (a, o) -> try o#sleep a with _ -> ());
  Queue.iter clock.sub_clocks stop;
  Queue.filter_out clocks (fun c -> Unifier.deref c == clock);
  if clear_controller then Unifier.set clock.controller `None;
  Atomic.set clock.state (`Stopped x.sync);
  WeakQueue.push pending_clocks c;
  x.log#important "Clock stopped"

and stop c =
  let clock = Unifier.deref c in
  match Atomic.get clock.state with
    | `Stopped _ | `Stopping _ -> ()
    | `Started ({ sync = `Passive } as x) ->
        has_stopped ~clear_controller:false ~clock ~c x
    | `Started x ->
        x.log#important "Clock stopping";
        Atomic.set clock.state (`Stopping x)

let clocks_started = Atomic.make false
let global_stop = Atomic.make false

exception Has_stopped

let[@inline] check_stopped () = if Atomic.get global_stop then raise Has_stopped

let _descr clock =
  Printf.sprintf "clock(id=%s,sync=%s%s)" (_id clock)
    (string_of_sync_mode (_sync clock))
    (match Atomic.get clock.state with
      | `Stopped pending ->
          Printf.sprintf ",pending=%s"
            (string_of_sync_mode (pending :> sync_mode))
      | _ -> "")

let descr clock = _descr (Unifier.deref clock)

let rec check_sub_clocks ~pos _c _c' =
  Queue.iter _c.sub_clocks (fun x ->
      let _x = Unifier.deref x in
      if _x == _c' then
        raise (Liquidsoap_lang.Error.Clock_loop (pos, _descr _c, _descr _c'));
      check_sub_clocks ~pos _x _c')

let unify =
  let _unify ~pos c c' =
    let clock = Unifier.deref c in
    let clock' = Unifier.deref c' in
    check_sub_clocks ~pos clock clock';
    check_sub_clocks ~pos clock' clock;
    (match
       (Unifier.deref clock.controller, Unifier.deref clock'.controller)
     with
      | _, `None -> Unifier.(clock'.controller <-- clock.controller)
      | _ -> ());
    Queue.flush_iter clock.pending_activations
      (Queue.push clock'.pending_activations);
    Queue.flush_iter clock.sub_clocks (Queue.push clock'.sub_clocks);
    Queue.flush_iter clock.on_error (Queue.push clock'.on_error);
    (match (Unifier.deref clock.id, Unifier.deref clock'.id) with
      | None, None -> Unifier.(clock.id <-- clock'.id)
      | Some _, None -> Unifier.(clock'.id <-- clock.id)
      | None, Some _ -> Unifier.(clock.id <-- clock'.id)
      | Some _, Some id ->
          log#info "Clocks %s and %s both have id already set. Setting id to %s"
            (descr c) (descr c') id;
          Unifier.(clock.id <-- clock'.id));
    Unifier.(c <-- c');
    Queue.filter_out clocks (fun el -> el == c);
    WeakQueue.filter_out pending_clocks (fun el -> el == c)
  in
  let rec unify ~pos c c' =
    let _c = Unifier.deref c in
    let _c' = Unifier.deref c' in
    let _controller = Unifier.deref _c.controller in
    let _controller' = Unifier.deref _c'.controller in
    if not (unifiable_controller ~unify:(unify ~pos) _controller _controller')
    then
      raise
        Liquidsoap_lang.Error.(
          Clock_main
            {
              pos;
              left_main = string_of_controller _controller;
              left_child = descr c;
              right_main = string_of_controller _controller';
              right_child = descr c';
            });
    match (_c == _c', Atomic.get _c.state, Atomic.get _c'.state) with
      | true, _, _ -> ()
      | _, `Stopped s, `Stopped s' when s = s' -> _unify ~pos c c'
      | _, `Stopped s, _
        when s = `Automatic || (s :> sync_mode) = _sync ~pending:true _c' ->
          _unify ~pos c c'
      | _, _, `Stopped s'
        when s' = `Automatic || _sync ~pending:true _c = (s' :> sync_mode) ->
          _unify ~pos c' c
      | _ ->
          raise (Liquidsoap_lang.Error.Clock_conflict (pos, descr c, descr c'))
  in
  unify

let () =
  Lifecycle.before_core_shutdown ~name:"Clocks stop" (fun () ->
      Atomic.set global_stop true;
      Queue.iter clocks (fun c -> if sync c <> `Passive then stop c))

let _animated_sources { outputs; active_sources } =
  List.map snd (Queue.elements outputs) @ WeakQueue.elements active_sources

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
  let is_self_sync = List.length self_sync_sources = 1 in
  if x.is_self_sync <> is_self_sync && x.sync = `Automatic then (
    x.log#important "Switching to %sself-sync mode"
      (if is_self_sync then "" else "non ");
    x.is_self_sync <- is_self_sync);
  is_self_sync

let ticks c =
  match Atomic.get (Unifier.deref c).state with
    | `Stopped _ -> 0
    | `Stopping { ticks } | `Started { ticks } -> Atomic.get ticks

let _time { time_implementation; frame_duration; ticks } =
  let module Time = (val time_implementation : Liq_time.T) in
  Time.(frame_duration |*| of_float (float_of_int (Atomic.get ticks)))

let _target_time ({ time_implementation; t0 } as c) =
  let module Time = (val time_implementation : Liq_time.T) in
  Time.(t0 |+| _time c)

let _set_time { time_implementation; t0; frame_duration; ticks } t =
  let module Time = (val time_implementation : Liq_time.T) in
  let delta = Time.(to_float (t |-| t0)) in
  Atomic.set ticks (int_of_float (delta /. Time.to_float frame_duration))

let _after_tick ~self_sync x =
  Queue.flush_iter x.after_tick (fun fn ->
      check_stopped ();
      fn ());
  let module Time = (val x.time_implementation : Liq_time.T) in
  let end_time = Time.time () in
  let target_time = _target_time x in
  check_stopped ();
  match (x.sync, self_sync, Time.(end_time |<| target_time)) with
    | `Unsynced, _, _ | `Passive, _, _ | `Automatic, true, _ -> ()
    | `Automatic, false, true | `CPU, _, true ->
        if
          Time.(of_float conf_clock_latency#get |<=| (target_time |-| end_time))
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
          x.log#severe
            "Latency is too high: we must catchup %.2f seconds! Check if your \
             system can process your stream fast enough (CPU usage, disk \
             access, etc) or if your stream should be self-sync (can happen \
             when using `input.ffmpeg`). Refer to the latency control section \
             of the documentation for more info."
            Time.(to_float (end_time |-| target_time)))

let started c =
  match Atomic.get (Unifier.deref c).state with
    | `Stopping _ | `Started _ -> true
    | `Stopped _ -> false

let wrap_errors clock fn s =
  check_stopped ();
  try fn s
  with exn when exn <> Has_stopped ->
    let bt = Printexc.get_raw_backtrace () in
    Printf.printf "Error: %s\n%s\n%!" (Printexc.to_string exn)
      (Printexc.raw_backtrace_to_string bt);
    log#severe "Source %s failed while streaming: %s!\n%s" s#id
      (Printexc.to_string exn)
      (Printexc.raw_backtrace_to_string bt);
    _detach clock s;
    if Queue.length clock.on_error > 0 then
      Queue.iter clock.on_error (fun fn -> fn exn bt)
    else Printexc.raise_with_backtrace exn bt

let pretty_sources ~clock x =
  let rec build_entry clock x =
    let sub_clocks =
      List.filter_map
        (fun sub ->
          let sub_clock = Unifier.deref sub in
          match Atomic.get sub_clock.state with
            | `Started sub_x | `Stopping sub_x ->
                Some (build_entry sub_clock sub_x)
            | _ -> None)
        (Queue.elements clock.sub_clocks)
    in
    let src s =
      Clock_utils.
        { id = s#id; activations = List.map (fun a -> a#id) s#activations }
    in
    Clock_utils.
      {
        name = _id clock;
        outputs = List.map (fun (_, s) -> src s) (Queue.elements x.outputs);
        active = List.map src (WeakQueue.elements x.active_sources);
        passive = List.map src (WeakQueue.elements x.passive_sources);
        sub_clocks;
      }
  in
  Clock_utils.format_clock (build_entry clock x)

let rec active_params c =
  match Atomic.get (Unifier.deref c).state with
    | `Stopping s | `Started s -> s
    | _ when Atomic.get global_stop -> raise Has_stopped
    | s ->
        log#critical "Clock %s has invalid state: %s" (id c) (string_of_state s);
        raise Invalid_state

and _activate_pending_sources ~clock x =
  let pending_sources = Queue.length clock.pending_activations in
  Queue.flush_iter clock.pending_activations
    (wrap_errors clock (fun s ->
         match s#source_type with
           | `Active _ -> WeakQueue.push x.active_sources s
           | `Output _ ->
               let a = s#wake_up (s :> source) in
               Queue.push x.outputs (a, s)
           | `Passive -> WeakQueue.push x.passive_sources s));
  if 0 < pending_sources then (
    let total_sources =
      Queue.length x.outputs
      + WeakQueue.length x.active_sources
      + WeakQueue.length x.passive_sources
    in
    if
      (total_sources - pending_sources) / conf_leak_warning#get
      < total_sources / conf_leak_warning#get
    then (
      x.log#severe
        "There are currently %d sources, possible source leak! Please check \
         that you don't have a loop creating multiple sources."
        total_sources;
      x.log#important "Current sources:\n%s" (pretty_sources ~clock x)))

and _tick ~clock x =
  let sub_clocks =
    List.map (fun c -> (c, ticks c)) (Queue.elements clock.sub_clocks)
  in
  _activate_pending_sources ~clock x;
  let sources = _animated_sources x in
  List.iter
    (wrap_errors clock (fun s ->
         match s#source_type with
           | `Output s | `Active s -> s#output
           | _ -> assert false))
    sources;
  Queue.flush_iter x.on_tick (fun fn ->
      check_stopped ();
      fn ());
  let self_sync = _self_sync ~clock x in
  check_stopped ();
  List.iter
    (fun (c, old_ticks) ->
      if ticks c = old_ticks then
        _tick ~clock:(Unifier.deref c) (active_params c))
    sub_clocks;
  Atomic.incr x.ticks;
  check_stopped ();
  _after_tick ~self_sync x;
  check_stopped ()

and _clock_thread ~clock ~c x =
  let has_sources_to_process () =
    0 < Queue.length clock.pending_activations
    || 0 < Queue.length x.outputs
    || 0 < WeakQueue.length x.active_sources
  in
  let clock_stopped () =
    match Atomic.get clock.state with `Started _ -> true | _ -> false
  in
  let global_stop () = Atomic.get global_stop in
  let on_stop () =
    let reasons =
      [
        ("clock stopped", clock_stopped ());
        ("global stop", global_stop ());
        ("no more sources to process", has_sources_to_process ());
      ]
    in
    let reasons =
      List.fold_left
        (fun reasons -> function
          | _, false -> reasons | reason, true -> reason :: reasons)
        [] reasons
    in
    x.log#important "Clock thread has stopped: %s." (String.concat ", " reasons);
    has_stopped ~clear_controller:true ~clock ~c x
  in
  let run () =
    try
      while
        clock_stopped () && (not (global_stop ())) && has_sources_to_process ()
      do
        _tick ~clock x
      done;
      on_stop ()
    with Has_stopped -> on_stop ()
  in
  Tutils.create
    (fun () ->
      x.log#info "Clock thread is starting";
      run ())
    ()
    ("Clock " ^ _id clock)

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

and _start ?force ~sync ~c clock =
  _set_id clock (_id clock);
  let id = _id clock in
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
  let _controller = Unifier.deref clock.controller in
  let controlled_by =
    if _controller = `None then ""
    else Printf.sprintf " controlled by %s" (string_of_controller _controller)
  in
  log#important "Starting %s clock %s%s with sources: %s%s" top_level id
    controlled_by sources sync_mode;
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
      is_self_sync = false;
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
  Queue.iter clock.sub_clocks (fun c -> start ?force c);
  Atomic.set clock.state (`Started x);
  if sync <> `Passive then (
    let th = _clock_thread ~clock ~c x in
    match _controller with
      | `None ->
          let controller =
            object
              method id = string_of_int (Thread.id th)
            end
          in
          Unifier.set clock.controller (`Other ("thread", controller))
      | _ -> raise Invalid_state)

and start ?force c =
  let clock = Unifier.deref c in
  match _can_start ?force clock with
    | `True sync -> _start ?force ~sync ~c clock
    | `False -> ()

let add_pending_clock =
  (* Make sure that we're not collecting clocks between
     the time they have sources attached to them and before
     we get a chance to call [start_pending]. *)
  let finalise c =
    let clock = Unifier.deref c in
    match _can_start clock with
      | `True sync when sync <> `Passive ->
          _start ~sync ~c clock;
          Queue.push clocks c
      | _ -> ()
  in
  fun c ->
    Gc.finalise finalise c;
    WeakQueue.push pending_clocks c

let create ?(stack = []) ?(controller = `None) ?on_error ?id
    ?(sync = `Automatic) () =
  let on_error_queue = Queue.create () in
  (match on_error with None -> () | Some fn -> Queue.push on_error_queue fn);
  let id = Unifier.make (Option.map generate_id id) in
  let pending_activations = Queue.create () in
  let c =
    Unifier.make
      {
        id;
        controller = Unifier.make controller;
        stack = Atomic.make stack;
        pending_activations;
        sub_clocks = Queue.create ();
        state = Atomic.make (`Stopped sync);
        on_error = on_error_queue;
      }
  in
  if sync <> `Passive then add_pending_clock c;
  c

let time c =
  match active_params c with
    | { time_implementation } as c ->
        let module Time = (val time_implementation : Liq_time.T) in
        Time.to_float (_time c)
    | exception Invalid_state -> -1.

let start_pending () =
  let c = WeakQueue.flush_elements pending_clocks in
  let c = List.map (fun c -> (c, Unifier.deref c)) c in
  let c = List.sort_uniq (fun (_, c) (_, c') -> Stdlib.compare c c') c in
  List.iter
    (fun (c, clock) ->
      match Atomic.get clock.state with
        | `Stopped _ -> (
            match _can_start clock with
              | `True `Passive -> ()
              | `True sync ->
                  _start ~sync ~c clock;
                  Queue.push clocks c
              | `False -> WeakQueue.push pending_clocks c)
        | _ -> ())
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

let create_sub_clock ?controller ~id clock =
  let controller = Option.value ~default:(`Clock clock) controller in
  let _clock = Unifier.deref clock in
  let sub_clock =
    create ~stack:(Atomic.get _clock.stack) ~id ~controller ~sync:`Passive ()
  in
  Queue.push _clock.sub_clocks sub_clock;
  sub_clock

let create ?stack ?controller ?on_error ?id ?sync () =
  create ?stack ?controller ?on_error ?id ?sync ()

let clocks () =
  List.sort_uniq
    (fun c c' -> Stdlib.compare (Unifier.deref c) (Unifier.deref c'))
    (WeakQueue.elements pending_clocks @ Queue.elements clocks)

let sub_clocks c =
  let clock = Unifier.deref c in
  Queue.elements clock.sub_clocks

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
