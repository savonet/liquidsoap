open Settings
open Status
open State
open Life
module Event = Event
module Settings = Settings
module Status = Status
module Sync_source = Sync_source

let on_event = Event.on_event
let string_of_sync_mode = string_of_sync_mode
let sync_mode_of_string = sync_mode_of_string

type sync_mode = Status.sync_mode
type owner = Status.owner = { kind : string; id : string }
type animator = Status.animator

type failure = Status.failure = {
  error : exn;
  backtrace : Printexc.raw_backtrace;
  source : string option;
}

type stop_reason = Status.stop_reason
type activity = Status.activity
type figures = Status.figures

exception Conflict = State.Conflict
exception Loop = State.Loop
exception Controller_conflict = State.Controller_conflict
exception Sync_error = State.Sync_error

let sync_error = State.sync_error

exception Not_running = State.Not_running
exception Cannot_start = State.Cannot_start
exception Not_a_sub_clock = State.Not_a_sub_clock
exception Stop_signal = State.Stop_signal

type activation = State.activation
type active = State.active
type source_type = [ `Passive | `Active of active | `Output of active ]
type source = State.source

type reported = State.reported = {
  sync_source : string;
  source : string;
  stack : Pos.t list;
}

type t = State.t

let equal = equal
let compare = compare
let name t = clock_name (get t)
let id t = Atomic.get (get t).id
let set_id t id = set_clock_id (get t) id
let sync_mode t = (get t).sync_mode
let parent t = Atomic.get (get t).parent

let set_stack t stack =
  let c = get t in
  if Atomic.get c.stack = [] then Atomic.set c.stack stack

let streaming t = Atomic.get (get t).streaming
let ticks t = Option.map (fun st -> Atomic.get st.ticks) (streaming t)
let time t = Option.map stream_time (streaming t)

let time_implementation () =
  Option.value ~default:Liq_time.unix
    (Hashtbl.find_opt Liq_time.implementations Settings.conf_preferred#get)

let tick_count t = Option.value ~default:0 (ticks t)

let self_sync t =
  match streaming t with
    | Some st -> (Atomic.get st.pace).followed <> None
    | None -> false

let pulled t =
  match streaming t with Some st -> Atomic.get st.pulled | None -> false

let stop_reason t =
  let c = get t in
  if state c = `Stopped then Some (Atomic.get c.stop_reason) else None

let started t = state (get t) = `Started
let pending t = Atomic.get (get t).pending
let on_tick t fn = push (running_streaming (get t)).on_tick fn
let after_tick t fn = push (running_streaming (get t)).after_tick fn
let set_failure_policy = set_failure_policy
let attach = Activation.attach
let detach = Activation.detach
let stop = stop
let tick = tick
let activate_pending = activate_pending
let start = start
let start_pass = start_pass
let start_scope = start_scope
let application_start = application_start
let register = register
let deregister = deregister
let sub_clocks = sub_clocks
let unify = Unify.unify
let status t = Inspect.clock_status (get t)

let create ?(stack = []) ?on_error ?id ?(sync = `Automatic) ?parent ?owner () =
  (match (sync, parent, owner) with
    | `Passive, None, None ->
        invalid_arg "Clock.create: a passive clock needs a parent or an owner"
    | `Passive, _, _ | _, None, None -> ()
    | _ -> invalid_arg "Clock.create: only a passive clock has a controller");
  let c =
    {
      identity = Atomic.fetch_and_add Registry.identities 1;
      id = Atomic.make None;
      sync_mode = sync;
      parent = Atomic.make parent;
      owner;
      stack = Atomic.make stack;
      state = Atomic.make `Stopped;
      stop_reason = Atomic.make `Never_started;
      pending = Atomic.make [];
      subs = Atomic.make [];
      error_handlers = Atomic.make (Option.to_list on_error);
      streaming = Atomic.make None;
      ticking = Atomic.make false;
      activated = Atomic.make 0;
      self = Atomic.make None;
      log = Atomic.make None;
    }
  in
  Registry.watch c;
  Option.iter (set_clock_id c) id;
  if parent = None then Registry.wait c;
  let self = Unifier.make c in
  Atomic.set c.self (Some self);
  self

let clocks () =
  List.map handle
    (Registry.by_creation
       (Registry.waiting_clocks () @ Registry.running_clocks ()))

let statuses () =
  List.map Inspect.clock_status
    (Registry.by_creation
       (Registry.waiting_clocks () @ Registry.running_clocks ()))

let shutdown () =
  Atomic.set global_stop true;
  List.iter (fun c -> stop_clock c `Global_stop) (Registry.running_clocks ());
  let deadline = Duppy.time () +. conf_shutdown_wait#get in
  while Registry.running_clocks () <> [] && Duppy.time () < deadline do
    Thread_utils.delay 0.01
  done;
  List.iter
    (fun c ->
      Option.iter
        (fun st ->
          State.emit c
            (Still_running
               {
                 activity = Atomic.get st.activity;
                 slowest_source = (Atomic.get st.life).slowest_source;
               }))
        (Atomic.get c.streaming))
    (Registry.running_clocks ())

let () =
  Lifecycle.before_start ~name:"clocks start" application_start;
  Lifecycle.after_main_loop ~name:"clocks global stop" (fun () ->
      Atomic.set global_stop true);
  Lifecycle.before_core_shutdown ~name:"clocks shutdown" shutdown
