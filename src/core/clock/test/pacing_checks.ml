open Harness

let wait_until ?(timeout = 5.) condition =
  let deadline = Duppy.time () +. timeout in
  while (not (condition ())) && Duppy.time () < deadline do
    Thread.delay 0.002
  done;
  condition ()

let use_time_source (time_source : Clock.Sync_source.time_source) =
  Hashtbl.replace Clock.Sync_source.time_sources time_source.label time_source;
  Clock.Settings.conf_preferred#set time_source.label

(* The timer answers a short delay until the test moves [now]: the clock then
   reads the time again, as it must after every rest. *)
let manual_time () =
  let now = Atomic.make 0. in
  let time_source =
    {
      Clock.Sync_source.label = "manual";
      now = (fun () -> Atomic.get now);
      wait =
        `Timer (fun target -> if Atomic.get now >= target then 0. else 0.001);
    }
  in
  use_time_source time_source;
  now

let real_time () = use_time_source Clock.Sync_source.builtin
let stream clock = Option.value ~default:0. (Clock.time clock)
let ticks clock = Option.value ~default:0 (Clock.ticks clock)
let figures clock = (Option.get (Clock.status clock).statistics).life
let animator clock = Option.map fst (Clock.status clock).animator
let count clock matching = List.length (List.filter matching (events_of clock))
let is_warning = function Clock.Event.Latency_warning _ -> true | _ -> false
let is_reset = function Clock.Event.Latency_reset _ -> true | _ -> false

let started ?id ?on_error ?(sync = `Automatic) sources =
  let clock = Clock.create ?id ?on_error ~sync () in
  List.iter (attach clock) sources;
  Clock.start ~force:true clock;
  clock

let stopped clock = wait_until (fun () -> Clock.stop_reason clock <> None)

let stop clock =
  Clock.stop clock;
  ignore (stopped clock)

let settled clock =
  let before = ticks clock in
  Thread.delay 0.05;
  ticks clock = before

let latency = 0.1

let paced () =
  let now = manual_time () in
  let output = source `Output in
  let clock = started ~id:"paced" ~sync:`Cpu [output] in
  check "a paced clock that is ahead rests"
    (wait_until (fun () -> settled clock));
  check "a paced clock produces at most the latency plus a frame ahead"
    (ticks clock >= 1 && stream clock <= latency +. frame_duration +. 1e-9);
  check "a clock that rests is animated by a task" (animator clock = Some `Task);
  Atomic.set now 10.;
  check "a late clock catches up" (wait_until (fun () -> stream clock >= 10.));
  ignore (wait_until (fun () -> settled clock));
  check "a paced clock does not drift from its time source"
    (stream clock <= 10. +. latency +. frame_duration +. 1e-9);
  check "lateness at the log threshold logs one warning per log period"
    (count clock is_warning = 1 && count clock is_reset = 0);
  Atomic.set now 200.;
  check "lateness at the maximum resets the clock"
    (wait_until (fun () -> count clock is_reset = 1));
  check "a reset sets the ticks to floor (now / frame duration)"
    (List.exists
       (function
         | Clock.Event.Latency_reset { ticks_after } ->
             ticks_after = int_of_float (floor (200. /. frame_duration))
         | _ -> false)
       (events_of clock)
    && stream clock >= 200.);
  check "a reset resets every animated source" (output#resets = 1);
  stop clock;
  check "a stopped clock task is wound down" (is_stopped clock `Requested)

let device_output ?(id = "device") () =
  let card = Clock.Sync_source.make ~name:id `Self_paced in
  let output = source ~id ~sync:card `Output in
  output#set_on_animate (fun () -> Thread.delay frame_duration);
  output

let device_paced ~tasks () =
  real_time ();
  Clock.Settings.conf_task#set tasks;
  let clock = started ~id:"device" [device_output ()] in
  ignore (wait_until (fun () -> ticks clock > 2));
  let ticks_before = ticks clock and started_at = Duppy.time () in
  Thread.delay 1.;
  let produced = float (ticks clock - ticks_before) *. frame_duration in
  let elapsed = Duppy.time () -. started_at in
  let label fmt =
    Printf.ksprintf
      (fun text ->
        Printf.sprintf "%s (tasks %s)" text (if tasks then "on" else "off"))
      fmt
  in
  check
    (label "K9: a device-paced stream runs in real time: %.02fs in %.02fs"
       produced elapsed)
    (produced /. elapsed > 0.8 && produced /. elapsed < 1.05);
  check
    (label "a clock following a self-paced sync source never rests")
    ((figures clock).rests = 0 && (Clock.status clock).lateness = None);
  check
    (label "it never warns and never resets")
    (count clock is_warning = 0 && count clock is_reset = 0);
  check
    (label "a clock that blocks is animated by a thread")
    (animator clock = Some `Thread && Clock.self_sync clock);
  stop clock;
  Clock.Settings.conf_task#set true

let unsynced () =
  real_time ();
  let clock = started ~id:"unsynced" ~sync:`Unsynced [source `Output] in
  Thread.delay 0.2;
  check "an unsynced clock ticks as fast as it can and never rests"
    (ticks clock > 100
    && (figures clock).rests = 0
    && count clock is_warning = 0);
  check "an unsynced clock is animated by a thread"
    (animator clock = Some `Thread);
  stop clock;
  Clock.Settings.conf_task#set false;
  let clock = started ~id:"tasks.off" ~sync:`Cpu [source `Output] in
  check "with clocks as tasks off, a clock is animated by a thread"
    (animator clock = Some `Thread);
  stop clock;
  Clock.Settings.conf_task#set true

let is_change = function Clock.Event.Animator_change _ -> true | _ -> false

let changing_animator () =
  let now = manual_time () in
  let pacer = source ~id:"pacer" `Active in
  let clock = started ~id:"changing" [source `Output; pacer] in
  ignore (wait_until (fun () -> settled clock));
  pacer#set_sync (Some (Clock.Sync_source.generic ~name:"generic"));
  Atomic.set now 1.;
  check "a generic sync source is followed and changes nothing to the pacing"
    (wait_until (fun () -> Clock.self_sync clock)
    && wait_until (fun () -> settled clock)
    && animator clock = Some `Task
    && stream clock <= 1. +. latency +. frame_duration +. 1e-9);
  let before = ticks clock in
  pacer#set_on_animate (fun () -> Thread.delay frame_duration);
  pacer#set_sync (Some (Clock.Sync_source.make ~name:"card" `Self_paced));
  Atomic.set now 2.;
  check "a blocking sync source that joins moves the clock to a thread"
    (wait_until (fun () -> animator clock = Some `Thread));
  ignore (wait_until (fun () -> ticks clock > before + 20));
  pacer#set_on_animate ignore;
  pacer#set_sync None;
  check "when it leaves, the clock moves back to a task"
    (wait_until (fun () -> animator clock = Some `Task));
  ignore (wait_until (fun () -> settled clock));
  let lateness = Option.get (Clock.status clock).lateness in
  check "a switch leaves the clock neither late nor ahead"
    (lateness <= 1e-9 && lateness >= -.(latency +. frame_duration +. 1e-9));
  check "ticks are continuous across the changes, with no error and no reset"
    (ticks clock > before + 20
    && Clock.started clock
    && count clock is_change = 2
    && count clock is_reset = 0);
  stop clock

let sub_clock_blocks () =
  real_time ();
  let parent = started ~id:"blocked.by.child" [source `Output] in
  let child = sub_clock parent in
  let pacer = source `Active in
  attach child pacer;
  Clock.register ~parent child;
  check "a clock whose sub-clock does not block stays a task"
    (wait_until (fun () -> ticks child > 2) && animator parent = Some `Task);
  pacer#set_on_animate (fun () -> Thread.delay frame_duration);
  pacer#set_sync (Some (Clock.Sync_source.make ~name:"child.card" `Self_paced));
  check "a blocking sync source in a sub-clock moves its parent to a thread"
    (wait_until (fun () -> animator parent = Some `Thread));
  pacer#set_on_animate ignore;
  pacer#set_sync None;
  check "when it leaves, the parent moves back to a task"
    (wait_until (fun () -> animator parent = Some `Task));
  stop parent

let animator_reason clock =
  match (Clock.status clock).animator with Some (_, why) -> why | None -> ""

let lease_starts clock =
  count clock (function Clock.Event.Thread_lease _ -> true | _ -> false)

let flaps clock =
  List.filter_map
    (function
      | Clock.Event.Flap { gap; lease } -> Some (gap, lease) | _ -> None)
    (events_of clock)

let lease = 0.5

(* A source that sets and drops a blocking sync source on [clock]. *)
let flapping_source clock pacer =
  let card = Clock.Sync_source.make ~name:"flapping.card" `Self_paced in
  let on_thread () = animator clock = Some `Thread in
  let on_task () = animator clock = Some `Task in
  let join () =
    pacer#set_on_animate (fun () -> Thread.delay frame_duration);
    pacer#set_sync (Some card);
    ignore (wait_until on_thread)
  in
  let leave () =
    pacer#set_on_animate ignore;
    pacer#set_sync None
  in
  (join, leave, on_thread, on_task)

let leased_clock id =
  let pacer = source ~id:(id ^ ".source") `Active in
  let clock = started ~id [source `Output; pacer] in
  ignore (wait_until (fun () -> ticks clock > 2));
  (clock, flapping_source clock pacer)

let no_flap () =
  let clock, (join, leave, _, on_task) = leased_clock "steady" in
  let back_at_once () =
    join ();
    leave ();
    let back = wait_until ~timeout:0.3 on_task in
    Thread.delay (lease +. 0.2);
    back
  in
  let first = back_at_once () in
  let second = back_at_once () in
  let third = back_at_once () in
  check "a clock that has seen no flap moves back at once, every time"
    (first && second && third && flaps clock = [] && lease_starts clock = 0);
  stop clock

let flap () =
  let clock, (join, leave, on_thread, on_task) = leased_clock "leased" in
  join ();
  leave ();
  ignore (wait_until ~timeout:0.3 on_task);
  Thread.delay 0.15;
  join ();
  check
    "a source that returns within the thread lease is logged as a flap, once"
    (match flaps clock with
      | [(gap, logged)] -> gap > 0.1 && gap <= lease && logged = lease
      | _ -> false);
  for _ = 1 to 4 do
    leave ();
    Thread.delay 0.15;
    join ();
    Thread.delay 0.15
  done;
  check "a leased clock stays on its thread while its source flaps"
    (on_thread () && count clock is_change = 3 && List.length (flaps clock) = 1);
  check "the start of each lease is logged" (lease_starts clock = 4);
  leave ();
  Thread.delay 0.05;
  check "a clock in its lease is on a thread and says how long is left"
    (on_thread ()
    && String.starts_with ~prefix:"lease: " (animator_reason clock));
  let ticks_before = ticks clock and started_at = Duppy.time () in
  Thread.delay 0.2;
  let produced = float (ticks clock - ticks_before) *. frame_duration in
  let elapsed = Duppy.time () -. started_at in
  check
    (Printf.sprintf
       "during a lease the clock rests in real time: %.02fs in %.02fs" produced
       elapsed)
    (produced /. elapsed > 0.5 && produced /. elapsed < 1.6);
  check "a leased clock whose lease has elapsed moves back to a task"
    (wait_until ~timeout:1.5 on_task && count clock is_change = 4);
  Thread.delay 0.1;
  let leases = lease_starts clock in
  join ();
  leave ();
  check "back on a task, the clock starts over without a lease"
    (wait_until ~timeout:0.3 on_task
    && lease_starts clock = leases
    && List.length (flaps clock) = 1);
  Thread.delay 0.15;
  join ();
  Clock.Settings.conf_thread_lease#set 0.;
  leave ();
  check "with a thread lease of 0, a leased clock moves back at once"
    (List.length (flaps clock) = 2 && wait_until ~timeout:0.3 on_task);
  stop clock

let thread_lease () =
  real_time ();
  Clock.Settings.conf_thread_lease#set lease;
  no_flap ();
  flap ();
  Clock.Settings.conf_thread_lease#set 5.

let sync_error_in_each_mode () =
  real_time ();
  List.iter
    (fun sync ->
      let errors = Atomic.make 0 in
      let second = source ~id:"second" `Active in
      let clock =
        started ~sync
          ~on_error:(fun _ _ -> Atomic.incr errors)
          [
            source `Output;
            source ~sync:(Clock.Sync_source.generic ~name:"one") `Active;
            second;
          ]
      in
      ignore (wait_until (fun () -> ticks clock > 2));
      second#set_sync (Some (Clock.Sync_source.generic ~name:"two"));
      check
        (Printf.sprintf "two sync sources are a sync error on a %s clock"
           (Clock.string_of_sync_mode sync))
        (wait_until (fun () -> Atomic.get errors = 1) && Clock.started clock);
      stop clock)
    [`Automatic; `Cpu; `Unsynced]

let failing ~tasks () =
  real_time ();
  Clock.Settings.conf_task#set tasks;
  let good = source `Output and bad = source ~id:"bad" `Output in
  let id = if tasks then "failing.task" else "failing.thread" in
  let clock = started ~id [good; bad] in
  let child = sub_clock clock in
  Clock.register ~parent:clock child;
  ignore (wait_until (fun () -> ticks clock > 2));
  bad#set_on_animate (fun () -> raise Boom);
  check
    (Printf.sprintf "a clock animated by a %s fails like any other"
       (if tasks then "task" else "thread"))
    (stopped clock && failed clock
    && registered clock = 1
    && good#awake = 0
    && Clock.stop_reason child <> None
    && policy_calls_for clock = 1);
  Clock.Settings.conf_task#set true

(* The double of an audio server: its time is the stream it has consumed, and
   a wait ends one period before its target. *)
let server () =
  let m = Mutex.create () and c = Condition.create () in
  let consumed = ref 0. and ended = ref false in
  let locked fn = Mutex.protect m fn in
  let until ~interrupted target =
    locked (fun () ->
        while
          !consumed +. frame_duration < target -. 1e-9
          && (not !ended)
          && not (interrupted ())
        do
          Condition.wait c m
        done;
        if !ended then `Ended else if interrupted () then `Interrupted else `Due)
  in
  let interrupt () = locked (fun () -> Condition.broadcast c) in
  let time_source =
    {
      Clock.Sync_source.label = "server";
      now = (fun () -> locked (fun () -> !consumed));
      wait = `Blocking { until; interrupt };
    }
  in
  let sync_source =
    Clock.Sync_source.make ~name:"server"
      (`Timed
         {
           time_source = Some time_source;
           latency = Some 0.;
           max_latency = Some 0.5;
         })
  in
  let advance periods =
    for _ = 1 to periods do
      locked (fun () ->
          consumed := !consumed +. frame_duration;
          Condition.broadcast c);
      Thread.delay 0.002
    done
  in
  let stop () =
    locked (fun () ->
        ended := true;
        Condition.broadcast c)
  in
  (sync_source, advance, stop)

let server_driven () =
  real_time ();
  let sync, advance, stop_server = server () in
  let first = started ~id:"server.1" [source ~sync `Output] in
  let second = started ~id:"server.2" [source `Output] in
  ignore (wait_until (fun () -> ticks second > 2));
  attach second (source ~sync `Active);
  check "K10: a server-driven source can join a clock that is running"
    (wait_until (fun () -> Clock.self_sync second)
    && animator second = Some `Thread);
  ignore (wait_until (fun () -> settled first && settled second));
  let first_before = ticks first and second_before = ticks second in
  advance 200;
  ignore (wait_until (fun () -> settled first && settled second));
  let close clock before = abs (ticks clock - before - 200) <= 2 in
  check "K10: one tick per server period and no drift"
    (close first first_before);
  check "K10: two clocks on one server each keep their own pace"
    (close second second_before);
  let policy_before = List.length !policy_calls in
  stop_server ();
  check "K10: ending the server stops its clocks, which is not a failure"
    (stopped first && stopped second
    && is_stopped first `Sync_source_ended
    && is_stopped second `Sync_source_ended
    && List.length !policy_calls = policy_before)

let unsynced_and_due_task () =
  real_time ();
  let workers = Domain.recommended_domain_count () in
  let clocks =
    List.init workers (fun _ -> started ~sync:`Unsynced [source `Output])
  in
  Thread.delay 0.1;
  let ran = Atomic.make 0. in
  let submitted = Duppy.time () in
  Duppy.Task.add Tutils.scheduler
    {
      Duppy.Task.priority = `Blocking;
      events = [`Delay 0.2];
      handler =
        (fun _ ->
          Atomic.set ran (Duppy.time ());
          []);
    };
  ignore (wait_until (fun () -> Atomic.get ran > 0.));
  let delay = Atomic.get ran -. submitted in
  check
    (Printf.sprintf
       "K12: with %d unsynced clocks, a task due in 0.2s runs after %.03fs"
       workers delay)
    (delay >= 0.2 && delay < 0.3);
  List.iter stop clocks

let breakdown () =
  real_time ();
  let slow = source ~id:"slow" `Output in
  slow#set_on_animate (fun () -> Thread.delay (frame_duration *. 1.5));
  let warnings = ref [] in
  let clock = Clock.create ~id:"slow.clock" ~sync:`Cpu () in
  Clock.on_event (fun (event : Clock.Event.event) ->
      match event.kind with
        | Latency_warning { since_last } when event.clock = "slow.clock" ->
            warnings := (Duppy.time (), since_last) :: !warnings
        | _ -> ());
  attach clock slow;
  Clock.start ~force:true clock;
  check "a clock that produces slower than real time warns again"
    (wait_until ~timeout:10. (fun () -> List.length !warnings >= 2));
  (match !warnings with
    | (last, figures) :: (previous, _) :: _ ->
        let span = last -. previous in
        let accounted =
          figures.producing +. figures.waiting +. figures.resting
          +. figures.released +. figures.no_worker
        in
        check
          (Printf.sprintf
             "a latency warning's breakdown adds up: %.03fs of %.03fs" accounted
             span)
          (accounted > 0.9 *. span && accounted <= span +. 0.01);
        check "it names the slowest source"
          (figures.slowest_source = Some "slow")
    | _ -> ());
  stop clock

let run () =
  Tutils.start ();
  paced ();
  device_paced ~tasks:true ();
  device_paced ~tasks:false ();
  unsynced ();
  changing_animator ();
  sub_clock_blocks ();
  thread_lease ();
  sync_error_in_each_mode ();
  failing ~tasks:true ();
  failing ~tasks:false ();
  server_driven ();
  unsynced_and_due_task ();
  breakdown ()
