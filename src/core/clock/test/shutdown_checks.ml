open Harness
open Pacing_checks

(* The global stop cannot be undone, so each of these runs in a process of its
   own. *)
let orderly () =
  Tutils.start ();
  real_time ();
  let task = started ~id:"task" ~sync:`Cpu [source `Output] in
  let thread = started ~id:"thread" ~sync:`Unsynced [source `Output] in
  let device = started ~id:"device" [device_output ()] in
  use_time_source
    {
      Clock.Sync_source.label = "frozen";
      now = (fun () -> 0.);
      wait = `Timer (fun _ -> 3.);
    };
  let resting = started ~id:"resting" ~sync:`Cpu [source `Output] in
  real_time ();
  let by_hand = passive ~id:"by.hand" () in
  let failing = source `Output in
  let failed_clock = started ~id:"failed" ~sync:`Cpu [failing] in
  ignore (wait_until (fun () -> settled task && settled resting));
  failing#set_on_animate (fun () -> raise Boom);
  ignore (stopped failed_clock);
  let idle = Clock.create () in
  let started_at = Duppy.time () in
  Clock.shutdown ();
  let took = Duppy.time () -. started_at in
  let clocks = [task; thread; device; by_hand; resting] in
  check
    (Printf.sprintf "K13: every clock is stopped when shutdown returns (%.03fs)"
       took)
    (List.for_all (fun clock -> is_stopped clock `Global_stop) clocks);
  check "K13: a resting clock stops within one tick plus one rest" (took < 0.5);
  check "a clock started explicitly is stopped at shutdown too"
    (is_stopped by_hand `Global_stop);
  check "a failed clock keeps its reason and is not waited for"
    (failed failed_clock);
  raises "after the global stop nothing starts"
    (function
      | Clock.Cannot_start { reason = `Global_stop } -> true | _ -> false)
    (fun () -> Clock.start ~force:true idle);
  let before = event_count () in
  check "after the global stop a stopped clock still reads as no value"
    (Clock.ticks idle = None
    && Clock.time idle = None
    && (Clock.status idle).lateness = None
    && event_count () = before);
  raises "ticking after the global stop fails with the stop signal"
    (( = ) Clock.Stop_signal) (fun () -> Clock.tick by_hand)

let stuck () =
  Tutils.start ();
  real_time ();
  Clock.Settings.conf_shutdown_wait#set 0.3;
  Clock.Settings.conf_max_latency#set 1000.;
  let output = source ~id:"stuck.output" `Output in
  let clock = started ~id:"stuck" ~sync:`Cpu [output] in
  ignore (wait_until (fun () -> settled clock));
  output#set_on_animate (fun () -> Thread.delay 2.);
  Thread.delay 0.3;
  let started_at = Duppy.time () in
  Clock.shutdown ();
  let took = Duppy.time () -. started_at in
  check
    (Printf.sprintf
       "the shutdown wait bounds the shutdown, whatever the maximum latency \
        (%.03fs)"
       took)
    (took >= 0.3 && took < 0.6);
  check "a clock still running at shutdown is logged with what it was doing"
    (List.exists
       (function
         | Clock.Event.Still_running { activity = `Ticking _ } -> true
         | _ -> false)
       (events_of clock))
