open Harness
open Pacing_checks

(* The legacy pool is the one whose number of workers a test can choose. *)
let start_pool workers =
  let setting name = Configure.conf#path ["scheduler"; name] in
  (Dtools.Conf.as_bool (setting "legacy"))#set true;
  (Dtools.Conf.as_int (setting "generic_queues"))#set workers;
  (Dtools.Conf.as_int (setting "non_blocking_queues"))#set 0;
  Scheduler.start ();
  real_time ()

let resting_clocks () =
  let clocks =
    List.init 6 (fun i ->
        started ~id:(Printf.sprintf "resting.%d" i) ~sync:`Cpu [source `Output])
  in
  let late = ref 0. in
  for _ = 1 to 100 do
    Thread.delay 0.02;
    List.iter
      (fun clock -> late := Float.max !late (Option.get (lateness clock)))
      clocks
  done;
  check
    (Printf.sprintf
       "K12: 6 clocks that rest on 2 workers stay in real time (latest: %.03fs)"
       !late)
    (!late < latency && List.for_all (fun c -> count c is_warning = 0) clocks);
  List.iter stop clocks

let tick_time = 0.03
let time_box = 0.1

let late_clock id =
  let picks = ref [] in
  let output = source `Output in
  output#set_on_animate (fun () ->
      picks := Duppy.time () :: !picks;
      Thread.delay tick_time);
  (started ~id ~sync:`Cpu [output], picks, output)

let rec longest_gap = function
  | later :: (earlier :: _ as rest) ->
      Float.max (later -. earlier) (longest_gap rest)
  | _ -> 0.

let late_clocks_take_turns () =
  let clocks = List.init 4 (fun i -> late_clock (Printf.sprintf "late.%d" i)) in
  Thread.delay 2.;
  let gaps = List.map (fun (_, picks, _) -> longest_gap !picks) clocks in
  let longest = List.fold_left Float.max 0. gaps in
  check
    (Printf.sprintf
       "K12: 4 late clocks on 2 workers take turns (longest wait: %.03fs)"
       longest)
    (longest <= (2. *. (time_box +. tick_time)) +. 0.05
    && List.for_all (fun (_, picks, _) -> List.length !picks > 10) clocks);
  check "a clock that gives its worker back counts the releases"
    (List.for_all (fun (clock, _, _) -> (figures clock).releases > 0) clocks);
  List.iter (fun (clock, _, _) -> stop clock) clocks

let clocks_before_other_work () =
  let clock, _, output = late_clock "alone" in
  Thread.delay 0.2;
  let ran = Atomic.make 0 in
  for _ = 1 to 3 do
    Scheduler.Task.add
      {
        Duppy.Task.priority = `Blocking;
        events = [`Delay 0.];
        handler =
          (fun _ ->
            Atomic.incr ran;
            []);
      }
  done;
  Thread.delay 0.8;
  check "a late clock is picked again before lower-ranked work at each release"
    (Atomic.get ran = 0 && (figures clock).releases > 3);
  output#set_on_animate ignore;
  check "lower-ranked work runs once the clock rests"
    (wait_until (fun () -> Atomic.get ran = 3));
  stop clock

let work_time = 0.05

let wait_in_tick () =
  let spent = Atomic.make 0. in
  let output = source `Output in
  output#set_on_animate (fun () ->
      output#set_on_animate ignore;
      let started = Duppy.time () in
      let finished = Atomic.make false in
      let wait = Scheduler.Condition.create () in
      Scheduler.Task.add
        {
          Duppy.Task.priority = `Blocking;
          events = [`Delay 0.];
          handler =
            (fun _ ->
              Thread.delay work_time;
              Atomic.set finished true;
              Scheduler.Condition.signal wait;
              []);
        };
      Scheduler.Condition.until wait (fun () -> Atomic.get finished);
      Atomic.set spent (Duppy.time () -. started));
  let clock = started ~id:"waiting" ~sync:`Cpu [output] in
  check "a tick waiting, on one worker, for work only a worker can do completes"
    (wait_until (fun () -> Atomic.get spent > 0.));
  check
    (Printf.sprintf
       "it completes as soon as the work is done: %.03fs for %.03fs"
       (Atomic.get spent) work_time)
    (Atomic.get spent < work_time +. 0.05);
  stop clock

let two_workers () =
  start_pool 2;
  resting_clocks ();
  late_clocks_take_turns ()

let one_worker () =
  start_pool 1;
  clocks_before_other_work ();
  wait_in_tick ()
