(* The pool dispatches across domains, batches immediate tasks onto one of
   them, delivers socket events, and lets running tasks finish on stop. *)

type priority = Immediate | Blocking

let classify = function Immediate -> `Immediate | Blocking -> `Blocking
let domain_id () = (Domain.self () :> int)

let fail fmt =
  Printf.ksprintf
    (fun s ->
      prerr_endline ("FAIL: " ^ s);
      exit 1)
    fmt

let ok fmt = Printf.ksprintf (fun s -> print_endline ("ok: " ^ s)) fmt

type latch = { m : Mutex.t; c : Condition.t; n : int Atomic.t }

let latch () =
  { m = Mutex.create (); c = Condition.create (); n = Atomic.make 0 }

let bump l =
  ignore (Atomic.fetch_and_add l.n 1);
  Mutex.lock l.m;
  Condition.broadcast l.c;
  Mutex.unlock l.m

let await l target =
  Mutex.lock l.m;
  while Atomic.get l.n < target do
    Condition.wait l.c l.m
  done;
  Mutex.unlock l.m

(* A hang would otherwise wedge the whole suite. *)
let watchdog seconds =
  ignore
    (Thread.create
       (fun () ->
         Thread.delay seconds;
         prerr_endline "FAIL: timed out";
         exit 1)
       ())

let burn n =
  let x = ref 0.0 in
  for i = 1 to n do
    x := Sys.opaque_identity (!x +. sqrt (float_of_int i))
  done;
  ignore (Sys.opaque_identity !x)

(* A negative delay is already elapsed when the task is added, so the task is
   ready before the pool starts rather than reaching it through the poller. *)
let task priority handler =
  { Duppy.Task.priority; events = [`Delay (-1.)]; handler }

let ran_on domains =
  Array.iteri (fun i d -> if d < 0 then fail "task %d never ran" i) domains;
  List.length (List.sort_uniq compare (Array.to_list domains))

(* One blocking slot per worker, and every task holds its slot until all of
   them are running: they can only all make progress if each landed on a
   different worker, so a pool that dispatches to one domain deadlocks here
   rather than passing. *)
let test_parallel () =
  let s = Duppy.create ~classify () in
  let started = latch () in
  let finished = latch () in
  let count = 4 in
  let domains = Array.make count (-1) in
  for i = 0 to count - 1 do
    Duppy.Task.add s
      (task Blocking (fun _ ->
           domains.(i) <- domain_id ();
           bump started;
           await started count;
           bump finished;
           []))
  done;
  Duppy.start ~domains:count ~max_blocking:count s;
  await finished count;
  Duppy.stop s;
  let distinct = ran_on domains in
  if distinct <> count then
    fail "%d concurrent tasks ran on %d domains, expected %d" count distinct
      count;
  ok "%d blocking tasks ran concurrently on %d domains" count distinct

let test_batch () =
  let s = Duppy.create ~classify () in
  let l = latch () in
  let count = 32 in
  let domains = Array.make count (-1) in
  for i = 0 to count - 1 do
    Duppy.Task.add s
      (task Immediate (fun _ ->
           domains.(i) <- domain_id ();
           bump l;
           []))
  done;
  Duppy.start ~domains:4 s;
  await l count;
  Duppy.stop s;
  let distinct = ran_on domains in
  if distinct <> 1 then
    fail "%d immediate tasks spread over %d domains, they were not batched"
      count distinct;
  ok "%d immediate tasks ran as one batch" count

let test_io () =
  let s = Duppy.create ~classify () in
  let l = latch () in
  let r, w = Unix.pipe () in
  Duppy.Task.add s
    {
      Duppy.Task.priority = Blocking;
      events = [`Read r];
      handler =
        (fun _ ->
          ignore (Unix.read r (Bytes.create 4) 0 4);
          bump l;
          []);
    };
  Duppy.start ~domains:2 s;
  ignore (Unix.write w (Bytes.of_string "ping") 0 4);
  await l 1;
  Duppy.stop s;
  Unix.close r;
  Unix.close w;
  ok "the event loop delivered a socket event"

(* A task that never returns must not keep [stop] from returning: it parks in a
   syscall on a descriptor the loop has dropped, and stopping joins its domain. *)
let test_stop_with_stuck_task () =
  let s = Duppy.create ~classify () in
  let started = latch () in
  let release = Atomic.make false in
  Duppy.Task.add s
    (task Blocking (fun _ ->
         bump started;
         while not (Atomic.get release) do
           Thread.delay 0.01
         done;
         []));
  Duppy.start ~domains:2 s;
  await started 1;
  let t0 = Unix.gettimeofday () in
  Duppy.stop s;
  let elapsed = Unix.gettimeofday () -. t0 in
  Atomic.set release true;
  if elapsed > 30. then fail "stop took %.1fs with a stuck task" elapsed;
  ok "stop returned in %.1fs despite a stuck task" elapsed

(* With nothing to wait for, the loop must still come back on its own: a
   wake-up is a byte on a socket whose writer drops it when the buffer is full,
   so a loop that only ever leaves on one can miss [stopped] for good. *)
let test_idle_loop_wakes_itself () =
  let s = Duppy.create ~classify () in
  let wakes = Atomic.make 0 in
  let log m =
    if String.length m >= 4 && String.sub m 0 4 = "Woke" then
      ignore (Atomic.fetch_and_add wakes 1)
  in
  Duppy.start ~domains:1 ~log s;
  Thread.delay 2.5;
  let n = Atomic.get wakes in
  Duppy.stop s;
  if n < 2 then fail "idle loop came back %d times in 2.5s" n;
  ok "idle loop came back on its own %d times in 2.5s" n

(* A task is free to start a thread that outlives it, which is what a binding
   logging from its own thread does. That thread belongs to the domain that ran
   the task, and a domain does not terminate until its threads have, so
   stopping must not wait on one. *)
let test_stop_with_thread_outliving_its_task () =
  let s = Duppy.create ~classify () in
  let started = latch () in
  let release = Atomic.make false in
  Duppy.Task.add s
    (task Blocking (fun _ ->
         ignore
           (Thread.create
              (fun () ->
                while not (Atomic.get release) do
                  Thread.delay 0.01
                done)
              ());
         bump started;
         []));
  Duppy.start ~domains:2 s;
  await started 1;
  Thread.delay 0.2;
  let t0 = Unix.gettimeofday () in
  Duppy.stop s;
  let elapsed = Unix.gettimeofday () -. t0 in
  Atomic.set release true;
  if elapsed > 30. then
    fail "stop took %.1fs with a thread outliving its task" elapsed;
  ok "stop returned in %.1fs with a thread pinning a domain" elapsed

let test_stop_drains () =
  let count = 8 in
  for _ = 1 to 5 do
    let s = Duppy.create ~classify () in
    let started = latch () in
    let finished = latch () in
    for _ = 1 to count do
      Duppy.Task.add s
        (task Blocking (fun _ ->
             bump started;
             Thread.delay 0.1;
             bump finished;
             []))
    done;
    Duppy.start ~domains:4 s;
    await started count;
    Duppy.stop s;
    let done_ = Atomic.get finished.n in
    if done_ < count then
      fail "stop returned with %d of %d tasks still running" (count - done_)
        count
  done;
  ok "stop drained %d running tasks, 5 times over" count

(* With every worker at its cap, the slot freed by a finishing task has to
   reach the worker that freed it, or the remaining tasks never get picked up. *)
let test_blocking_cap () =
  let s = Duppy.create ~classify () in
  let l = latch () in
  let count = 24 in
  for _ = 1 to count do
    Duppy.Task.add s
      (task Blocking (fun _ ->
           Thread.delay 0.05;
           bump l;
           []))
  done;
  Duppy.start ~domains:4 ~max_blocking:4 s;
  await l count;
  Duppy.stop s;
  ok "%d blocking tasks all ran with only 4 slots" count

(* A computation started here parks on the pool and resumes on one of its
   domains, so the two halves never run on the same one. *)
let test_effect_resumes_elsewhere () =
  let s = Duppy.create ~classify () in
  Duppy.start ~domains:2 s;
  let l = latch () in
  let before = ref (-1) in
  let after = ref (-1) in
  let r, w = Unix.pipe () in
  Duppy.run (fun () ->
      before := domain_id ();
      let events = Duppy.await ~priority:Blocking s [`Read r] in
      if events <> [`Read r] then fail "await returned the wrong events";
      ignore (Unix.read r (Bytes.create 4) 0 4);
      after := domain_id ();
      bump l);
  if !after <> -1 then fail "await did not park the computation";
  ignore (Unix.write w (Bytes.of_string "ping") 0 4);
  await l 1;
  Duppy.stop s;
  Unix.close r;
  Unix.close w;
  if !before = !after then
    fail "computation resumed on its starting domain %d" !before;
  ok "computation started on domain %d and resumed on %d" !before !after

let test_effect_raises_to_on_error () =
  let caught = Atomic.make None in
  let s =
    Duppy.create ~classify
      ~on_error:(fun exn _ -> Atomic.set caught (Some exn))
      ()
  in
  Duppy.start ~domains:2 s;
  let l = latch () in
  Duppy.run (fun () ->
      Duppy.reschedule ~priority:Blocking s;
      bump l;
      raise Exit);
  await l 1;
  Duppy.stop s;
  match Atomic.get caught with
    | Some Exit -> ok "an exception after resuming reached on_error"
    | Some e -> fail "on_error saw %s, expected Exit" (Printexc.to_string e)
    | None -> fail "an exception after resuming reached nobody"

let test_await_outside_run () =
  let s = Duppy.create ~classify () in
  match Duppy.await ~priority:Blocking s [`Delay 0.] with
    | _ -> fail "await outside run returned instead of raising"
    | exception Effect.Unhandled _ ->
        ok "await outside run raises Effect.Unhandled"

(* A lone worker facing a ready list that refills as fast as it drains must
   still dispatch the other class. *)
let test_no_starvation () =
  let s = Duppy.create ~classify () in
  let stop = Atomic.make false in
  let blocking_ran = Atomic.make false in
  let l = latch () in
  let rec churn _ =
    if Atomic.get stop then []
    else
      [
        {
          Duppy.Task.priority = Immediate;
          events = [`Delay (-1.)];
          handler = churn;
        };
      ]
  in
  for _ = 1 to 20 do
    Duppy.Task.add s (task Immediate churn)
  done;
  Duppy.Task.add s
    (task Blocking (fun _ ->
         Atomic.set blocking_ran true;
         bump l;
         []));
  Duppy.start ~domains:1 s;
  await l 1;
  Atomic.set stop true;
  Duppy.stop s;
  if not (Atomic.get blocking_ran) then
    fail "a flood of immediate tasks starved the blocking one";
  ok "one worker dispatched blocking work under an immediate flood"

let () =
  watchdog 60.;
  test_parallel ();
  test_batch ();
  test_io ();
  test_blocking_cap ();
  test_stop_drains ();
  test_stop_with_thread_outliving_its_task ();
  test_idle_loop_wakes_itself ();
  test_stop_with_stuck_task ();
  test_effect_resumes_elsewhere ();
  test_effect_raises_to_on_error ();
  test_await_outside_run ();
  test_no_starvation ();
  print_endline "all duppy pool checks passed"
