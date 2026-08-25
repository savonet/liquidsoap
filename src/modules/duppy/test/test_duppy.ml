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

let test_parallel () =
  let s = Duppy.create ~classify () in
  let l = latch () in
  let count = 16 in
  let domains = Array.make count (-1) in
  for i = 0 to count - 1 do
    Duppy.Task.add s
      (task Blocking (fun _ ->
           burn 3_000_000;
           domains.(i) <- domain_id ();
           bump l;
           []))
  done;
  Duppy.start ~domains:4 s;
  await l count;
  Duppy.stop s;
  let distinct = ran_on domains in
  if distinct < 2 then
    fail "%d tasks all ran on one domain, the pool is not parallel" count;
  ok "%d blocking tasks ran across %d domains" count distinct

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

let test_stop_drains () =
  let count = 8 in
  for _ = 1 to 5 do
    let s = Duppy.create ~classify () in
    let started = latch () and finished = latch () in
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

let () =
  watchdog 60.;
  test_parallel ();
  test_batch ();
  test_io ();
  test_blocking_cap ();
  test_stop_drains ();
  print_endline "all duppy pool checks passed"
