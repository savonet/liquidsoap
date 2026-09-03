(* Reports what the pool is worth: how CPU-bound tasks scale with the number of
   domains, and how much a periodic thread — a clock's shape — is delayed while
   they run. Not a gate; run it by hand with `dune build @bench`. *)

type priority = Immediate | Blocking

let classify = function Immediate -> `Immediate | Blocking -> `Blocking

let burn n =
  let x = ref 0.0 in
  for i = 1 to n do
    x := Sys.opaque_identity (!x +. sqrt (float_of_int i))
  done;
  ignore (Sys.opaque_identity !x)

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

let scaling ~domains ~tasks ~work =
  let s = Duppy.create ~classify () in
  let l = latch () in
  for _ = 1 to tasks do
    Duppy.Task.add s
      {
        Duppy.Task.priority = Blocking;
        events = [`Delay 0.];
        handler =
          (fun _ ->
            burn work;
            bump l;
            []);
      }
  done;
  let start = Unix.gettimeofday () in
  Duppy.start ~pool:(`Domains domains) s;
  await l tasks;
  let elapsed = Unix.gettimeofday () -. start in
  Duppy.stop s;
  elapsed

let percentile l p =
  let l = List.sort compare l in
  List.nth l
    (min (List.length l - 1) (int_of_float (p *. float_of_int (List.length l))))

let jitter ~domains ~load =
  let s = Duppy.create ~classify () in
  let ticks = 300 in
  let period = 0.02 in
  let running = Atomic.make true in
  let rec spin _ =
    burn 200_000;
    if Atomic.get running then
      [{ Duppy.Task.priority = Blocking; events = [`Delay 0.]; handler = spin }]
    else []
  in
  for _ = 1 to load do
    Duppy.Task.add s
      { Duppy.Task.priority = Blocking; events = [`Delay 0.]; handler = spin }
  done;
  Duppy.start ~pool:(`Domains domains) s;
  let start = Unix.gettimeofday () in
  let lateness = ref [] in
  for i = 1 to ticks do
    let target = start +. (float_of_int i *. period) in
    let now = Unix.gettimeofday () in
    if target > now then Thread.delay (target -. now);
    lateness := (Unix.gettimeofday () -. target) :: !lateness
  done;
  Atomic.set running false;
  Duppy.stop s;
  !lateness

(* What one turn of the loop costs while N tasks sit waiting on quiet sockets:
   the shape of a harbor with that many connected-but-silent clients. *)
let idle_cost ~idle ~rounds =
  let s = Duppy.create ~classify () in
  let keep =
    List.init idle (fun _ ->
        let a, b = Unix.socketpair Unix.PF_UNIX Unix.SOCK_STREAM 0 in
        Duppy.Task.add s
          {
            Duppy.Task.priority = Blocking;
            events = [`Read a];
            handler = (fun _ -> []);
          };
        (a, b))
  in
  let l = latch () in
  let rec ping _ =
    bump l;
    if Atomic.get l.n < rounds then
      [
        {
          Duppy.Task.priority = Blocking;
          events = [`Delay 1e-6];
          handler = ping;
        };
      ]
    else []
  in
  let start = Unix.gettimeofday () in
  Duppy.Task.add s
    { Duppy.Task.priority = Blocking; events = [`Delay 1e-6]; handler = ping };
  Duppy.start ~pool:(`Domains 2) s;
  await l rounds;
  let elapsed = Unix.gettimeofday () -. start in
  Duppy.stop s;
  List.iter
    (fun (a, b) ->
      Unix.close a;
      Unix.close b)
    keep;
  elapsed *. 1e6 /. float_of_int rounds

let () =
  let cores = Domain.recommended_domain_count () in
  Printf.printf "%d cores available\n\n%!" cores;
  Printf.printf "CPU-bound tasks (32 tasks)\n";
  let base = ref 0. in
  List.iter
    (fun domains ->
      if domains <= cores then begin
        let elapsed = scaling ~domains ~tasks:32 ~work:8_000_000 in
        if domains = 1 then base := elapsed;
        Printf.printf "  domains=%-2d  wall=%6.3fs  speed-up=%.1fx\n%!" domains
          elapsed (!base /. elapsed)
      end)
    [1; 2; 4; 6; 8];
  Printf.printf "\nLateness of a 20ms periodic thread\n";
  List.iter
    (fun load ->
      let l = jitter ~domains:cores ~load in
      Printf.printf
        "  busy tasks=%-2d  p50=%6.2fms  p99=%6.2fms  max=%6.2fms%s\n%!" load
        (percentile l 0.5 *. 1e3)
        (percentile l 0.99 *. 1e3)
        (List.fold_left max 0. l *. 1e3)
        (if cores <= load then "  (oversubscribed)" else ""))
    [0; 2; 4; 7];
  Printf.printf "\nCost of one loop turn vs waiting tasks\n";
  List.iter
    (fun idle ->
      Printf.printf "  idle tasks=%-5d %6.1f us/turn\n%!" idle
        (idle_cost ~idle ~rounds:2000))
    [0; 100; 500; 1000; 2000]
