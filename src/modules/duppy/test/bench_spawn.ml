(* Many short threaded tasks ready at once, added from a task that waits for
   them in suspended sleeps: a round past [stall] seconds is a stall. *)

let n = 50_000
let rounds = try int_of_string Sys.argv.(1) with _ -> 20
let stall = 5.
let rank = function `Immediate -> 0 | `Direct -> 1 | `Threaded -> 2

let scheduler : Duppy.execution_class Duppy.scheduler =
  Duppy.create ~rank ~classify:Fun.id ()

let sleep d =
  Duppy.suspend ~priority:`Threaded scheduler (fun resume ->
      Duppy.Task.add scheduler
        {
          priority = `Immediate;
          events = [`Delay d];
          handler =
            (fun _ ->
              resume ();
              []);
        })

let round () =
  let ended = Atomic.make 0 in
  let t0 = Unix.gettimeofday () in
  for _ = 1 to n do
    Duppy.Task.add scheduler
      {
        priority = `Threaded;
        events = [`Delay 0.];
        handler =
          (fun _ ->
            Duppy.run (fun () -> Atomic.incr ended);
            []);
      }
  done;
  let added = Unix.gettimeofday () in
  while Atomic.get ended < n do
    sleep 0.01
  done;
  (added -. t0, Unix.gettimeofday () -. t0)

let () =
  Duppy.start
    ~pool:(`Domains (max 2 (min 8 (Domain.recommended_domain_count ()))))
    ~max_blocking:256 scheduler;
  let finished = Atomic.make false in
  Duppy.Task.add scheduler
    {
      priority = `Threaded;
      events = [`Delay 0.];
      handler =
        (fun _ ->
          Duppy.run (fun () ->
              for i = 1 to rounds do
                let add, total = round () in
                Printf.printf "round %2d: add %.3fs total %.3fs%s\n%!" i add
                  total
                  (if total > stall then "  STALL" else "")
              done;
              Atomic.set finished true);
          []);
    };
  while not (Atomic.get finished) do
    Unix.sleepf 0.05
  done;
  Duppy.stop scheduler
