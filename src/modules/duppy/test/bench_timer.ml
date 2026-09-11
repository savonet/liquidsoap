(* How late a parked computation resumes after [reschedule ~delay], against a
   plain sleep in the same process. *)

type prio = Blocking

let classify (_ : prio) = `Blocking

let quantiles xs =
  let xs = List.sort compare xs in
  let n = List.length xs in
  let q p = List.nth xs (int_of_float (p *. float (n - 1))) in
  (q 0.5, q 0.95, q 1.0)

let report name d (m, p95, mx) =
  Printf.printf
    "%-10s delay=%7.1fms  late: median=%7.3fms  p95=%7.3fms  max=%7.3fms\n%!"
    name (d *. 1000.) (m *. 1000.) (p95 *. 1000.) (mx *. 1000.)

let () =
  let s = Duppy.create ~classify () in
  Duppy.start ~pool:(`Domains (Domain.recommended_domain_count ())) s;
  let trials =
    [(0.001, 300); (0.005, 300); (0.02, 300); (0.1, 100); (0.5, 20)]
  in
  List.iter
    (fun (d, n) ->
      let sleep = ref [] in
      for _ = 1 to n do
        let t0 = Unix.gettimeofday () in
        Unix.sleepf d;
        sleep := (Unix.gettimeofday () -. t0 -. d) :: !sleep
      done;
      report "sleepf" d (quantiles !sleep);
      let late = ref [] in
      let finished = Atomic.make false in
      Duppy.Task.add s
        {
          Duppy.Task.priority = Blocking;
          events = [`Delay 0.];
          handler =
            (fun _ ->
              Duppy.run (fun () ->
                  for _ = 1 to n do
                    let t0 = Unix.gettimeofday () in
                    Duppy.reschedule ~delay:d ~priority:Blocking s;
                    late := (Unix.gettimeofday () -. t0 -. d) :: !late
                  done;
                  Atomic.set finished true);
              []);
        };
      while not (Atomic.get finished) do
        Thread.delay 0.005
      done;
      report "duppy" d (quantiles !late))
    trials;
  Duppy.stop s
