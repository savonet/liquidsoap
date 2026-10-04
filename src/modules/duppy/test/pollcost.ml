(* Splits one event-loop iteration into its two costs: the poll syscall over N
   registered sockets, and the OCaml-side rebuild of the fd sets and readiness
   test over N waiting tasks. *)

let bench name n fn =
  let rounds = 200 in
  let start = Unix.gettimeofday () in
  for _ = 1 to rounds do
    fn ()
  done;
  let us = (Unix.gettimeofday () -. start) *. 1e6 /. float_of_int rounds in
  Printf.printf "  %-28s n=%-5d %7.1f us\n%!" name n us

type e = { r : Unix.file_descr list; t : float }

let () =
  let counts = [0; 100; 500; 1000; 2000; 4000] in
  let socks =
    List.map
      (fun _ ->
        let a, b = Unix.socketpair Unix.PF_UNIX Unix.SOCK_STREAM 0 in
        ignore b;
        a)
      (List.init (List.hd (List.rev counts)) (fun i -> i))
  in
  let wake_r, _wake_w = Unix.pipe () in
  List.iter
    (fun n ->
      let fds = List.filteri (fun i _ -> i < n) socks in
      (* The syscall alone, with the fd list already built. *)
      let all = wake_r :: fds in
      bench "poll syscall" n (fun () -> ignore (Unix_utils.poll all [] [] 0.));
      let ps = Pollset.create () in
      List.iter
        (fun fd ->
          Pollset.set ps fd
            { Pollset.read = true; write = false; except = false })
        all;
      bench "Pollset.wait" n (fun () -> ignore (Pollset.wait ps ~timeout:0.));
      Pollset.close ps;
      (* What duppy does around it: fold every task to build the event set,
         then test every task against the result. *)
      let tasks =
        List.map
          (fun fd ->
            let enrich e = { e with r = fd :: e.r } in
            let is_ready e = if List.mem fd e.r then Some () else None in
            (enrich, is_ready))
          fds
      in
      (* [is_ready] is tested against what poll returned, which is empty while
         the sockets stay quiet, not against the set that was submitted. *)
      let fired = { r = []; t = 0. } in
      bench "enrich fold (build set)" n (fun () ->
          let e =
            List.fold_left
              (fun e (enrich, _) -> enrich e)
              { r = [wake_r]; t = infinity }
              tasks
          in
          ignore (List.length e.r));
      bench "is_ready fold (dispatch)" n (fun () ->
          let ready, waiting =
            List.fold_left
              (fun (ready, waiting) (_, is_ready) ->
                match is_ready fired with
                  | Some () -> (() :: ready, waiting)
                  | None -> (ready, () :: waiting))
              ([], []) tasks
          in
          ignore (List.length ready + List.length waiting));
      print_newline ())
    counts
