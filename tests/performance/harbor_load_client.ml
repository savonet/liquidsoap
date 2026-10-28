(* Load client for output.harbor: [readers] listeners that read and discard
   the stream for [duration] seconds, and [stalled] listeners with a tiny
   receive buffer that never read. Prints one summary line:
   LOAD readers=.. connected=.. closed_early=.. ttfb_p50=.. ttfb_p99=.. ttfb_max=.. *)

let connect ~port ~agent ?rcvbuf mount =
  let fd = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Option.iter (fun size -> Unix.setsockopt_int fd Unix.SO_RCVBUF size) rcvbuf;
  Unix.connect fd (Unix.ADDR_INET (Unix.inet_addr_loopback, port));
  let request =
    Printf.sprintf "GET /%s HTTP/1.0\r\nUser-Agent: %s\r\n\r\n" mount agent
  in
  ignore (Unix.write_substring fd request 0 (String.length request));
  fd

type reader = {
  fd : Unix.file_descr;
  started : float;
  mutable first_byte : float option;
  mutable open_ : bool;
}

let percentile sorted p =
  match sorted with
    | [] -> nan
    | _ ->
        let n = List.length sorted in
        List.nth sorted (min (n - 1) (int_of_float (p *. float n)))

let () =
  let port = int_of_string Sys.argv.(1) in
  let mount = Sys.argv.(2) in
  let readers = int_of_string Sys.argv.(3) in
  let stalled = int_of_string Sys.argv.(4) in
  let duration = float_of_string Sys.argv.(5) in
  let stalled_fds =
    List.init stalled (fun _ ->
        connect ~port ~agent:"stalled" ~rcvbuf:4096 mount)
  in
  let lock = Mutex.create () in
  let readers_list = ref [] in
  let by_fd = Hashtbl.create readers in
  let connector =
    Thread.create
      (fun () ->
        for _ = 1 to readers do
          let started = Unix.gettimeofday () in
          let fd = connect ~port ~agent:"reader" mount in
          Unix.set_nonblock fd;
          let r = { fd; started; first_byte = None; open_ = true } in
          Mutex.protect lock (fun () ->
              readers_list := r :: !readers_list;
              Hashtbl.replace by_fd fd r)
        done)
      ()
  in
  let buffer = Bytes.create 65536 in
  let deadline = Unix.gettimeofday () +. duration in
  let closed_early = ref 0 in
  let rec loop () =
    if Unix.gettimeofday () < deadline then (
      let fds =
        Mutex.protect lock (fun () ->
            List.filter_map
              (fun r -> if r.open_ then Some r.fd else None)
              !readers_list)
      in
      let ready, _, _ = Unix_utils.poll fds [] [] 0.1 in
      List.iter
        (fun fd ->
          Option.iter
            (fun r ->
              match Unix.read fd buffer 0 (Bytes.length buffer) with
                | 0 ->
                    r.open_ <- false;
                    incr closed_early
                | _ ->
                    if r.first_byte = None then
                      r.first_byte <- Some (Unix.gettimeofday ())
                | exception
                    Unix.Unix_error ((Unix.EAGAIN | Unix.EWOULDBLOCK), _, _) ->
                    ()
                | exception Unix.Unix_error _ ->
                    r.open_ <- false;
                    incr closed_early)
            (Mutex.protect lock (fun () -> Hashtbl.find_opt by_fd fd)))
        ready;
      loop ())
  in
  loop ();
  Thread.join connector;
  let readers = Array.of_list !readers_list in
  let ttfb =
    Array.to_list readers
    |> List.filter_map (fun r ->
        Option.map (fun t -> t -. r.started) r.first_byte)
    |> List.sort compare
  in
  Printf.printf
    "LOAD readers=%d connected=%d closed_early=%d ttfb_p50=%.3f ttfb_p99=%.3f \
     ttfb_max=%.3f\n\
     %!"
    (Array.length readers) (List.length ttfb) !closed_early
    (percentile ttfb 0.5) (percentile ttfb 0.99) (percentile ttfb 1.);
  Array.iter (fun r -> try Unix.close r.fd with _ -> ()) readers;
  List.iter (fun fd -> try Unix.close fd with _ -> ()) stalled_fds
