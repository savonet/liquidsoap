(* The SRT peer double of spec/conformance.md §3, driven by the SRT tests.

   srt_peer receive --port P [--listen] [--host H] [--passphrase S]
     [--streamid S] [--stop-reading-after N] [--duration D] [--sizes FILE]
     [--bytes FILE] [--rcvbuf BYTES]
   srt_peer hold-udp --port P --duration D [--ready FILE]
   srt_peer send --port P [--listen] [--host H] [--passphrase S]
     [--streamid S] [--file FILE] [--payload N] [--no-tsbpd]
     [--then close|silent|vanish] [--hold D]

   Exit status: 0 once connected and done, 2 when no connection was made. *)

let port = ref 0
let listen = ref false
let host = ref "127.0.0.1"
let passphrase = ref ""
let streamid = ref ""
let stop_reading_after = ref (-1)
let duration = ref 10.
let sizes_file = ref ""
let bytes_file = ref ""
let file = ref ""
let payload = ref 1316
let tsbpd = ref true
let finish = ref "close"
let hold = ref 0.3
let rcvbuf = ref 0
let ready = ref ""

let spec =
  [
    ("--port", Arg.Set_int port, "port");
    ("--listen", Arg.Set listen, "listen instead of calling");
    ("--host", Arg.Set_string host, "host to call");
    ("--passphrase", Arg.Set_string passphrase, "passphrase");
    ("--streamid", Arg.Set_string streamid, "stream id");
    ( "--stop-reading-after",
      Arg.Set_int stop_reading_after,
      "stop reading after N payloads and keep the connection" );
    ("--duration", Arg.Set_float duration, "longest run, in seconds");
    ( "--sizes",
      Arg.Set_string sizes_file,
      "file receiving one payload size per line" );
    ("--bytes", Arg.Set_string bytes_file, "file receiving the received bytes");
    ("--file", Arg.Set_string file, "file to send");
    ("--payload", Arg.Set_int payload, "payload size");
    ("--no-tsbpd", Arg.Clear tsbpd, "turn timestamp-based delivery off");
    ("--then", Arg.Set_string finish, "close, silent or vanish once sent");
    ( "--hold",
      Arg.Set_float hold,
      "seconds to stay connected once sent, at least the receiver's latency" );
    ("--rcvbuf", Arg.Set_int rcvbuf, "receive buffer size, in bytes");
    ( "--ready",
      Arg.Set_string ready,
      "file created once hold-udp holds the port" );
  ]

let socket () =
  let s = Srt.create_socket () in
  Srt.setsockflag s Srt.transtype `Live;
  Srt.setsockflag s Srt.payloadsize Srt.live_max_payload_size;
  Srt.setsockflag s Srt.tsbpdmode !tsbpd;
  if !passphrase <> "" then Srt.setsockflag s Srt.passphrase !passphrase;
  if !streamid <> "" then Srt.setsockflag s Srt.streamid !streamid;
  if !rcvbuf > 0 then Srt.setsockflag s Srt.rcvbuf !rcvbuf;
  s

let address h =
  Unix.ADDR_INET ((Unix.gethostbyname h).Unix.h_addr_list.(0), !port)

let connect () =
  let s = socket () in
  if !listen then (
    Srt.bind s (address "0.0.0.0");
    Srt.listen s 1;
    Srt.setsockflag s Srt.rcvtimeo (int_of_float (!duration *. 1000.));
    let client, _ = Srt.accept s in
    Srt.close s;
    client)
  else (
    Srt.connect s (address !host);
    s)

let deadline () = Unix.gettimeofday () +. !duration

let receive s =
  let sizes = if !sizes_file = "" then None else Some (open_out !sizes_file) in
  let bytes =
    if !bytes_file = "" then None else Some (open_out_bin !bytes_file)
  in
  let buffer = Bytes.create Srt.live_max_payload_size in
  let deadline = deadline () in
  Srt.setsockflag s Srt.rcvtimeo 200;
  let count = ref 0 in
  (try
     while Unix.gettimeofday () < deadline do
       if !stop_reading_after >= 0 && !count >= !stop_reading_after then
         Thread.delay 0.1
       else (
         match Srt.recvmsg s buffer Srt.live_max_payload_size with
           | n ->
               incr count;
               Option.iter (fun c -> Printf.fprintf c "%d\n%!" n) sizes;
               Option.iter
                 (fun c ->
                   output c buffer 0 n;
                   flush c)
                 bytes
           | exception Srt.Error (`Easyncrcv, _, _) -> ()
           | exception Srt.Error (`Etimeout, _, _) -> ())
     done
   with Srt.Error _ -> ());
  Option.iter close_out sizes;
  Option.iter close_out bytes

let read_file name =
  let c = open_in_bin name in
  let s = really_input_string c (in_channel_length c) in
  close_in c;
  s

let send s =
  let data = if !file = "" then "" else read_file !file in
  let n = String.length data in
  let rec loop pos =
    if pos < n then (
      let len = min !payload (n - pos) in
      ignore
        (Srt.sendmsg s (Bytes.of_string (String.sub data pos len)) (-1) false);
      loop (pos + len))
  in
  loop 0;
  while (Srt.Stats.bstats s).Srt.Stats.pktSndBuf > 0 do
    Thread.delay 0.01
  done;
  Thread.delay !hold;
  match !finish with
    | "vanish" -> Unix._exit 0
    | "silent" -> Thread.delay !duration
    | _ -> ()

(* Holds a plain UDP port, as another program would. *)
let hold_udp () =
  let s = Unix.socket Unix.PF_INET Unix.SOCK_DGRAM 0 in
  Unix.bind s (Unix.ADDR_INET (Unix.inet_addr_any, !port));
  if !ready <> "" then close_out (open_out !ready);
  Thread.delay !duration;
  Unix.close s;
  exit 0

let () =
  let command = ref "" in
  Arg.parse spec
    (fun c -> command := c)
    "srt_peer receive|send|hold-udp [options]";
  if !command = "hold-udp" then hold_udp ();
  Srt.startup ();
  let status =
    match connect () with
      | exception exn ->
          prerr_endline ("srt_peer: no connection: " ^ Printexc.to_string exn);
          2
      | s ->
          (match !command with
            | "receive" -> receive s
            | "send" -> send s
            | c -> failwith ("unknown command " ^ c));
          (try Srt.close s with _ -> ());
          0
  in
  Srt.cleanup ();
  exit status
