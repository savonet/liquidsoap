(* Dedicated encoders through the listener's reused chunks: with outputs from
   one byte to several chunks and a socket that accepts a few bytes at a time,
   a listener receives exactly the encoder's bytes, with and without ICY
   blocks. *)

module Dedicated = Harbor_output.Dedicated
module Listener = Harbor_output.Listener

let log = Log.make ["test"]
let metaint = 1000
let head = "HTTP/1.0 200 OK\r\n\r\n"
let header = "HEADER"
let sizes = [1; 7; 3000; 70000; 200000; 65536; 65535; 12; 9000; 8192; 8191]
let accepts = [| 1; 300; 5000; 100000; 64; 70000 |]
let checks = ref 0

let check label cond =
  incr checks;
  if not cond then failwith label

(* An encoder whose bytes count up modulo 251, in outputs of [sizes] in turn,
   handed over with [encode_into]. [produced] keeps what it emitted. *)
let encoder produced =
  let position = ref 0 and outputs = ref sizes in
  let next () =
    let size, rest =
      match !outputs with
        | size :: rest -> (size, rest)
        | [] -> (List.hd sizes, List.tl sizes)
    in
    outputs := rest;
    let data = Bytes.init size (fun i -> Char.chr ((!position + i) mod 251)) in
    position := !position + size;
    Buffer.add_bytes produced data;
    data
  in
  let encode _ = Strings.of_bytes (next ()) in
  {
    Encoder.encode_metadata = (fun _ -> ());
    header = (fun () -> Strings.of_string header);
    encode;
    encode_into =
      Some
        (fun _ f ->
          let data = next () in
          f
            {
              Encoder.length = Bytes.length data;
              blit =
                (fun offset destination destination_offset length ->
                  Bytes.blit data offset destination destination_offset length);
            });
    stop = (fun () -> Strings.empty);
    bitrate = (fun () -> None);
    restart_offset = (fun () -> None);
  }

(* A socket that accepts [accepts] bytes per write in turn and keeps them. *)
let socket received =
  let fd, _ = Unix.socketpair Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  let writes = ref 0 in
  object
    method typ = "fake"
    method transport = Liq_http.unix_transport
    method file_descr = fd
    method wait_for ?log:_ _ _ = ()
    method pending = false
    method non_blocking _ = ()

    method write data offset length =
      let length = min length accepts.(!writes mod Array.length accepts) in
      incr writes;
      Buffer.add_subbytes received data offset length;
      length

    method read _ _ _ = 0
    method closed = false
    method close = ()
  end

let info id =
  {
    Listener.id;
    ip = "127.0.0.1";
    peer = "127.0.0.1:0";
    uri = "/test";
    query_string = "";
    protocol = "1.0";
    headers = [];
    connected_at = Unix.gettimeofday ();
  }

(* The stream of an ICY body: [metaint] bytes, then a length byte [n] and
   [16 n] bytes of metadata, and so on. *)
let strip_icy body =
  let out = Buffer.create (String.length body) in
  let rec go offset =
    let remaining = String.length body - offset in
    if remaining <= metaint then Buffer.add_substring out body offset remaining
    else (
      Buffer.add_substring out body offset metaint;
      let n = Char.code body.[offset + metaint] in
      go (offset + metaint + 1 + (16 * n)))
  in
  go 0;
  Buffer.contents out

let run ~icy =
  let produced = Buffer.create 4096 and received = Buffer.create 4096 in
  let t =
    Dedicated.create
      ~factory:(fun _ _ -> encoder produced)
      ~buffer:(1 lsl 30) ~metaint ()
  in
  Dedicated.start t ~name:"test" ~last:None;
  let listener =
    Listener.create ~log ~wake:(fun () -> ()) (info 0) (socket received)
  in
  let pending, flush =
    Option.get
      (Dedicated.join t ~head ~icy ~preroll:None listener
         (fun ~pending ~flush ~teardown:_ -> (pending, flush)))
  in
  let scratch = Bytes.create 131072 in
  let frame = Frame.create ~length:10 (Frame.Fields.from_list []) in
  for _ = 1 to 3 * List.length sizes do
    Dedicated.send t frame;
    while pending () do
      flush ~scratch
    done
  done;
  let body = Buffer.contents received in
  let label = if icy then "ICY" else "plain" in
  check (label ^ ": head first") (String.starts_with ~prefix:head body);
  let stream =
    String.sub body (String.length head)
      (String.length body - String.length head)
  in
  let stream = if icy then strip_icy stream else stream in
  check
    (label ^ ": the encoder's bytes, in order")
    (stream = header ^ Buffer.contents produced)

let () =
  run ~icy:false;
  run ~icy:true;
  let expected = 4 in
  if !checks <> expected then
    failwith (Printf.sprintf "ran %d checks, expected %d" !checks expected);
  Printf.printf "harbor_dedicated_test: %d checks passed\n" !checks
