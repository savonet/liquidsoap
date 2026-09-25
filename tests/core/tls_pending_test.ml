(* A request larger than one read arrives as a single TLS record: once it is
   off the wire, the rest of it only lives in the TLS layer. *)
let request = "GET /" ^ String.make 3000 'a' ^ " HTTP/1.0\r\n\r\n"
let read_timeout = 2.

let fail fmt =
  Printf.ksprintf
    (fun s ->
      Printf.eprintf "FAIL: %s\n%!" s;
      exit 1)
    fmt

let listen () =
  let socket = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Unix.setsockopt socket Unix.SO_REUSEADDR true;
  Unix.bind socket (Unix.ADDR_INET (Unix.inet_addr_loopback, 0));
  Unix.listen socket 1;
  match Unix.getsockname socket with
    | Unix.ADDR_INET (_, port) -> (socket, port)
    | _ -> assert false

(* The client stays connected and silent until [f] returns, as a client
   waiting for a response does. *)
let with_connection (transport : Liq_http.transport) f =
  let listening, port = listen () in
  let served = Atomic.make false in
  let client =
    Thread.create
      (fun () ->
        let socket = transport#connect ~prefer:`Ipv4 "localhost" port in
        let data = Bytes.of_string request in
        assert (socket#write data 0 (Bytes.length data) = Bytes.length data);
        while not (Atomic.get served) do
          Thread.delay 0.05
        done;
        socket#close)
      ()
  in
  let socket, _ = transport#server#accept ~timeout:5. listening in
  Fun.protect
    ~finally:(fun () ->
      Atomic.set served true;
      Thread.join client;
      (try socket#close with _ -> ());
      Unix.close listening)
    (fun () -> f socket)

let read_with_wait_for (socket : Liq_http.socket) =
  let buf = Bytes.create 1024 in
  let received = Buffer.create (String.length request) in
  while Buffer.length received < String.length request do
    socket#wait_for `Read read_timeout;
    let n = socket#read buf 0 (Bytes.length buf) in
    if n <= 0 then
      fail "connection closed after %d bytes" (Buffer.length received);
    Buffer.add_subbytes received buf 0 n
  done;
  Buffer.contents received

let read_with_duppy scheduler socket =
  let module Io = Harbor.Http_transport.Io in
  let result = Atomic.make None in
  Duppy.Task.add scheduler
    {
      Duppy.Task.priority = ();
      events = [`Delay 0.];
      handler =
        (fun _ ->
          Duppy.run (fun () ->
              let h = Io.handle scheduler socket in
              Atomic.set result
                (Some
                   (try
                      Ok
                        (Io.read ~timeout:read_timeout ~priority:() h
                           (Io.Split "\r\n\r\n"))
                    with exn -> Error exn)));
          []);
    };
  let deadline = Unix.gettimeofday () +. (2. *. read_timeout) in
  let rec wait () =
    match Atomic.get result with
      | Some (Ok headers) -> headers ^ "\r\n\r\n"
      | Some (Error exn) ->
          fail "duppy read failed: %s" (Printexc.to_string exn)
      | None when Unix.gettimeofday () > deadline -> fail "duppy read hung"
      | None ->
          Thread.delay 0.05;
          wait ()
  in
  wait ()

let check ~name transport scheduler =
  List.iter
    (fun (reader, read) ->
      let received = with_connection transport read in
      if received <> request then
        fail "%s/%s: received %d bytes, expected %d" name reader
          (String.length received) (String.length request);
      Printf.printf "%s/%s: read a %d-byte request\n%!" name reader
        (String.length request))
    [("wait_for", read_with_wait_for); ("duppy", read_with_duppy scheduler)]

let () =
  let certificate, key =
    match Sys.argv with
      | [| _; certificate; key |] -> (certificate, key)
      | _ -> fail "usage: %s <certificate> <key>" Sys.argv.(0)
  in
  let scheduler = Duppy.create () in
  Duppy.start ~pool:(`Domains 2) scheduler;
  let ssl =
    Builtins_ssl.transport ~min_protocol:None ~max_protocol:None ~read_timeout
      ~write_timeout:read_timeout
      ~server_context:
        (Builtins_ssl.server_context ~min_protocol:None ~max_protocol:None
           ~password:None
           ~certificate:(fun () -> certificate)
           ~key:(fun () -> Some key))
      ~certificate:(fun () -> certificate)
      ()
  in
  let tls =
    Builtins_tls.transport ~read_timeout ~write_timeout:read_timeout
      ~server_config:
        (Builtins_tls.server_config
           ~certificate:(fun () -> certificate)
           ~key:(fun () -> Some key)
           ~client_certificate:(fun () -> None))
      ~certificate:(fun () -> certificate)
      ~client_certificate:(fun () -> None)
      ~client_key:(fun () -> key)
      ()
  in
  check ~name:"ssl" ssl scheduler;
  check ~name:"tls" tls scheduler;
  Duppy.stop scheduler
