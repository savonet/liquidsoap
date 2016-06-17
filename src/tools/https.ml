let () =
  Ssl_threads.init ();
  Ssl.init ()

module Ssl_transport : Http.Transport_t with type connection = Ssl.socket =
struct
  type connection = Ssl.socket
  (* bind_addr is ignore. *)
  let connect ?bind_address host port =
    let socketaddr =
      (Unix.ADDR_INET((Unix.gethostbyname host).Unix.h_addr_list.(0),port))
    in
    let ctx = Ssl.create_context Ssl.SSLv23 Ssl.Client_context in
    (* TODO: add option.. *)
    Ssl.set_verify ctx [] (Some Ssl.client_verify_callback);
    Ssl.set_verify_depth ctx 3;
    Ssl.open_connection_with_context ctx socketaddr
  let disconnect = Ssl.shutdown
  (* Ignore for now *)
  let wait_for ?log event socket timeout = ()
  let read = Ssl.read
  let write = Ssl.write
end

module Https = Http.Make_http(Ssl_transport)

include Https
