module Ssl_transport : Http.Transport_t with type connection = Ssl.socket =
struct
  type connection = Ssl.socket

  type event =
    [ `Write of connection | `Read of connection | `Both of connection ]

  let default_port = 443

  let connect ?bind_address host port =
    let socketaddr =
      Unix.ADDR_INET ((Unix.gethostbyname host).Unix.h_addr_list.(0), port)
    in
    let ctx = Ssl.create_context Ssl.SSLv23 Ssl.Client_context in
    (* TODO: add option.. *)
    Ssl.set_verify ctx [] (Some Ssl.client_verify_callback);
    Ssl.set_verify_depth ctx 3;
    let socket = Ssl.open_connection_with_context ctx socketaddr in
    begin
      match bind_address with
      | None -> ()
      | Some s ->
          let unix_socket = Ssl.file_descr_of_socket socket in
          let bind_addr_inet = (Unix.gethostbyname s).Unix.h_addr_list.(0) in
          (* Seems like you need to bind on port 0 *)
          let bind_addr = Unix.ADDR_INET (bind_addr_inet, 0) in
          Unix.bind unix_socket bind_addr
    end;
    socket

  let disconnect ssl =
    Ssl.shutdown ssl;
    Unix.close (Ssl.file_descr_of_socket ssl)

  let wait_for ?log event timeout =
    let event =
      match event with
        | `Read s -> `Read (Ssl.file_descr_of_socket s)
        | `Write s -> `Write (Ssl.file_descr_of_socket s)
        | `Both s -> `Both (Ssl.file_descr_of_socket s)
    in
    Tutils.wait_for ?log event timeout

  let read = Ssl.read
  let write = Ssl.write
end

module Https = Http.Make (Ssl_transport)
include Https
