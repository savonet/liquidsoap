type socket = {ctx: SecureTransport.t; sock: Unix.file_descr}

module SecureTransport_transport :
  Http.Transport_t with type connection = socket = struct
  type connection = socket

  type event =
    [`Write of connection | `Read of connection | `Both of connection]

  let default_port = 443

  let connect ?bind_address host port =
    let sockaddr =
      Unix.ADDR_INET ((Unix.gethostbyname host).Unix.h_addr_list.(0), port)
    in
    let domain =
      match sockaddr with
        | Unix.ADDR_UNIX _ ->
            Unix.PF_UNIX
        | Unix.ADDR_INET (_, _) ->
            Unix.PF_INET
    in
    let sock = Unix.socket domain Unix.SOCK_STREAM 0 in
    begin
      try Unix.connect sock sockaddr with exn -> Unix.close sock ; raise exn
    end ;
    begin
      match bind_address with None -> () | Some s ->
          let bind_addr_inet = (Unix.gethostbyname s).Unix.h_addr_list.(0) in
          (* Seems like you need to bind on port 0 *)
          let bind_addr = Unix.ADDR_INET (bind_addr_inet, 0) in
          Unix.bind sock bind_addr
    end ;
    let ctx =
      SecureTransport.init SecureTransport.Client SecureTransport.Stream
    in
    SecureTransport.set_peer_domain_name ctx host ;
    SecureTransport.set_connection ctx sock ;
    SecureTransport.handshake ctx ;
    {ctx; sock}

  let disconnect h =
    SecureTransport.close h.ctx ;
    Unix.close h.sock

  let wait_for ?log event timeout =
    let event =
      match event with
        | `Read s ->
            `Read s.sock
        | `Write s ->
            `Write s.sock
        | `Both s ->
            `Both s.sock
    in
    Tutils.wait_for ?log event timeout

  let read {ctx} buf ofs len = SecureTransport.read ctx buf ofs len

  let write {ctx} buf ofs len = SecureTransport.write ctx buf ofs len
end

module Https = Http.Make (SecureTransport_transport)
include Https
