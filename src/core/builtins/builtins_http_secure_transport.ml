(* -*- mode: tuareg; -*- *)
(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2023 Savonet team

  This program is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details, fully stated in the COPYING
  file at the root of the liquidsoap distribution.

  You should have received a copy of the GNU General Public License
  along with this program; if not, write to the Free Software
  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301  USA

 *****************************************************************************)

let secure_transport_socket transport fd ctx =
  object
    method typ = "secure_transport"
    method transport = transport
    method file_descr = fd

    method wait_for ?log event timeout =
      let event =
        match event with
          | `Read -> `Read fd
          | `Write -> `Write fd
          | `Both -> `Both fd
      in
      Tutils.wait_for ?log event timeout

    method write = SecureTransport.write ctx
    method read = SecureTransport.read ctx

    method close =
      SecureTransport.close ctx;
      Unix.close fd
  end

let transport ~password ~certificate ~key:_ () =
  object (self)
    method name = "secure_transport"
    method protocol = "https"
    method default_port = 443

    method connect ?bind_address host port =
      let sockaddr =
        Unix.ADDR_INET ((Unix.gethostbyname host).Unix.h_addr_list.(0), port)
      in
      let domain =
        match sockaddr with
          | Unix.ADDR_UNIX _ -> Unix.PF_UNIX
          | Unix.ADDR_INET (_, _) -> Unix.PF_INET
      in
      let sock = Unix.socket ~cloexec:true domain Unix.SOCK_STREAM 0 in
      begin
        try Unix.connect sock sockaddr
        with exn ->
          Unix.close sock;
          raise exn
      end;
      begin
        match bind_address with
          | None -> ()
          | Some s ->
              let bind_addr_inet =
                (Unix.gethostbyname s).Unix.h_addr_list.(0)
              in
              (* Seems like you need to bind on port 0 *)
              let bind_addr = Unix.ADDR_INET (bind_addr_inet, 0) in
              Unix.bind sock bind_addr
      end;
      let ctx =
        SecureTransport.init SecureTransport.Client SecureTransport.Stream
      in
      SecureTransport.set_peer_domain_name ctx host;
      SecureTransport.set_connection ctx sock;
      SecureTransport.handshake ctx;
      secure_transport_socket self sock ctx

    method accept sock =
      let sock, caller = Unix.accept ~cloexec:true sock in
      let ctx =
        SecureTransport.init SecureTransport.Server SecureTransport.Stream
      in
      let certs =
        SecureTransport.import_p12_certificate ?password certificate
      in
      List.iter (SecureTransport.set_certificate ctx) certs;
      SecureTransport.set_connection ctx sock;
      SecureTransport.handshake ctx;
      (secure_transport_socket self sock ctx, caller)
  end

let _ =
  Lang.add_builtin ~base:Modules.http_transport "secure_transport"
    ~category:`Liquidsoap ~descr:"Https transport using macos' SecureTransport"
    [
      ( "password",
        Lang.nullable_t Lang.string_t,
        Some Lang.null,
        Some "SSL certificate password" );
      ("certificate", Lang.string_t, None, Some "Path to certificate file");
      ("key", Lang.string_t, None, Some "Path to certificate private key");
    ]
    Lang.http_transport_t
    (fun p ->
      let password =
        Lang.to_valued_option Lang.to_string (List.assoc "password" p)
      in
      let certificate = Lang.to_string (List.assoc "certificate" p) in
      let key = Lang.to_string (List.assoc "key" p) in
      Lang.http_transport (transport ~password ~certificate ~key ()))
