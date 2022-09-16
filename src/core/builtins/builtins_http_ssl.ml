(* -*- mode: tuareg; -*- *)
(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2022 Savonet team

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

open Dtools

let conf_harbor_ssl =
  Conf.void ~p:(Harbor_base.conf_harbor#plug "ssl") "Harbor SSL settings."

let conf_harbor_ssl_certificate =
  Conf.string
    ~p:(conf_harbor_ssl#plug "certificate")
    ~d:"" "Path to the server's SSL certificate. (mandatory)"

let conf_harbor_ssl_private_key =
  Conf.string
    ~p:(conf_harbor_ssl#plug "private_key")
    ~d:"" "Path to the server's SSL private key. (mandatory)"

let conf_harbor_ssl_password =
  Conf.string
    ~p:(conf_harbor_ssl#plug "password")
    ~d:"" "Path to the server's SSL password. (optional, blank if omitted)"

let conf_harbor_ssl_read_timeout =
  Conf.float
    ~p:(conf_harbor_ssl#plug "read_timeout")
    ~d:(-1.)
    "Read timeout on SSL sockets. Set to zero to never timeout, ignored \
     (system default) if negative."

let conf_harbor_ssl_write_timeout =
  Conf.float
    ~p:(conf_harbor_ssl#plug "write_timeout")
    ~d:(-1.)
    "Read timeout on SSL sockets. Set to zero to never timeout, ignored \
     (system default) if negative."

let m = Mutex.create ()
let ctx = ref None

let get_ctx =
  Tutils.mutexify m (fun () ->
      match !ctx with
        | Some ctx -> ctx
        | None ->
            let _ctx = Ssl.create_context Ssl.SSLv23 Ssl.Server_context in
            let password = conf_harbor_ssl_password#get in
            if password != "" then
              Ssl.set_password_callback _ctx (fun _ -> password);
            Ssl.use_certificate _ctx conf_harbor_ssl_certificate#get
              conf_harbor_ssl_private_key#get;
            ctx := Some _ctx;
            _ctx)

let set_socket_default fd =
  if conf_harbor_ssl_read_timeout#get >= 0. then
    Unix.setsockopt_float fd Unix.SO_RCVTIMEO conf_harbor_ssl_read_timeout#get;
  if conf_harbor_ssl_write_timeout#get >= 0. then
    Unix.setsockopt_float fd Unix.SO_SNDTIMEO conf_harbor_ssl_write_timeout#get

let ssl_socket transport ssl =
  object
    method typ = "ssl"
    method transport = transport
    method file_descr = Ssl.file_descr_of_socket ssl

    method wait_for ?log event timeout =
      let event =
        match event with
          | `Read -> `Read (Ssl.file_descr_of_socket ssl)
          | `Write -> `Write (Ssl.file_descr_of_socket ssl)
          | `Both -> `Both (Ssl.file_descr_of_socket ssl)
      in
      Tutils.wait_for ?log event timeout

    method read = Ssl.read ssl
    method write = Ssl.write ssl

    method close =
      Ssl.shutdown ssl;
      Unix.close (Ssl.file_descr_of_socket ssl)
  end

let transport =
  object (self)
    method name = "ssl"
    method protocol = "https"
    method default_port = 443

    method connect ?bind_address host port =
      let socketaddr =
        Unix.ADDR_INET ((Unix.gethostbyname host).Unix.h_addr_list.(0), port)
      in
      let ctx = Ssl.create_context Ssl.SSLv23 Ssl.Client_context in
      (* TODO: add option.. *)
      Ssl.set_verify ctx [] (Some Ssl.client_verify_callback);
      Ssl.set_verify_depth ctx 3;
      ignore (Ssl.set_default_verify_paths ctx);
      let domain =
        match socketaddr with
          | Unix.ADDR_UNIX _ -> Unix.PF_UNIX
          | Unix.ADDR_INET (_, _) -> Unix.PF_INET
      in
      let unix_socket = Unix.socket domain Unix.SOCK_STREAM 0 in
      let socket =
        try
          Unix.connect unix_socket socketaddr;
          let socket = Ssl.embed_socket unix_socket ctx in
          (try Ssl.set_client_SNI_hostname socket host with _ -> ());
          Ssl.connect socket;
          socket
        with exn ->
          Unix.close unix_socket;
          raise exn
      in
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
      ssl_socket self socket

    method accept sock =
      let s, caller = Unix.accept ~cloexec:true sock in
      set_socket_default s;
      let ctx = get_ctx () in
      let ssl_s = Ssl.embed_socket s ctx in
      Ssl.accept ssl_s;
      (ssl_socket self ssl_s, caller)
  end

let () =
  Builtins_http.add_transport ~descr:"Https transport using libssl" ~name:"ssl"
    transport
