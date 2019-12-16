(* -*- mode: tuareg; -*- *)
(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2019 Savonet team

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
    ~d:"" "Path to the server's SSL password. (optional, blank if omited)"

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

module Monad = Duppy.Monad

module type Monad_t = module type of Monad with module Io := Monad.Io

module Websocket_transport = struct
  type socket = Ssl.socket

  let read = Ssl.read
  let read_retry fd = Extralib.read_retry (Ssl.read fd)
  let write = Ssl.write
end

module Duppy_transport : Duppy.Transport_t with type t = Ssl.socket = struct
  type t = Ssl.socket

  type bigarray =
    (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

  let sock = Ssl.file_descr_of_socket
  let read = Ssl.read
  let write = Ssl.write
  let ba_write _ _ _ _ = failwith "Not implemented!"
end

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

module Transport = struct
  type socket = Ssl.socket

  let name = "ssl"
  let file_descr_of_socket = Ssl.file_descr_of_socket
  let read = Ssl.read

  let accept sock =
    let ctx = get_ctx () in
    let s, caller = Unix.accept sock in
    set_socket_default s;
    let ssl_s = Ssl.embed_socket s ctx in
    Ssl.accept ssl_s;
    (ssl_s, caller)

  let close ssl =
    Ssl.shutdown ssl;
    Unix.close (Ssl.file_descr_of_socket ssl)

  module Duppy = struct
    module Io = Duppy.MakeIo (Duppy_transport)

    module Monad = struct
      module Io = Duppy.Monad.MakeIo (Io)
      include (Monad : Monad_t)
    end
  end

  module Http = Https
  module Websocket = Websocket.Make (Websocket_transport)
end

module Ssl = Harbor.Make (Transport)
include Ssl
