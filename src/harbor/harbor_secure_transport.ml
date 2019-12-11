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

type secure_transport_socket = Https_secure_transport.socket = {
  ctx: SecureTransport.t;
  sock: Unix.file_descr;
}

open Dtools

let conf_harbor_secure_transport =
  Conf.void
    ~p:(Harbor_base.conf_harbor#plug "secure_transport")
    "Harbor SSL settings."

let conf_harbor_secure_transport_certificate =
  Conf.string
    ~p:(conf_harbor_secure_transport#plug "certificate")
    ~d:"" "Path to the server's SSL certificate. (mandatory)"

let conf_harbor_secure_transport_private_key =
  Conf.string
    ~p:(conf_harbor_secure_transport#plug "private_key")
    ~d:"" "Path to the server's SSL private key. (mandatory)"

let conf_harbor_secure_transport_password =
  Conf.string
    ~p:(conf_harbor_secure_transport#plug "password")
    ~d:"" "Path to the server's SSL password. (optional, blank if omited)"

module Monad = Duppy.Monad

module type Monad_t = module type of Monad with module Io := Monad.Io

module Websocket_transport = struct
  type socket = secure_transport_socket

  let read {ctx} buf ofs len = SecureTransport.read ctx buf ofs len

  let read_retry fd = Extralib.read_retry (read fd)

  let write {ctx} buf ofs len = SecureTransport.write ctx buf ofs len
end

module Duppy_transport :
  Duppy.Transport_t with type t = secure_transport_socket = struct
  type t = secure_transport_socket

  type bigarray =
    (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

  let sock {sock} = sock

  let read {ctx} buf ofs len = SecureTransport.read ctx buf ofs len

  let write {ctx} buf ofs len = SecureTransport.write ctx buf ofs len

  let ba_write _ _ _ _ = failwith "Not implemented!"
end

module Transport = struct
  type socket = secure_transport_socket

  let name = "secure_transport"

  let file_descr_of_socket {sock} = sock

  let read {ctx} buf ofs len = SecureTransport.read ctx buf ofs len

  let accept sock =
    let sock, caller = Unix.accept sock in
    let ctx =
      SecureTransport.init SecureTransport.Server SecureTransport.Stream
    in
    let password = conf_harbor_secure_transport_password#get in
    let password = if password = "" then None else Some password in
    let cert = conf_harbor_secure_transport_certificate#get in
    let certs = SecureTransport.import_p12_certificate ?password cert in
    List.iter (SecureTransport.set_certificate ctx) certs ;
    SecureTransport.set_connection ctx sock ;
    SecureTransport.handshake ctx ;
    let socket = {sock; ctx} in
    (socket, caller)

  let close {ctx} = SecureTransport.close ctx

  module Duppy = struct
    module Io = Duppy.MakeIo (Duppy_transport)

    module Monad = struct
      module Io = Duppy.Monad.MakeIo (Io)

      include (Monad : Monad_t)
    end
  end

  module Http = Https_secure_transport
  module Websocket = Websocket.Make (Websocket_transport)
end

module Ssl = Harbor.Make (Transport)
include Ssl
