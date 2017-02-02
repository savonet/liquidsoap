(* -*- mode: tuareg; -*- *)
(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2017 Savonet team

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
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 *****************************************************************************)

open Dtools

let conf_harbor_secure_transport =
  Conf.void ~p:(Harbor_base.conf_harbor#plug "ssl")
    "Harbor SSL (OSX SecureTransport implementation) settings."
let conf_harbor_secure_transport_certificate =
  Conf.string ~p:(conf_harbor_secure_transport#plug "certificate") ~d:""
    "Path to the server's SSL certificate. (mandatory)"
let conf_harbor_secure_transport_password =
  Conf.string ~p:(conf_harbor_secure_transport#plug "password") ~d:""
    "Path to the server's SSL password. (optional, blank if omited)"

module Monad = Duppy.Monad
module type Monad_t = module type of Monad with module Io := Monad.Io

type handler = Https_secure_transport.socket = {
  ctx: SecureTransport.t;
  sock: Unix.file_descr
}

let read {ctx} buf ofs len =
  SecureTransport.read ctx buf ofs len
let read_retry = Stdlib.read_retry read
let write {ctx} buf ofs len =
  SecureTransport.write ctx buf ofs len
let sock {sock} = sock

module Websocket_transport =
struct
  type socket = handler
  let read = read
  let read_retry = Stdlib.read_retry read
  let write = write
end

module Duppy_transport : Duppy.Transport_t with type t = handler =
struct
  type t = handler
  type bigarray = (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t
  let sock = sock
  let read = read
  let write = write
  let ba_write _ _ _ _ =
    failwith "Not implemented!"
end

module Transport =
struct
  type socket = handler
  let file_descr_of_socket = sock
  let read socket len =
    let buf = Bytes.create len in
    let n = read socket buf 0 len in
    buf, n
  let accept sock =
    let (sock, caller) = Unix.accept sock in
    let ctx =
      SecureTransport.init SecureTransport.Server SecureTransport.Stream
    in
    let password =
      conf_harbor_secure_transport_password#get
    in
    let password =
      if password = "" then None else Some password
    in
    let cert = conf_harbor_secure_transport_certificate#get in
    begin match SecureTransport.import_p12_certificate ?password cert with
      | cert::_ -> SecureTransport.set_certificate ctx cert
      | [] -> failwith "No certificate found in p12 file!"
    end;
    SecureTransport.set_connection ctx sock;
    ({ctx;sock}, caller)
  let close {ctx;sock} = 
    SecureTransport.close ctx;
    Unix.close sock

  module Duppy =
  struct
    module Io = Duppy.MakeIo(Duppy_transport)
    module Monad =
    struct
      module Io = Duppy.Monad.MakeIo(Io)
      include (Monad : Monad_t)
    end
  end

  module Http = Https_secure_transport
  module Websocket = Websocket.Make(Websocket_transport)
end

module SecureTransport = Harbor.Make(Transport)

include SecureTransport
