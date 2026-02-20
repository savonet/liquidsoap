(*****************************************************************************

    Liquidsoap, a programmable stream generator.
    Copyright 2003-2016 Savonet team

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

module Monad = Duppy.Monad
module Type = Liquidsoap_lang.Type
module Http = Liq_http

module type Monad_t = module type of Monad with module Io := Monad.Io

module type Transport_t = sig
  type socket = Http.socket

  val file_descr_of_socket : socket -> Unix.file_descr
  val read : socket -> bytes -> int -> int -> int
  val write : socket -> bytes -> int -> int -> int
  val close : socket -> unit

  module Duppy : sig
    module Io : Duppy.Io_t with type socket = socket

    module Monad : sig
      module Io :
        Duppy.Monad.Monad_io_t with type socket = socket and module Io = Io

      include Monad_t
    end
  end

  module Websocket : Websocket.Websocket_t with type socket = socket
end

module Http_transport : Transport_t with type socket = Http.socket

module type T = sig
  type socket

  exception Retry
  exception Assoc of string
  exception Not_authenticated
  exception Unknown_codec
  exception Mount_taken
  exception Websocket_closed
  exception Protocol_not_supported of string

  val file_descr_of_socket : socket -> Unix.file_descr
  val read : socket -> bytes -> int -> int -> int
  val write : socket -> bytes -> int -> int -> int
  val close : socket -> unit

  type http_verb = [ `Get | `Post | `Put | `Delete | `Head | `Options ]

  type reply =
    | Close of (unit -> string)
    | Relay of string * (unit -> unit)
    | Custom

  type http_handler =
    protocol:string ->
    meth:http_verb ->
    data:(float -> string) ->
    headers:(string * string) list ->
    query:(string * string) list ->
    socket:socket ->
    string ->
    (reply, reply) Duppy.Monad.t

  val verb_of_string : string -> http_verb
  val string_of_verb : http_verb -> string
  val mk_simple : string -> unit -> string
  val simple_reply : string -> ('a, reply) Duppy.Monad.t
  val reply : (unit -> string) -> ('a, reply) Duppy.Monad.t
  val custom : unit -> ('a, reply) Duppy.Monad.t

  val add_http_handler :
    pos:Liquidsoap_lang.Pos.t list ->
    transport:Http.transport ->
    port:int ->
    verb:http_verb ->
    uri:Lang.regexp ->
    http_handler ->
    unit

  val remove_http_handler :
    port:int -> verb:http_verb -> uri:Lang.regexp -> unit -> unit

  class virtual source : object
    inherit Source.source

    method virtual relay :
      string ->
      (string * string) list ->
      ?read:(socket -> bytes -> int -> int -> int) ->
      socket ->
      unit

    method virtual encode_metadata : Frame.metadata -> unit
    method virtual login : string * (socket:socket -> string -> string -> bool)
    method virtual icy_charset : string option
    method virtual meta_charset : string option
    method virtual get_mime_type : string option
  end

  val http_auth_check :
    ?query:(string * string) list ->
    login:string * (socket:socket -> string -> string -> bool) ->
    socket ->
    (string * string) list ->
    (unit, reply) Duppy.Monad.t

  val relayed : string -> (unit -> unit) -> ('a, reply) Duppy.Monad.t

  val add_source :
    pos:Liquidsoap_lang.Pos.t list ->
    transport:Http.transport ->
    port:int ->
    mountpoint:string ->
    icy:bool ->
    source ->
    unit

  val remove_source : port:int -> mountpoint:string -> unit -> unit
end

module Make (T : Transport_t) : T with type socket = T.socket
include T with type socket = Http.socket
