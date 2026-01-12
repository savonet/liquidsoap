(*****************************************************************************

  Liquidsoap, a programmable stream generator.
  Copyright 2003-2026 Savonet team

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

(** Where the server commands are registered, and the server interfaces are
    created and controlled. *)

open Dtools

(** {2 Configuration parameters} *)

(** The configuration of the server. *)
val conf : Conf.ut

(** Should a socket server be created? *)
val conf_socket : bool Conf.t

(** Path of the socket. *)
val conf_socket_path : string Conf.t

(** Permissions of the socket. **)
val conf_socket_perms : int Conf.t

(** Should a telnet server be created? *)
val conf_telnet : bool Conf.t

(** Address of the telnet server. *)
val conf_telnet_bind_addr : string Conf.t

(** Port of the telnet server. *)
val conf_telnet_port : int Conf.t

(** {2 Commands of the server} *)

(** Namespaces are used to avoid name conflicts. *)
type namespace = string list

(** Get a string representation of a namespace. *)
val to_string : namespace -> string

(** Specialized implementation of conditions to use in server commands. *)
type condition = {
  wait : (unit -> string) -> unit;
  signal : unit -> unit;
  broadcast : unit -> unit;
}

(** [condition ()] instantiates a server command condition. *)
val condition : unit -> condition

(** Partial response write without returning. *)
val write : after:(unit -> string) -> string -> unit

(** Read from the client. *)
val read : after:(string -> string) -> Duppy.Io.marker -> unit

(** [add ~ns ~descr command f] adds a new command [command] in a given namespace
    ~ns. When the command is called, the function [f] is executed with the
    argument of the command as parameter. The return value of [f] is then
    displayed. ~descr is the command description. *)
val add :
  ns:namespace ->
  ?usage:string ->
  descr:string ->
  string ->
  (string -> string) ->
  unit

(** Remove a command from the server. *)
val remove : ns:namespace -> string -> unit

(** [exec command] executes the [command], returns its result.
    @raise Not_found if the command does not exist. *)
val exec : string -> string

(** Register a handle to be executed on server start or immediately if the
    server has already started. *)
val on_start : (unit -> unit) -> unit

(** Start server and telnet if enabled. *)
val start : unit -> unit
