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
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 *****************************************************************************)

(** Where the server commands are registered, and the server interfaces are
  * created and controlled. *)

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

(** Namespaces are used to avoid name conflicts. Any entity which wants to add
  * commands to the server should first get a new unique namespace, in which the
  * commands will be added. *)
type namespace = string list

(** Get a string representation of a namespace. *)
val to_string : namespace -> string

(** [register hint kind] creates a new namespace and registers it.
  * If [hint] is not used, it will be used as the new namespace. Otherwise,
  * some numeral suffix will be appended to it in order to distinguish it.
  * The [kind] is meant to provide information about what kind of namespace
  * this is. In liquidsoap, it tells which kind of operator owns the namespace,
  * and hence which commands should be expected in it. *)
val register : namespace -> string -> namespace

(** Release a namespace, deleting all associated commands. *)
val unregister : namespace -> unit

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

(** [add ~ns ~descr command f] adds a new command [command] in a given namespace ~ns.
  * When the command is called, the function [f] is executed with the argument of
  * the command as parameter. The return value of [f] is then displayed. ~descr is
  * the command description. *)
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
  * @raise Not_found if the command does not exist.  *)
val exec : string -> string
