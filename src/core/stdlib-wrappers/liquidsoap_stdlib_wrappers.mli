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

(** Stdlib calls raising their failures as runtime errors of a given kind. *)

(** Filesystem calls, raising errors of kind [file]. *)
module File : sig
  val readdir : string -> string array
  val digest : string -> Digest.t
  val temp_file : ?temp_dir:string -> string -> string -> string
  val temp_dir : string -> string -> string

  val openfile :
    string -> Unix.open_flag list -> Unix.file_perm -> Unix.file_descr

  val copy :
    recurse:bool -> force:bool -> preserve:bool -> string -> string -> unit

  val move : force:bool -> string -> string -> unit

  (** Atomic rename. Raises [file.cross_device] when [src] and [dst] are on
      different filesystems. *)
  val rename : string -> string -> unit
end

(** Socket calls, raising errors of kind [socket]. *)
module Socket : sig
  val socket :
    ?cloexec:bool ->
    Unix.socket_domain ->
    Unix.socket_type ->
    int ->
    Unix.file_descr

  val socketpair :
    ?cloexec:bool ->
    Unix.socket_domain ->
    Unix.socket_type ->
    int ->
    Unix.file_descr * Unix.file_descr

  val bind : Unix.file_descr -> Unix.sockaddr -> unit
  val listen : Unix.file_descr -> int -> unit
  val connect : Unix.file_descr -> Unix.sockaddr -> unit
end
