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

(** EINTR-safe wrappers around Unix I/O calls. Each function retries
    automatically when interrupted by a signal. *)

val poll :
  Unix.file_descr list ->
  Unix.file_descr list ->
  Unix.file_descr list ->
  float ->
  Unix.file_descr list * Unix.file_descr list * Unix.file_descr list

val select :
  Unix.file_descr list ->
  Unix.file_descr list ->
  Unix.file_descr list ->
  float ->
  Unix.file_descr list * Unix.file_descr list * Unix.file_descr list

val read : Unix.file_descr -> bytes -> int -> int -> int
val write : Unix.file_descr -> bytes -> int -> int -> int
val accept : ?cloexec:bool -> Unix.file_descr -> Unix.file_descr * Unix.sockaddr
val recv : Unix.file_descr -> bytes -> int -> int -> Unix.msg_flag list -> int

val recvfrom :
  Unix.file_descr ->
  bytes ->
  int ->
  int ->
  Unix.msg_flag list ->
  int * Unix.sockaddr

val send : Unix.file_descr -> bytes -> int -> int -> Unix.msg_flag list -> int

val sendto :
  Unix.file_descr ->
  bytes ->
  int ->
  int ->
  Unix.msg_flag list ->
  Unix.sockaddr ->
  int

val mkdir : string -> Unix.file_perm -> unit
