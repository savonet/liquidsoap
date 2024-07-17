(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2006 Savonet team

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

type namespace = string list
val to_string : namespace -> string

(** [register namespace kind]. [kind] is the type of operator *)
val register : namespace -> string -> namespace
val add :
      ns:namespace -> ?usage:string -> string -> (string -> string) -> unit
val remove : ns:namespace -> string -> unit

(** [exec command] executes the [command], returns its result.
  * @raise Not_found if the command does not exist.  *)
val exec : string -> string

(** {1 Multi-socket event engine} *)

type task =
  | Read    of (Unix.file_descr * (unit -> unit))
  | Write   of (Unix.file_descr * (unit -> unit))

(** [scheduler f] passes to [f] a way to add events.
  * WARNING: TODO: There should always be at least one event waited for,
  * otherwise the scheduler will block forever. *)
val scheduler : ((task -> unit) -> unit) -> unit
