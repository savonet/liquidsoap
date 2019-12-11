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

(** Runtime error, should eventually disappear. *)
exception Invalid_value of Lang_values.V.value * string

exception Clock_conflict of (Lang_types.pos option * string * string)

exception Clock_loop of (Lang_types.pos option * string * string)

(** Exception raised by report_error after an error has been displayed.
  * Unknown errors are re-raised, so that their content is not totally lost. *)
exception Error

(** Raise errors for warnings. *)
val strict : bool ref

val report : Sedlexing.lexbuf -> (unit -> unit) -> unit
