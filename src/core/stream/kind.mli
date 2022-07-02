(*****************************************************************************

  Liquidsoap, a programmable stream generator.
  Copyright 2003-2022 Savonet team

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

(** Content kind represent incomplete content specifications
  * and are used in sources to negociate a baseline of content
  * that works through a content production pipeline. *)

type kind
type t = kind Frame.Fields.t

val of_kind : Frame.content_kind -> t
val to_string : t -> string

exception Conflict of string * string

val unify_kind : kind -> kind -> unit
val unify : t -> t -> unit
val set_audio : t -> Frame.kind -> t
val set_video : t -> Frame.kind -> t
val set_midi : t -> Frame.kind -> t
val content_type : t -> Frame.content_type
