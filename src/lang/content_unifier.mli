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

(* Abstract module for resolving stream content during typecheck
   and runtime. *)

exception Conflict of string * string

type t
type content

val make : ?fields:content Frame.Fields.t -> sealed:bool -> unit -> t
val make_content : Frame.kind -> content
val sealed : t -> bool
val fields : t -> content Frame.Fields.t
val copy : t -> t
val content : content -> Frame.kind
val ( <: ) : t -> t -> unit
val sup : t -> t -> t
val unify_content : content -> content -> unit
val to_string : t -> string
