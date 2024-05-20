(*****************************************************************************

  Liquidsoap, a programmable stream generator.
  Copyright 2003-2024 Savonet team

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

type custom = Runtime_term.custom = ..
type t = Runtime_term.custom_term

val to_string : t -> string
val to_json : pos:Pos.t list -> t -> Json.t
val to_descr : t -> Type_base.descr
val compare : t -> t -> int

module type Specs = sig
  type content

  val to_string : content -> string
  val to_json : pos:Pos.t list -> content -> Json.t
  val compare : content -> content -> int
  val typ : (module Type.Custom.Implementation)
end

module type Implementation = sig
  type content

  val to_custom : content -> t
  val of_custom : t -> content
  val is_custom : t -> bool
  val descr : Type_base.descr
end

module Make (S : Specs) : Implementation with type content = S.content
