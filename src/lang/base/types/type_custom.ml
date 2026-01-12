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

open Type_base

type custom = Type_base.custom

module type Specs = sig
  type content

  val name : string
  val copy_with : (t -> t) -> content -> content
  val occur_check : (t -> unit) -> content -> unit

  val filter_vars :
    (var list -> t -> var list) -> var list -> content -> var list

  val repr : (var list -> t -> Repr.t) -> var list -> content -> Repr.t
  val subtype : (t -> t -> unit) -> content -> content -> unit
  val sup : (t -> t -> t) -> content -> content -> content
  val to_string : content -> string
end

module type Implementation = sig
  type content

  val handler : content -> Type_base.custom_handler
  val to_content : custom -> content
end

let custom_types = ref []

module Make (S : Specs) = struct
  type content = S.content

  let () =
    if List.mem S.name !custom_types then failwith "custom type exist!";
    custom_types := S.name :: !custom_types

  let to_custom : content -> custom = Obj.magic
  let to_content : custom -> content = Obj.magic
  let copy_with fn v = to_custom (S.copy_with fn (to_content v))
  let occur_check fn v = S.occur_check fn (to_content v)
  let filter_vars fn vars v = S.filter_vars fn vars (to_content v)
  let repr fn vars v = S.repr fn vars (to_content v)
  let subtype fn v v' = S.subtype fn (to_content v) (to_content v')
  let sup fn v v' = to_custom (S.sup fn (to_content v) (to_content v'))
  let to_string v = S.to_string (to_content v)

  let handler v =
    {
      typ = to_custom v;
      custom_name = S.name;
      copy_with;
      occur_check;
      filter_vars;
      repr;
      subtype;
      sup;
      to_string;
    }
end
