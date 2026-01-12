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

open Runtime_term

type custom = Runtime_term.custom
type t = Runtime_term.custom_term

let to_string { value; handler } = handler.to_string value
let to_json ~pos { value; handler } = handler.to_json ~pos value

let compare { value = v; handler = h } { value = v'; handler = h' } =
  if h.name <> h.name then Stdlib.compare h.name h'.name else h.compare v v'

let custom_terms = ref []

module type Specs = sig
  type content

  val name : string
  val t : Type.t
  val to_string : content -> string
  val to_json : pos:Pos.t list -> content -> Json.t
  val compare : content -> content -> int
end

module type Implementation = sig
  type content

  val to_custom : content -> t
  val of_custom : t -> content
  val is_custom : t -> bool
end

module Make (S : Specs) = struct
  type content = S.content

  let () =
    if List.mem S.name !custom_terms then failwith "custom term exist!";
    custom_terms := S.name :: !custom_terms

  let to_custom : content -> custom = Obj.magic
  let to_content : custom -> content = Obj.magic
  let to_string v = S.to_string (to_content v)
  let to_json ~pos v = S.to_json ~pos (to_content v)
  let compare v v' = S.compare (to_content v) (to_content v')

  let handler =
    { Runtime_term.typ = S.t; name = S.name; to_string; to_json; compare }

  let to_custom v = { value = to_custom v; handler }

  let of_custom { value; handler = { name } } =
    assert (S.name = name);
    to_content value

  let is_custom { handler = { name } } = S.name = name
end
