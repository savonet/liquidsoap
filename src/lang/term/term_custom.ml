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

open Runtime_term

type custom = Runtime_term.custom = ..
type t = Runtime_term.custom_term

let to_string { value; handler } = handler.to_string value
let to_json ~pos { value; handler } = handler.to_json ~pos value

let to_descr { handler } =
  let module C = (val handler.typ : Type.Custom.Implementation) in
  C.descr

let compare { value = v; handler } { value = v' } = handler.compare v v'

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

module Make (S : Specs) = struct
  include S

  type custom += Term of content

  let of_content = function Term v -> v | _ -> assert false
  let of_custom = function { value = Term v } -> v | _ -> assert false
  let is_custom = function { value = Term _ } -> true | _ -> false
  let to_string = function Term v -> S.to_string v | _ -> assert false
  let to_json ~pos = function Term v -> S.to_json ~pos v | _ -> assert false

  let descr =
    let module C = (val S.typ : Type.Custom.Implementation) in
    C.descr

  let handler =
    {
      to_string;
      to_json;
      compare = (fun v v' -> S.compare (of_content v) (of_content v'));
      typ = S.typ;
    }

  let to_custom v = { value = Term v; handler }
end
