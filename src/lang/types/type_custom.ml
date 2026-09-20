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

  (** How a payload survives a dump, which carries strings and no more. [parse]
      answers [None] for a printed form it cannot rebuild. *)
  val serialize : content -> string

  val parse : string -> content option
end

module type Implementation = sig
  type content

  val handler : content -> Type_base.custom_instance
  val to_content : custom -> content

  (** What a custom type of this one's name carries here. *)
  val payload : Type_base.custom_instance -> content
end

let registered_custom_type_names = ref []

(* How to rebuild each custom type implemented here from what a dump wrote. *)
let parsers : (string, string -> Type_base.custom_handler option) Hashtbl.t =
  Hashtbl.create 16

let of_dump name payload =
  match Hashtbl.find_opt parsers name with
    | Some parse -> parse payload
    | None -> None

module Make (S : Specs) = struct
  type content = S.content

  (* See [Type_base.custom]: erasure has to stay [Obj.magic] so that custom
     types remain marshalable for the typechecking cache. Only [handler] below
     pairs a payload with its dispatch table, so the cast is never applied
     across instantiations. *)
  let to_custom : content -> custom = Obj.magic
  let to_content : custom -> content = Obj.magic
  let copy_with fn v = to_custom (S.copy_with fn (to_content v))
  let occur_check fn v = S.occur_check fn (to_content v)
  let filter_vars fn vars v = S.filter_vars fn vars (to_content v)
  let repr fn vars v = S.repr fn vars (to_content v)
  let subtype fn v v' = S.subtype fn (to_content v) (to_content v')
  let sup fn v v' = to_custom (S.sup fn (to_content v) (to_content v'))

  let () =
    if List.mem S.name !registered_custom_type_names then
      failwith ("Custom type already registered: " ^ S.name);
    registered_custom_type_names := S.name :: !registered_custom_type_names

  let serialize v = S.serialize (to_content v)

  let dispatch typ =
    { typ; serialize; copy_with; occur_check; filter_vars; repr; subtype; sup }

  let () =
    Hashtbl.replace parsers S.name (fun payload ->
        Option.map (fun v -> dispatch (to_custom v)) (S.parse payload))

  let payload c = to_content (Type_base.custom_handler c).typ

  let handler v =
    { custom_name = S.name; handler_state = Resolved (dispatch (to_custom v)) }
end
