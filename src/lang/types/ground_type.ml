(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
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
  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301  USA

 *****************************************************************************)

let types = Hashtbl.create 10
let resolve_opt t = Hashtbl.find_opt types t

module type Spec = sig
  type t

  val name : string
end

module type Custom = sig
  type Type_base.custom += Type

  val descr : Type_base.descr
end

module Make (S : Spec) = struct
  type Type_base.custom += Type

  let get = function Type -> Type | _ -> assert false

  let handler =
    {
      Type_base.typ = Type;
      copy_with = (fun _ _ c -> get c);
      occur_check = (fun _ c -> ignore (get c));
      filter_vars =
        (fun _ l _ c ->
          ignore (get c);
          l);
      print =
        (fun f c ->
          ignore (get c);
          Format.fprintf f "%s" S.name;
          Type_base.DS.empty);
      satisfies_constraint = (fun _ _ c -> ignore (get c));
      subtype =
        (fun _ c c' ->
          match
            ( (Type_base.deref c).Type_base.descr,
              (Type_base.deref c').Type_base.descr )
          with
            | ( Type_base.Custom { Type_base.typ = Type },
                Type_base.Custom { Type_base.typ = Type } ) ->
                ()
            | _ -> assert false);
      sup =
        (fun _ c c' ->
          match
            ( (Type_base.deref c).Type_base.descr,
              (Type_base.deref c').Type_base.descr )
          with
            | ( Type_base.Custom { Type_base.typ = Type },
                Type_base.Custom { Type_base.typ = Type } ) ->
                c
            | _ -> assert false);
      to_string =
        (fun c ->
          ignore (get c);
          S.name);
    }

  let descr = Type_base.Custom handler
  let () = Hashtbl.add types S.name descr
end

module Int = Make (struct
  type t = int

  let name = "int"
end)

let int = Int.descr

module Float = Make (struct
  type t = float

  let name = "float"
end)

let float = Float.descr

module String = Make (struct
  type t = string

  let name = "string"
end)

let string = String.descr

module Bool = Make (struct
  type t = bool

  let name = "bool"
end)

let bool = Bool.descr
