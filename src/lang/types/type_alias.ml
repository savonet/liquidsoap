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

type alias = { name : string; typ : Type_base.t }
type Type_base.custom += Type of alias

let get = function Type { typ } -> typ | _ -> assert false

let handler ~name typ =
  {
    Type_base.typ = Type { name; typ };
    copy_with =
      (fun aux -> function
        | Type { name; typ } -> Type { name; typ = aux typ }
        | _ -> assert false);
    occur_check = (fun occur_check v c -> occur_check v (get c));
    filter_vars = (fun aux l c -> aux l (get c));
    repr =
      (fun repr g -> function
        | Type { name; typ } ->
            `Constr
              ( name,
                [
                  ( Type_base.Invariant,
                    `Constr ("alias", [(Type_base.Invariant, repr g typ)]) );
                ] )
        | _ -> assert false);
    satisfies_constraint =
      (fun check t cons ->
        let t =
          match t.Type_base.descr with
            | Type_base.Custom { Type_base.typ = Type { typ } } -> typ
            | _ -> assert false
        in
        check t cons);
    subtype = (fun _ _ _ -> assert false);
    sup = (fun _ _ _ -> assert false);
    to_string = (fun c -> Type_base.to_string (get c));
  }
