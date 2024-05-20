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

module type Spec = sig
  val name : string
end

module type Implementation = sig
  val descr : Type_base.descr
  val is_descr : Type_base.descr -> bool
end

module Make (S : Spec) = struct
  type Type_base.custom += Type

  let is_descr = function
    | Type_base.Custom { Type_base.typ = Type } -> true
    | _ -> false

  let handler =
    {
      Type_base.typ = Type;
      copy_with = (fun _ c -> c);
      occur_check = (fun _ _ -> ());
      filter_vars = (fun _ l _ -> l);
      repr = (fun _ _ _ -> `Constr (S.name, []));
      subtype = (fun _ c c' -> assert (c = c'));
      sup =
        (fun _ c c' ->
          assert (c = c');
          c);
      to_string = (fun _ -> S.name);
    }

  let descr = Type_base.Custom handler

  let () =
    Type_base.register_type S.name (fun () ->
        Type_base.make (Type_base.Custom handler))
end
