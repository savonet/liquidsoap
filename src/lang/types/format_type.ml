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

type Type_base.custom += Type of Content.format

let get = function Type c -> c | _ -> assert false

let handler f =
  {
    Type_base.typ = Type f;
    copy_with = (fun _ _ c -> Type (Content.duplicate (get c)));
    occur_check = (fun _ c -> ignore (get c));
    filter_vars =
      (fun _ l _ c ->
        ignore (get c);
        l);
    print =
      (fun f c ->
        Format.fprintf f "%s" (Content.string_of_format (get c));
        Type_base.DS.empty);
    satisfies_constraint = (fun _ _ c -> ignore (get c));
    subtype =
      (fun _ c c' ->
        match
          ( (Type_base.deref c).Type_base.descr,
            (Type_base.deref c').Type_base.descr )
        with
          | ( Type_base.Custom { Type.typ = Type f },
              Type_base.Custom { Type.typ = Type f' } ) ->
              Content.merge f f'
          | _ -> assert false);
    sup =
      (fun _ c c' ->
        match
          ( (Type_base.deref c).Type_base.descr,
            (Type_base.deref c').Type_base.descr )
        with
          | ( Type_base.Custom { Type.typ = Type f },
              Type_base.Custom { Type.typ = Type f' } ) ->
              Content.merge f f';
              c
          | _ -> assert false);
    to_string = (fun c -> Content.string_of_format (get c));
  }

let descr f = Type_base.Custom (handler f)
