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
    copy_with = (fun _ c -> Type (Content.duplicate (get c)));
    occur_check = (fun _ _ c -> ignore (get c));
    filter_vars =
      (fun _ l _ c ->
        ignore (get c);
        l);
    repr = (fun _ _ c -> `Constr (Content.string_of_format (get c), []));
    satisfies_constraint =
      (fun _ c -> function
        | InternalMedia when Content.(is_internal_format (get c)) -> ()
        | _ -> assert false);
    subtype = (fun _ c c' -> Content.merge (get c) (get c'));
    sup =
      (fun _ c c' ->
        Content.merge (get c) (get c');
        c);
    to_string = (fun c -> Content.string_of_format (get c));
  }

let descr f = Type_base.Custom (handler f)
