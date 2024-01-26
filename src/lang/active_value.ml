(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2023 Savonet team

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

(* Weak hashes to track values that have not been collected. *)

module type T = sig
  type t
end

module Make (T : T) = struct
  type entry = { id : int; value : T.t }

  include Weak.Make (struct
    type t = entry

    let equal t t' = t.id = t'.id
    let hash t = t.id
  end)

  type data = T.t

  let mk value = { id = Hashtbl.hash value; value }
  let merge t v = (merge t (mk v)).value
  let add t v = add t (mk v)
  let remove t v = remove t (mk v)
  let find t v = (find t (mk v)).value
  let find_opt t v = Option.map (fun { value; _ } -> value) (find_opt t (mk v))
  let find_all t v = List.map (fun { value; _ } -> value) (find_all t (mk v))
  let mem t v = mem t (mk v)
  let iter fn = iter (fun { value; _ } -> fn value)
  let fold fn = fold (fun { value; _ } acc -> fn value acc)
end
