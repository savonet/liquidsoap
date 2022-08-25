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

include Type_base
module Ground = Ground_type

let num_constr : constr =
  object (self)
    method t = Num
    method descr = "a number type"

    method satisfied b =
      let b = demeth b in
      match b.descr with
        | Custom { typ = Ground.Int.Type } | Custom { typ = Ground.Float.Type }
          ->
            ()
        | Custom { typ; satisfies_constraint } -> satisfies_constraint typ self
        | Var { contents = Free v } ->
            if not (List.exists (fun c -> c#t = Num) v.constraints) then
              v.constraints <- self :: v.constraints
        | _ -> raise Unsatisfied_constraint
  end

let ord_constr : constr =
  object (self)
    method t = Ord
    method descr = "an orderable type"

    method satisfied b =
      let rec check b =
        let m, b = split_meths b in
        match b.descr with
          | Custom { typ; satisfies_constraint } ->
              satisfies_constraint typ self
          | Var { contents = Free v } ->
              if not (List.exists (fun c -> c#t = Ord) v.constraints) then
                v.constraints <- self :: v.constraints
          | Tuple [] ->
              (* For records, we want to ensure that all fields are ordered. *)
              List.iter
                (fun { scheme = v, a } ->
                  if v <> [] then raise Unsatisfied_constraint;
                  check a)
                m
          | Tuple l -> List.iter (fun b -> check b) l
          | List { t = b } -> check b
          | Nullable b -> check b
          | _ -> raise Unsatisfied_constraint
      in
      check b
  end
