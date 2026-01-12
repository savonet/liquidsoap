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

include Type_base
include Ref_type
module Custom = Type_custom

let record_constr =
  {
    constr_descr = "a record type";
    univ_descr = None;
    satisfied =
      (fun ~subtype:_ ~satisfies b ->
        let m, b = split_meths b in
        match b.descr with
          | Var _ -> satisfies b
          | Tuple [] when m = [] -> raise Unsatisfied_constraint
          | Tuple [] -> ()
          | _ -> raise Unsatisfied_constraint);
  }

let num_constr =
  {
    constr_descr = "a number type";
    univ_descr = None;
    satisfied =
      (fun ~subtype:_ ~satisfies b ->
        let b = demeth b in
        match b.descr with
          | Var _ -> satisfies b
          | Never | Int | Float -> ()
          | _ -> raise Unsatisfied_constraint);
  }

let ord_constr =
  {
    constr_descr = "an orderable type";
    univ_descr = None;
    satisfied =
      (fun ~subtype:_ ~satisfies b ->
        let m, b = split_meths b in
        match b.descr with
          | Var _ -> satisfies b
          | Custom _ | Int | Float | String | Bool | Never -> ()
          | Constr c -> List.iter (fun (_, t) -> satisfies t) c.params
          | Tuple [] ->
              (* For records, we want to ensure that all fields are ordered. *)
              List.iter
                (fun { scheme = v, a } ->
                  if v <> [] then raise Unsatisfied_constraint;
                  satisfies a)
                m
          | Tuple l -> List.iter satisfies l
          | List { t = b } -> satisfies b
          | Nullable b -> satisfies b
          | _ -> raise Unsatisfied_constraint);
  }
