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
        match b with
          | Var _ -> satisfies b
          | Tuple { t = [] } when m = [] -> raise Unsatisfied_constraint
          | Tuple { t = [] } -> ()
          | _ -> raise Unsatisfied_constraint);
  }

let num_constr =
  {
    constr_descr = "a number type";
    univ_descr = None;
    satisfied =
      (fun ~subtype:_ ~satisfies b ->
        let b = demeth b in
        match b with
          | Var _ -> satisfies b
          | Never _ | Int _ | Float _ -> ()
          | _ -> raise Unsatisfied_constraint);
  }

let ord_constr =
  {
    constr_descr = "an orderable type";
    univ_descr = None;
    satisfied =
      (fun ~subtype:_ ~satisfies b ->
        let m, b = split_meths b in
        match b with
          | Var _ -> satisfies b
          | Custom _ | Int _ | Float _ | String _ | Bool _ | Never _ -> ()
          | Tuple { t = [] } ->
              (* For records, we want to ensure that all fields are ordered. *)
              List.iter
                (fun { scheme = v, a } ->
                  if v <> [] then raise Unsatisfied_constraint;
                  satisfies a)
                m
          | Tuple { t } -> List.iter satisfies t
          | List { t } | Nullable { t } -> satisfies t
          | _ -> raise Unsatisfied_constraint);
  }
