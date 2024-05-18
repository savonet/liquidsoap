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

let () =
  let t = Lang.univ_t ~constraints:[Type.ord_constr] () in
  let register_op ~ground_op name op =
    ignore
      (Lang.add_builtin name ~category:`Bool
         ~descr:"Comparison of comparable values."
         [("", t, None, None); ("", t, None, None)]
         Lang.bool_t
         (fun p ->
           let v = Lang.assoc "" 1 p in
           let v' = Lang.assoc "" 2 p in
           Lang.bool
             (match (v.Value.value, v'.Value.value) with
               | Value.Custom g, Value.Custom g' ->
                   op (Term.Custom.compare g g')
               | (Value.Int _ as v), (Value.Int _ as v')
               | (Value.Float _ as v), (Value.Float _ as v')
               | (Value.String _ as v), (Value.String _ as v')
               | (Value.Bool _ as v), (Value.Bool _ as v') ->
                   ground_op v v'
               | _ -> op (Value.compare v v'))))
  in
  register_op ~ground_op:(fun c c' -> c == c') "==" (fun c -> c = 0);
  register_op ~ground_op:(fun c c' -> c <> c') "!=" (fun c -> c <> 0);
  register_op ~ground_op:(fun c c' -> c < c') "<" (fun c -> c = -1);
  register_op ~ground_op:(fun c c' -> c <= c') "<=" (fun c -> c <> 1);
  register_op ~ground_op:(fun c c' -> c >= c') ">=" (fun c -> c <> -1);
  register_op ~ground_op:(fun c c' -> c > c') ">" (fun c -> c = 1)

let _ =
  Lang.add_builtin "and" ~category:`Bool
    ~descr:"Return the conjunction of its arguments"
    [
      ("", Lang.getter_t Lang.bool_t, None, None);
      ("", Lang.getter_t Lang.bool_t, None, None);
    ]
    Lang.bool_t
    (fun p ->
      let a = Lang.to_bool_getter (Lang.assoc "" 1 p) in
      let b = Lang.to_bool_getter (Lang.assoc "" 2 p) in
      Lang.bool (if a () then b () else false))

let _ =
  Lang.add_builtin "or" ~category:`Bool
    ~descr:"Return the disjunction of its arguments"
    [
      ("", Lang.getter_t Lang.bool_t, None, None);
      ("", Lang.getter_t Lang.bool_t, None, None);
    ]
    Lang.bool_t
    (fun p ->
      let a = Lang.to_bool_getter (Lang.assoc "" 1 p) in
      let b = Lang.to_bool_getter (Lang.assoc "" 2 p) in
      Lang.bool (if a () then true else b ()))

let _ =
  Lang.add_builtin "not" ~category:`Bool
    ~descr:"Returns the negation of its argument."
    [("", Lang.bool_t, None, None)]
    Lang.bool_t
    (fun p -> Lang.bool (not (Lang.to_bool (List.assoc "" p))))
