(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2019 Savonet team

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

open Lang_builtins

let compare_value a b =
  let rec aux = function
    | Lang.Float a, Lang.Float b ->
        compare a b
    | Lang.Int a, Lang.Int b ->
        compare a b
    | Lang.String a, Lang.String b ->
        compare a b
    | Lang.Bool a, Lang.Bool b ->
        compare a b
    | Lang.Tuple l, Lang.Tuple m ->
        List.fold_left2
          (fun cmp a b ->
            if cmp <> 0 then cmp else aux (a.Lang.value, b.Lang.value))
          0 l m
    | Lang.List l1, Lang.List l2 ->
        let rec cmp = function
          | [], [] ->
              0
          | [], _ ->
              -1
          | _, [] ->
              1
          | h1 :: l1, h2 :: l2 ->
              let c = aux (h1.Lang.value, h2.Lang.value) in
              if c = 0 then cmp (l1, l2) else c
        in
        cmp (l1, l2)
    | _ ->
        assert false
  in
  aux (a.Lang.value, b.Lang.value)

let () =
  let t = Lang.univ_t ~constraints:[Lang_types.Ord] () in
  let register_op name op =
    add_builtin name ~cat:Bool ~descr:"Comparison of comparable values."
      [("", t, None, None); ("", t, None, None)] Lang.bool_t (function
      | [("", a); ("", b)] ->
          Lang.bool (op (compare_value a b))
      | _ ->
          assert false)
  in
  register_op "==" (fun c -> c = 0) ;
  register_op "!=" (fun c -> c <> 0) ;
  register_op "<" (fun c -> c = -1) ;
  register_op "<=" (fun c -> c <> 1) ;
  register_op ">=" (fun c -> c <> -1) ;
  register_op ">" (fun c -> c = 1)

let () =
  add_builtin "and" ~cat:Bool ~descr:"Return the conjunction of its arguments"
    [("", Lang.bool_t, None, None); ("", Lang.bool_t, None, None)] Lang.bool_t
    (fun p ->
      match List.map (fun (_, x) -> Lang.to_bool x) p with
        | [a; b] ->
            Lang.bool (a && b)
        | _ ->
            assert false) ;
  add_builtin "or" ~cat:Bool ~descr:"Return the disjunction of its arguments"
    [("", Lang.bool_t, None, None); ("", Lang.bool_t, None, None)] Lang.bool_t
    (fun p ->
      match List.map (fun (_, x) -> Lang.to_bool x) p with
        | [a; b] ->
            Lang.bool (a || b)
        | _ ->
            assert false)

let () =
  add_builtin "not" ~cat:Bool ~descr:"Returns the negation of its argument."
    [("", Lang.bool_t, None, None)] Lang.bool_t (fun p ->
      Lang.bool (not (Lang.to_bool (List.assoc "" p))))
