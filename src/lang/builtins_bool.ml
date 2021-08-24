(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2021 Savonet team

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

let () =
  let t = Lang.univ_t ~constraints:[Type.Ord] () in
  let register_op name op =
    add_builtin name ~cat:Bool ~descr:"Comparison of comparable values."
      [("", t, None, None); ("", t, None, None)] Lang.bool_t (fun p ->
        let a = Lang.assoc "" 1 p in
        let b = Lang.assoc "" 2 p in
        let a' = Lang.demeth a in
        let b' = Lang.demeth b in
        (* For records, we also compare fields. *)
        let ans =
          if a'.Lang.value = Lang.Tuple [] && b'.Lang.value = Lang.Tuple [] then (
            let r a =
              let m, _ = Term.V.split_meths a in
              m
            in
            let a = r a in
            let b = r b in
            (* Keep only common fields: with subtyping it might happen that some fields are ignored. *)
            let a =
              List.filter
                (fun (l, _) -> List.exists (fun (l', _) -> l = l') b)
                a
            in
            let b =
              List.filter
                (fun (l, _) -> List.exists (fun (l', _) -> l = l') a)
                b
            in
            (* TODO: the order is not the expected one on records (for < we want
               one field to be < and the other to be <=). *)
            List.for_all
              (fun (l, v) ->
                let v' = List.assoc l b in
                op (Lang.compare_values v v'))
              a)
          else op (Lang.compare_values a' b')
        in
        Lang.bool ans)
  in
  register_op "==" (fun c -> c = 0);
  register_op "!=" (fun c -> c <> 0);
  register_op "<" (fun c -> c = -1);
  register_op "<=" (fun c -> c <> 1);
  register_op ">=" (fun c -> c <> -1);
  register_op ">" (fun c -> c = 1)

let () =
  add_builtin "and" ~cat:Bool ~descr:"Return the conjunction of its arguments"
    [
      ("", Lang.getter_t Lang.bool_t, None, None);
      ("", Lang.getter_t Lang.bool_t, None, None);
    ]
    Lang.bool_t
    (fun p ->
      match List.map (fun (_, x) -> Lang.to_bool_getter x) p with
        | [a; b] -> Lang.bool (if a () then b () else false)
        | _ -> assert false);
  add_builtin "or" ~cat:Bool ~descr:"Return the disjunction of its arguments"
    [
      ("", Lang.getter_t Lang.bool_t, None, None);
      ("", Lang.getter_t Lang.bool_t, None, None);
    ]
    Lang.bool_t
    (fun p ->
      match List.map (fun (_, x) -> Lang.to_bool_getter x) p with
        | [a; b] -> Lang.bool (if a () then true else b ())
        | _ -> assert false)

let () =
  add_builtin "not" ~cat:Bool ~descr:"Returns the negation of its argument."
    [("", Lang.bool_t, None, None)] Lang.bool_t (fun p ->
      Lang.bool (not (Lang.to_bool (List.assoc "" p))))
