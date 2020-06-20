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

let () =
  let a = Lang.univ_t () in
  add_builtin "null" ~cat:Liq ~descr:"Create a nullable value."
    [("", Lang.nullable_t a, Some Lang.null, Some "Value to make nullable.")]
    (Lang.nullable_t a)
    (fun p ->
      match Lang.to_option (List.assoc "" p) with
        | Some x -> x
        | None -> Lang.null)

let () =
  let a = Lang.univ_t ~constraints:[Lang_types.Non_null] () in
  add_builtin "null.valued" ~cat:Liq ~descr:"Create a not-nullable value."
    [("", a, None, Some "Value to make not-nullable.")]
    a (List.assoc "")

let () =
  let a = Lang.univ_t () in
  let b = Lang.univ_t () in
  add_builtin "null.case" ~cat:Liq
    ~descr:"Return a result dending on whether a value is nothing or not."
    [
      ("", Lang.nullable_t a, None, Some "Value to reason by case analysis on.");
      ( "",
        Lang.fun_t [] b,
        None,
        Some "Value to return in case we have nothing." );
      ( "",
        Lang.fun_t [(false, "", a)] b,
        None,
        Some "Value to return in case we have something." );
    ]
    b
    (fun p ->
      let x = Lang.assoc "" 1 p in
      let d = Lang.to_fun (Lang.assoc "" 2 p) in
      let f = Lang.to_fun (Lang.assoc "" 3 p) in
      match Lang.to_option x with None -> d [] | Some x -> f [("", x)])

let () =
  let a = Lang.univ_t () in
  add_builtin "null.default" ~cat:Liq
    ~descr:"Return a result dending on whether a value is nothing or not."
    [
      ("", Lang.nullable_t a, None, Some "Value to reason by case analysis on.");
      ( "",
        Lang.fun_t [] a,
        None,
        Some "Value to return in case we have nothing." );
    ]
    a
    (fun p ->
      let x = Lang.assoc "" 1 p in
      let d = Lang.assoc "" 2 p in
      match Lang.to_option x with None -> Lang.apply d [] | Some x -> x)
