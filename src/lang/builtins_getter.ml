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
  let a = Lang.univ_t () in
  add_builtin ~cat:Liq "getter" ~descr:"Create a getter."
    [
      ( "",
        Lang.getter_t a,
        None,
        Some "Value from which the getter should be created." );
    ]
    (Lang.getter_t a)
    (fun p -> List.assoc "" p)

let () =
  let a = Lang.univ_t () in
  let b = Lang.univ_t () in
  add_builtin ~cat:Liq "getter.case"
    ~descr:"Return a value depending on whether the getter is constant or not."
    [
      ("", Lang.getter_t a, None, Some "Getter to inspect.");
      ("", Lang.fun_t [(false, "", a)] b, None, None);
      ("", Lang.fun_t [(false, "", Lang.fun_t [] a)] b, None, None);
    ]
    b
    (fun p ->
      let x = Lang.assoc "" 1 p in
      let f = Lang.assoc "" 2 p in
      let g = Lang.assoc "" 3 p in
      match (Lang.demeth x).Lang.value with
        | Lang.Fun ([], _, _, _) -> Lang.apply g [("", x)]
        | _ -> Lang.apply f [("", x)])

(* TODO: this could be implemented in Liq with getter.case *)
let () =
  let a = Lang.univ_t () in
  add_builtin ~cat:Liq "getter.get" ~descr:"Get the value of a getter."
    [("", Lang.getter_t a, None, None)]
    a
    (fun p ->
      let x = List.assoc "" p |> Lang.to_getter in
      x ())
