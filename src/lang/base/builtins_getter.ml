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

let getter =
  let a = Lang.univ_t () in
  Lang.add_builtin ~category:`Getter "getter" ~descr:"Create a getter."
    [
      ( "",
        Lang.getter_t a,
        None,
        Some "Value from which the getter should be created." );
    ]
    (Lang.getter_t a)
    (fun p -> List.assoc "" p)

let _ =
  let a = Lang.univ_t () in
  let b = Lang.univ_t () in
  Lang.add_builtin ~category:`Getter ~base:getter "case"
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
      match x with
        | Fun { fun_args = [] } | FFI { ffi_args = []; _ } ->
            Lang.apply ~pos:(Lang.pos p) g [("", x)]
        | _ -> Lang.apply ~pos:(Lang.pos p) f [("", x)])

let _ =
  let a = Lang.univ_t () in
  Lang.add_builtin ~category:`Getter ~base:getter "get"
    ~descr:"Get the value of a getter."
    [("", Lang.getter_t a, None, None)]
    a
    (fun p ->
      let x = List.assoc "" p |> Lang.to_getter in
      x ())

let getter_map =
  let a = Lang.univ_t () in
  let b = Lang.univ_t () in
  Lang.add_builtin ~category:`Getter ~base:getter "map"
    ~descr:"Apply a function on a getter."
    [
      ("", Lang.fun_t [(false, "", a)] b, None, Some "Function to apply.");
      ("", Lang.getter_t a, None, None);
    ]
    (Lang.getter_t b)
    (fun p ->
      let f = Lang.assoc "" 1 p in
      let x = Lang.assoc "" 2 p in
      match x with
        | Fun { fun_args = [] } | FFI { ffi_args = []; _ } ->
            Lang.val_fun [] (fun p' ->
                Lang.apply ~pos:(Lang.pos p') f
                  [("", Lang.apply ~pos:(Lang.pos p') x [])])
        | _ -> Lang.apply ~pos:(Lang.pos p) f [("", x)])

let _ =
  let a = Lang.univ_t ~constraints:[Type.ord_constr] () in
  let b = Lang.univ_t () in
  Lang.add_builtin ~category:`Getter ~base:getter_map "memoize"
    ~descr:
      "Apply a function on a getter. If the input value has not changed \
       compared to last call, the previous result is returned without \
       computing the function again."
    [
      ("", Lang.fun_t [(false, "", a)] b, None, Some "Function to apply.");
      ("", Lang.getter_t a, None, None);
    ]
    (Lang.getter_t b)
    (fun p ->
      let f = Lang.assoc "" 1 p in
      let x = Lang.assoc "" 2 p in
      match x with
        | Fun { fun_args = [] } | FFI { ffi_args = []; _ } ->
            let last_x = ref (Lang.apply ~pos:(Lang.pos p) x []) in
            let last_y = ref (Lang.apply ~pos:(Lang.pos p) f [("", !last_x)]) in
            Lang.val_fun [] (fun p' ->
                let x = Lang.apply ~pos:(Lang.pos p') x [] in
                if Value.compare x !last_x = 0 then !last_y
                else (
                  let y = Lang.apply ~pos:(Lang.pos p') f [("", x)] in
                  last_x := x;
                  last_y := y;
                  y))
        | _ -> Lang.apply ~pos:(Lang.pos p) f [("", x)])
