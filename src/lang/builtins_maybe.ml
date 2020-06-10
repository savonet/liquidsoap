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

let () = Lang.add_module "maybe"

let () =
  add_builtin "maybe.nothing" ~cat:Liq ~descr:"Nothing." []
    (Lang.maybe_t (Lang.univ_t ()))
    (fun _ -> Lang.nothing)

let () =
  let a = Lang.univ_t () in
  let b = Lang.univ_t () in
  add_builtin "maybe.case" ~cat:Liq
    ~descr:"Return a result dending on whether a value is nothing or not."
    [
      ("", Lang.maybe_t a, None, Some "Value to reason by case analysis on.");
      ("", b, None, Some "Value to return in case we have nothing.");
      ( "",
        Lang.fun_t [(false, "", a)] b,
        None,
        Some "Value to return in case we have something." );
    ]
    b
    (fun p ->
      let x = Lang.assoc "" 1 p in
      let d = Lang.assoc "" 2 p in
      let f = Lang.assoc "" 3 p in
      let f = Lang.to_fun f in
      match Lang.to_option x with None -> d | Some x -> f [("", x)])
