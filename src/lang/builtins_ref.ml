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
  add_builtin "ref" ~cat:Liq
    ~descr:"Create a reference, i.e. a value which can be modified."
    [("", a, None, None)] (Lang.ref_t a) (fun p ->
      let x = List.assoc "" p in
      Lang.reference (ref x))

let () =
  let a = Lang.univ_t () in
  add_builtin "ref.get" ~cat:Liq ~descr:"Retrieve the contents of a reference."
    [("", Lang.ref_t a, None, None)]
    a
    (fun p ->
      let r = Lang.to_ref (List.assoc "" p) in
      !r)

let () =
  let a = Lang.univ_t () in
  add_builtin "ref.set" ~cat:Liq ~descr:"Set the value of a reference."
    [("", Lang.ref_t a, None, None); ("", a, None, None)]
    Lang.unit_t
    (fun p ->
      let r = Lang.to_ref (Lang.assoc "" 1 p) in
      let v = Lang.assoc "" 2 p in
      r := v;
      Lang.unit)
