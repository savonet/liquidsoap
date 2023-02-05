(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2023 Savonet team

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

let ref_t a =
  Lang.record_t
    [("get", Lang.fun_t [] a); ("set", Lang.fun_t [(false, "", a)] Lang.unit_t)]

let ref =
  let a = Lang.univ_t () in
  Lang.add_builtin "ref" ~category:`Programming
    ~descr:"Create a reference, i.e. a value which can be modified."
    [("", a, None, None)]
    (ref_t a)
    (fun p ->
      let x = List.assoc "" p |> Atomic.make in
      Lang.record
        [
          ("get", Lang.val_fun [] (fun _ -> Atomic.get x));
          ( "set",
            Lang.val_fun [] (fun p ->
                List.assoc "" p |> Atomic.set x;
                Lang.unit) );
        ])
