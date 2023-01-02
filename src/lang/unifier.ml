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

(* Simple unification module for variables with no unknown value *)

type 'a t = [ `Value of 'a | `Link of 'a t ref ]

let make v = `Link (ref (`Value v))
let rec deref x = match x with `Value v -> v | `Link x -> deref !x

let set x v =
  let rec f x = match !x with `Value _ -> x := `Value v | `Link x -> f x in
  match x with `Value _ -> assert false | `Link x -> f x

let ( <-- ) x x' =
  let rec f x x' =
    match (!x, !x') with
      | `Link x, _ -> f x x'
      | _, `Link x' -> f x x'
      | _ when x != x' -> x := `Link x'
      | _ -> ()
  in
  match (x, x') with
    | `Value _, _ | _, `Value _ -> assert false
    | `Link x, `Link x' -> f x x'
