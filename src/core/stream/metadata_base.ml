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
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 *****************************************************************************)

include Liquidsoap_lang.Methods

type bigarray =
  (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

type value = [ `String of string | `Float of float | `Bigarray of bigarray ]
type t = (string, value) Liquidsoap_lang.Methods.t

let string_of_value = function
  | `String s -> s
  | `Float f -> string_of_float f
  | `Bigarray ba -> Liquidsoap_lang.Term_base.string_of_bigarray ba

let float_of_value = function
  | `Float f -> f
  | v -> float_of_string (string_of_value v)

let to_list m =
  List.sort (fun (k, _) (k', _) -> Stdlib.compare k k') (bindings m)

let to_string_list m =
  List.map (fun (k, v) -> (k, string_of_value v)) (to_list m)

let from_string_list m = from_list (List.map (fun (k, v) -> (k, `String v)) m)

module Export = struct
  type metadata = t
  type t = metadata

  let metadata m =
    let l = Encoder_formats.conf_export_metadata#get in
    fold
      (fun x y m ->
        if List.mem (String.lowercase_ascii x) l then (x, y) :: m else m)
      m []

  let from_metadata m = m
  let to_metadata m = m
  let to_list m = List.map (fun (k, v) -> (k, string_of_value v)) (to_list m)

  let equal m m' =
    let normalize m =
      List.sort (fun (k, _) (k', _) -> Stdlib.compare k k') (to_list m)
    in
    normalize m = normalize m'

  let empty : t = from_list []
  let is_empty m = to_list m = []
end
