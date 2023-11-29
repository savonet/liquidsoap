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

include Liquidsoap_lang.Methods

type t = (string, string) Liquidsoap_lang.Methods.t

let to_list m =
  List.sort (fun (k, _) (k', _) -> Stdlib.compare k k') (bindings m)

module Export = struct
  type metadata = t
  type t = metadata

  let from_metadata ?(cover = true) m =
    let export = Encoder_formats.conf_export_metadata#get in
    let export =
      if cover then export
      else (
        let cover = Encoder_formats.conf_meta_cover#get in
        List.filter (fun m -> not (List.mem m cover)) export)
    in
    fold
      (fun x y m ->
        if List.mem (String.lowercase_ascii x) export then add x y m else m)
      m empty

  let to_metadata m = m
  let to_list m = bindings m

  let equal m m' =
    let normalize m =
      List.sort (fun (k, _) (k', _) -> Stdlib.compare k k') (to_list m)
    in
    normalize m = normalize m'

  let empty : t = from_list []
  let is_empty m = to_list m = []
end
