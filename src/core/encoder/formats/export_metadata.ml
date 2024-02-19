(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2024 Savonet team

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

type metadata = Frame.metadata

let metadata ?(cover = true) m =
  let ret = Hashtbl.create 10 in
  let export = Encoder_formats.conf_export_metadata#get in
  let export =
    if cover then export
    else (
      let cover = Encoder_formats.conf_meta_cover#get in
      List.filter (fun m -> not (List.mem m cover)) export)
  in
  Hashtbl.iter
    (fun x y ->
      if List.mem (String.lowercase_ascii x) export then Hashtbl.add ret x y)
    m;
  ret

let to_metadata m = m
let to_list m = Utils.list_of_metadata m

let equal m m' =
  let normalize m =
    List.sort (fun (k, _) (k', _) -> Stdlib.compare k k') (to_list m)
  in
  normalize m = normalize m'

let empty_metadata = Hashtbl.create 0
let is_empty m = Hashtbl.length m == 0

let to_string m =
  "{ "
  ^ Hashtbl.fold
      (fun k v s ->
        Printf.sprintf "%s\"%s\": \"%s\"" (if s = "" then "" else s ^ " , ") k v)
      m ""
  ^ " }"
