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

let of_list l =
  let table = Hashtbl.create (List.length l) in
  List.iter (fun (k, v) -> Hashtbl.replace table k (v :> Avutil.value)) l;
  table

let string_of_value : Avutil.value -> string = function
  | `String s -> Lang_string.quote_string s
  | `Int i -> string_of_int i
  | `Int64 i -> Int64.to_string i
  | `Float f -> Utils.string_of_float f

let unused_message scopes =
  let scopes =
    List.filter_map
      (fun (scope, table) ->
        if Hashtbl.length table = 0 then None
        else (
          let entries =
            List.sort compare
              (Hashtbl.fold
                 (fun k v l ->
                   Printf.sprintf "%s=%s" k (string_of_value v) :: l)
                 table [])
          in
          Some (Printf.sprintf "%s: %s" scope (String.concat ", " entries))))
      scopes
  in
  match scopes with
    | [] -> None
    | l -> Some ("Unrecognized options: " ^ String.concat "; " l)

let add_flag opts ~name flag =
  let flags : Liquidsoap_ffmpeg_formats.Encoder_format.value =
    match List.assoc_opt "flags" opts with
      | Some (`Int i) -> `Int (i lor flag)
      | Some (`Int64 i) -> `Int64 (Int64.logor i (Int64.of_int flag))
      | Some (`String s) -> `String (s ^ "+" ^ name)
      | Some (`Float _) | None -> `Int flag
  in
  Liquidsoap_ffmpeg_formats.Encoder_format.set opts "flags" flags
