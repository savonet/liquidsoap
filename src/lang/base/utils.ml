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

(* Resolve a path. *)
let resolve_path ?current_dir path =
  match path with
    | "-" -> "-"
    | path ->
        let path = Lang_string.home_unrelate path in
        let current_dir =
          match current_dir with
            | None -> Sys.getcwd ()
            | Some current_dir -> current_dir
        in
        if Filename.is_relative path then Filename.concat current_dir path
        else path

let readable f =
  try
    let c = open_in f in
    close_in c;
    true
  with _ -> false

let check_readable ?current_dir ~pos path =
  match path with
    | "-" -> "-"
    | path ->
        let resolved_path = resolve_path ?current_dir path in
        let details =
          if path = resolved_path then "Given path: " ^ path
          else "Given path: " ^ path ^ ", resolved path: " ^ resolved_path
        in
        if not (Sys.file_exists resolved_path) then
          Runtime_error.raise ~pos
            ~message:("File not found! " ^ details)
            "not_found";
        if not (readable resolved_path) then
          Runtime_error.raise ~pos
            ~message:("File is not readable!" ^ details)
            "not_found";
        resolved_path

let string_of_float f =
  let s = string_of_float f in
  if s.[String.length s - 1] = '.' then s ^ "0" else s
