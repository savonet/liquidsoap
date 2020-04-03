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

(** Dynamic Lame encoder *)

let path =
  try [Sys.getenv "LAME_DYN_PATH"]
  with Not_found ->
    List.fold_left
      (fun l x -> (x ^ "/lame") :: l)
      Configure.findlib_path Configure.findlib_path

open Lame_dynlink

let () =
  let load () =
    match handler.lame_module with
      | Some m ->
          let module Lame = (val m : Lame_dynlink.Lame_t) in
          let module Register = Lame_encoder.Register (Lame) in
          Register.register_encoder "MP3/liblame/dynlink"
      | None -> assert false
  in
  Hashtbl.add Dyntools.dynlink_list "lame encoder"
    { Dyntools.path; files = ["lame"; "lame_loader"]; load }
