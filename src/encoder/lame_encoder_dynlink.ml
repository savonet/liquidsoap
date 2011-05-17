(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2010 Savonet team

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

(** Dynamic Lame encoder *)

let log = Dtools.Log.make ["encoder";"lame";"dynlink"]

let path =
  try
    [Sys.getenv "LAME_DYN_PATH"]
  with
    | Not_found -> 
       List.fold_left 
         (fun l x -> (x ^ "/lame") :: l)
         Configure.findlib_path Configure.findlib_path

open Lame_dynlink

exception Done of string

let () =
  let file_ext = 
    if Dynlink.is_native then
      "cmxs"
    else
      "cma"
  in
  begin
   try 
    List.iter (fun path ->
     try
      if Sys.file_exists (path ^ "/lame." ^ file_ext) &&
         Sys.file_exists (path ^ "/lame_loader." ^ file_ext) then
       begin
        Dynlink.loadfile (path ^ "/lame." ^ file_ext);
        Dynlink.loadfile (path ^ "/lame_loader." ^ file_ext);
        raise (Done path)
       end
     with
       | Dynlink.Error e ->
           log#f 3 "Error while loading dynamic lame encoder at %s" path;
           log#f 4 "%s" (Dynlink.error_message e)) path;
    log#f 3 "Could not find dynamic module for lame encoder."
   with 
     | Done path -> log#f 3 "Loaded dynamic lame encoder from %s" path
  end;
  match handler.lame_module with
    | Some m ->
        let module Lame = (val m : Lame_dynlink.Lame_t) in
        let module Register = Lame_encoder.Register(Lame) in
        Register.register_encoder "MP3/liblame/dynlink"
    | None   -> ()
