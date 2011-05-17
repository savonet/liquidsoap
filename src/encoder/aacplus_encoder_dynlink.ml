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

(** Shared AAC+ encoder *)

let log = Dtools.Log.make ["encoder";"aacplus";"dynlink"]

let path = 
  try 
    [Sys.getenv "AACPLUS_DYN_PATH"] 
  with 
    | Not_found -> 
       List.fold_left 
         (fun l x -> (x ^ "/aacplus") :: l) 
         Configure.findlib_path Configure.findlib_path

open Aacplus_dynlink

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
      if Sys.file_exists (path ^ "/aacplus." ^ file_ext) &&
         Sys.file_exists (path ^ "/aacplus_loader." ^ file_ext) then
       begin
        Dynlink.loadfile (path ^ "/aacplus." ^ file_ext);
        Dynlink.loadfile (path ^ "/aacplus_loader." ^ file_ext);
        raise (Done path)
       end
     with
       | Dynlink.Error e ->
           log#f 4 "Error while loading dynamic aacplus encoder at %s" path;
           log#f 4 "%s" (Dynlink.error_message e)) path;
      log#f 3 "Could not find dynamic module for aacplus encoder."
   with
     | Done path -> log#f 3 "Loaded dynamic aacplus encoder from %s" path
  end;
  match handler.encoder with
    | Some m ->
      let module Aacplus = (val m : Aacplus_dynlink.Aacplus_t) in
      let module Register = Aacplus_encoder.Register(Aacplus) in
      Register.register_encoder "AAC+/libaacplus/dynlink"
    | None   -> ()
