(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2018 Savonet team

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

(* Dynamic loading. *)

let dyn_log = Dtools.Log.make ["dynamic";"loader"]

let dynlink_suffix =
  if Dynlink.is_native then
    ".cmxs"
  else
    ".cma"

let load_plugins_dir d =
  (* We need to allow unsafe modules
   * for plugins to work in liquidsoap.
   * Otherwise, plugins that load C stubs
   * will not be available.. Additionaly,
   * this function does nothing in native mode.. *)
  Dynlink.allow_unsafe_modules true;
  try
    let dir = Unix.opendir d in
    let rec files cur =
      try
        let f = Unix.readdir dir in
        let f = Printf.sprintf "%s/%s" d f in
        if Filename.check_suffix f dynlink_suffix then
           files (f :: cur)
        else
           files cur
      with
        | End_of_file -> cur
    in
    let files = files [] in
    Unix.closedir dir;
    (* Brute force dependency method:
     * Try to load plugins in any order
     * and stop when the list does not shrink.. *)
    let load ~report cur file =
     try
       Dynlink.loadfile file;
       dyn_log#f 2 "Loaded plugin file %s." file;
       cur
      with
        | Dynlink.Error e when report ->
            dyn_log#f 2 "Could not load plugin file %s: %s."
             file (Dynlink.error_message e);
            cur
        | e ->
             if report then
              begin
                dyn_log#f 2 "Unknown error while loading plugin file %s: %s"
                  file (Printexc.to_string e) ;
                cur
              end
             else
              file :: cur
    in
    let rec try_load files =
      let new_files =
        List.fold_left (load ~report:false) [] files
      in
      if List.length new_files = List.length files then
        (* Run a last time to report errors.. *)
        ignore(List.fold_left (load ~report:true) [] files)
      else
        (* List has shrinked, run again.. *)
        try_load new_files
    in
    try_load files
  with
    | _ -> dyn_log#f 2 "Could not load plugins in directory %s." d

type dynload =
  { path : string list;
    (* Files are registered *without*
     * their extension. e.g.: foo.cmxs -> foo *)
    files : string list;
    load : unit -> unit }

let dynlink_list = Hashtbl.create 2

exception Done of string

(* A function to load external libraries (currently lame) *)
let load_dynlinks () =
  let rec check_list f cur =
    match cur with
      | x :: l -> 
         if f x then
           check_list f l
         else false
      | [] -> true
  in
  let load_library name dynload =
    try
     List.iter (fun path ->
      try
       let get_file = 
         (fun f -> Printf.sprintf "%s/%s%s" path f dynlink_suffix)
       in
       if check_list
           (fun file -> Sys.file_exists (get_file file))
           dynload.files then
        begin
         List.iter (fun file ->
                       Dynlink.loadfile (get_file file))
                   dynload.files;
         raise (Done path)
        end
      with
        | Dynlink.Error e ->
            dyn_log#f 3 "Error while loading dynamic %s at %s" name path;
            dyn_log#f 3 "%s" (Dynlink.error_message e)) dynload.path;
     dyn_log#f 3 "Could not find dynamic module for %s." name
    with
      | Done path ->
          dyn_log#f 3 "Loaded dynamic %s from %s" name path;
          dynload.load ()
  in
  Hashtbl.iter load_library dynlink_list
