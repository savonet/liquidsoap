(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2022 Savonet team

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

module Filename = struct
  include Filename

  let rand_digits () =
    let rand = Random.State.(bits (make_self_init ()) land 0xFFFFFF) in
    Printf.sprintf "%06x" rand

  let mk_temp_dir ?(mode = 0o700) ?dir prefix suffix =
    let dir = match dir with Some d -> d | None -> get_temp_dir_name () in
    let raise_err msg = raise (Sys_error msg) in
    let rec loop count =
      if count < 0 then raise_err "mk_temp_dir: too many failing attempts"
      else (
        let dir =
          Printf.sprintf "%s/%s%s%s" dir prefix (rand_digits ()) suffix
        in
        try
          Unix.mkdir dir mode;
          dir
        with
          | Unix.Unix_error (Unix.EEXIST, _, _) -> loop (count - 1)
          | Unix.Unix_error (Unix.EINTR, _, _) -> loop count
          | Unix.Unix_error (e, _, _) ->
              raise_err ("mk_temp_dir: " ^ Unix.error_message e))
    in
    loop 1000
end

(* From OCaml *)
let file_extension_len ~dir_sep name =
  let rec check i0 i =
    if i < 0 || name.[i] = dir_sep then 0
    else if name.[i] = '.' then check i0 (i - 1)
    else String.length name - i0
  in
  let rec search_dot i =
    if i < 0 || name.[i] = dir_sep then 0
    else if name.[i] = '.' then check i (i - 1)
    else search_dot (i - 1)
  in
  search_dot (String.length name - 1)

let file_extension ?(leading_dot = true) ?(dir_sep = Filename.dir_sep) name =
  let dir_sep = dir_sep.[0] in
  let l = file_extension_len ~dir_sep name in
  let s = if l = 0 then "" else String.sub name (String.length name - l) l in
  try
    match (leading_dot, s.[0]) with
      | false, '.' -> String.sub s 1 (String.length s - 1)
      | _ -> s
  with Invalid_argument _ -> s

let () =
  Lang.add_builtin "file.extension" ~category:`File
    ~descr:"Returns a file's extension."
    [
      ( "dir_sep",
        Lang.string_t,
        Some (Lang.string Filename.dir_sep),
        Some "Directory separator." );
      ( "leading_dot",
        Lang.bool_t,
        Some (Lang.bool true),
        Some "Return extension with a leading dot, e.g. `.foo`." );
      ("", Lang.string_t, None, None);
    ]
    Lang.string_t
    (fun p ->
      let dir_sep = Lang.to_string (List.assoc "dir_sep" p) in
      let leading_dot = Lang.to_bool (List.assoc "leading_dot" p) in
      Lang.string
        (file_extension ~dir_sep ~leading_dot
           (Lang.to_string (List.assoc "" p))))

let () =
  Lang.add_builtin "file.remove" ~category:`File ~descr:"Remove a file."
    [("", Lang.string_t, None, None)]
    Lang.unit_t
    (fun p ->
      try
        Unix.unlink (Lang.to_string (List.assoc "" p));
        Lang.unit
      with _ -> Lang.unit)

let () =
  Lang.add_builtin "file.size" ~category:`File ~descr:"File size in bytes."
    [("", Lang.string_t, None, None)]
    Lang.int_t
    (fun p ->
      try
        let ic = open_in_bin (Lang.to_string (List.assoc "" p)) in
        let ret = in_channel_length ic in
        close_in ic;
        Lang.int ret
      with _ -> Lang.int 0)

let () =
  Lang.add_builtin "file.mkdir" ~category:`File ~descr:"Create a directory."
    [
      ( "perms",
        Lang.int_t,
        Some (Lang.int 0o755),
        Some "Default file rights if created (default is `0o755`)." );
      ("", Lang.string_t, None, None);
    ]
    Lang.unit_t
    (fun p ->
      let perms = List.assoc "perms" p |> Lang.to_int in
      let dir = List.assoc "" p |> Lang.to_string in
      try
        Unix.mkdir dir perms;
        Lang.unit
      with _ -> Lang.unit)

let rm_dir dir =
  let rec finddepth f roots =
    Array.iter
      (fun root ->
        (match Unix.lstat root with
          | { Unix.st_kind = S_DIR } ->
              finddepth f (Array.map (Filename.concat root) (Sys.readdir root))
          | _ -> ());
        f root)
      roots
  in
  let zap path =
    match Unix.lstat path with
      | { st_kind = S_DIR } -> Unix.rmdir path
      | _ -> Unix.unlink path
  in
  finddepth zap [| dir |];
  Unix.rmdir dir

let () =
  Lang.add_builtin "file.rmdir" ~category:`File
    ~descr:"Remove a directory and its content."
    [("", Lang.string_t, None, None)]
    Lang.unit_t
    (fun p ->
      try
        rm_dir (Lang.to_string (List.assoc "" p));
        Lang.unit
      with _ -> Lang.unit)

let () =
  Lang.add_builtin "file.temp" ~category:`File
    ~descr:
      "Return a fresh temporary filename. The temporary file is created empty, \
       with permissions 0o600 (readable and writable only by the file owner)."
    [
      ("", Lang.string_t, None, Some "File prefix");
      ("", Lang.string_t, None, Some "File suffix");
    ]
    Lang.string_t
    (fun p ->
      try
        Lang.string
          (Filename.temp_file
             (Lang.to_string (Lang.assoc "" 1 p))
             (Lang.to_string (Lang.assoc "" 2 p)))
      with exn ->
        let bt = Printexc.get_raw_backtrace () in
        Lang.raise_as_runtime ~bt ~kind:"file" exn)

let () =
  Lang.add_builtin "file.temp_dir" ~category:`File
    ~descr:
      "Return a fresh temporary directory name. The temporary directory is \
       created empty, in the default tmp directory, with permissions 0o700 \
       (readable, writable and listable only by the file owner)."
    [
      ("", Lang.string_t, None, Some "Directory name prefix.");
      ("", Lang.string_t, Some (Lang.string ""), Some "Directory name suffix.");
    ]
    Lang.string_t
    (fun p ->
      try
        let prefix = Lang.to_string (Lang.assoc "" 1 p) in
        let suffix = Lang.to_string (Lang.assoc "" 2 p) in
        Lang.string (Filename.mk_temp_dir prefix suffix)
      with exn ->
        let bt = Printexc.get_raw_backtrace () in
        Lang.raise_as_runtime ~bt ~kind:"file" exn)

let () =
  Lang.add_builtin "file.exists" ~category:`File
    [("", Lang.string_t, None, None)]
    Lang.bool_t ~descr:"Returns true if the file or directory exists."
    (fun p ->
      let f = Lang.to_string (List.assoc "" p) in
      let f = Lang_string.home_unrelate f in
      Lang.bool (Sys.file_exists f))

let () =
  Lang.add_builtin "file.is_directory" ~category:`File
    [("", Lang.string_t, None, None)]
    Lang.bool_t ~descr:"Returns true if the file exists and is a directory."
    (fun p ->
      let f = Lang.to_string (List.assoc "" p) in
      let f = Lang_string.home_unrelate f in
      Lang.bool (try Sys.is_directory f with Sys_error _ -> false))

let () =
  Lang.add_builtin "file.ls" ~category:`File
    [
      ( "absolute",
        Lang.bool_t,
        Some (Lang.bool false),
        Some "Whether to return absolute paths." );
      ( "recursive",
        Lang.bool_t,
        Some (Lang.bool false),
        Some "Whether to look recursively in subdirectories." );
      ( "pattern",
        Lang.nullable_t Lang.string_t,
        Some Lang.null,
        Some "Pattern that the filenames should match (e.g. `\"*.mp3\"`)." );
      ( "sorted",
        Lang.bool_t,
        Some (Lang.bool false),
        Some "Return results in a sorted order." );
      ("", Lang.string_t, None, Some "Directory to look in.");
    ]
    (Lang.list_t Lang.string_t)
    ~descr:"List all the files in a directory."
    (fun p ->
      let absolute = Lang.to_bool (List.assoc "absolute" p) in
      let recursive = Lang.to_bool (List.assoc "recursive" p) in
      let pattern =
        List.assoc "pattern" p |> Lang.to_option |> Option.map Lang.to_string
      in
      let pattern =
        pattern
        |> Option.map (fun s ->
               Regexp.substitute (Regexp.regexp "\\.") ~subst:(fun _ -> "\\.") s)
        |> Option.map (fun s ->
               Regexp.substitute (Regexp.regexp "\\*") ~subst:(fun _ -> ".*") s)
        |> Option.map (fun s -> "^" ^ s ^ "$")
        |> Option.value ~default:""
      in
      let sorted = List.assoc "sorted" p |> Lang.to_bool in
      let rex = Regexp.regexp pattern in
      let dir = Lang.to_string (List.assoc "" p) in
      let dir = Lang_string.home_unrelate dir in
      let readdir dir =
        Array.to_list (Sys.readdir dir)
        |> List.filter (fun s -> Regexp.test rex s)
      in
      let files =
        if not recursive then readdir dir
        else (
          let rec aux subdir acc = function
            | f :: l ->
                let concat d f =
                  if d = Filename.current_dir_name then f
                  else Filename.concat d f
                in
                let df = concat subdir f in
                let df = Filename.concat dir df in
                if try Sys.is_directory df with _ -> false then (
                  let f = concat subdir f in
                  let acc =
                    if f <> Filename.current_dir_name then f :: acc else acc
                  in
                  let in_dir =
                    (* Cope with permission problems. *)
                    try readdir df with Sys_error _ -> []
                  in
                  let acc = aux f acc in_dir in
                  aux subdir acc l)
                else aux subdir (concat subdir f :: acc) l
            | [] -> acc
          in
          aux Filename.current_dir_name [] [Filename.current_dir_name])
      in
      let files =
        if absolute then List.map (Filename.concat dir) files else files
      in
      let files = List.map Lang.string files in
      let files = if sorted then List.sort compare files else files in
      Lang.list files)

(************** Paths ********************)

let () =
  Lang.add_module "path";
  Lang.add_module "path.home"

let () =
  Lang.add_builtin "path.home.unrelate" ~category:`File
    [("", Lang.string_t, None, None)]
    Lang.string_t
    ~descr:"Expand path that start with '~' with the current home directory."
    (fun p ->
      let f = Lang.to_string (List.assoc "" p) in
      Lang.string (Lang_string.home_unrelate f))

let () =
  Lang.add_builtin "path.basename" ~category:`File
    [("", Lang.string_t, None, None)]
    Lang.string_t ~descr:"Get the base name of a path."
    (fun p ->
      let f = Lang.to_string (List.assoc "" p) in
      Lang.string (Filename.basename f))

let () =
  Lang.add_builtin "path.dirname" ~category:`File
    [("", Lang.string_t, None, None)]
    Lang.string_t ~descr:"Get the directory name of a path."
    (fun p ->
      let f = Lang.to_string (List.assoc "" p) in
      Lang.string (Filename.dirname f))

let () =
  Lang.add_builtin "path.concat" ~category:`File
    [("", Lang.string_t, None, None); ("", Lang.string_t, None, None)]
    Lang.string_t
    ~descr:"Concatenate two paths, using the appropriate directory separator."
    (fun p ->
      let f = Lang.to_string (Lang.assoc "" 1 p) in
      let s = Lang.to_string (Lang.assoc "" 2 p) in
      Lang.string (Filename.concat f s))

let () =
  Lang.add_builtin "path.remove_extension" ~category:`File
    [("", Lang.string_t, None, None)]
    Lang.string_t ~descr:"Remove the file extension from a path."
    (fun p ->
      let f = Lang.to_string (List.assoc "" p) in
      Lang.string (Filename.remove_extension f))

let () =
  Lang.add_builtin "file.digest" ~category:`File
    ~descr:"Return an MD5 digest for the given file."
    [("", Lang.string_t, None, None)]
    Lang.string_t
    (fun p ->
      let file = Lang.to_string (List.assoc "" p) in
      if Sys.file_exists file then
        Lang.string (Digest.to_hex (Digest.file file))
      else (
        let message = Printf.sprintf "The file %s does not exist." file in
        Lang.raise_error ~message "file"))
