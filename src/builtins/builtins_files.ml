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

open Extralib

let log = Log.make ["lang.file"]
let () = Lang.add_module "file"

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
        (Utils.file_extension ~dir_sep ~leading_dot
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

let () =
  Lang.add_builtin "file.rmdir" ~category:`File
    ~descr:"Remove a directory and its content."
    [("", Lang.string_t, None, None)]
    Lang.unit_t
    (fun p ->
      try
        Extralib.Unix.rm_dir (Lang.to_string (List.assoc "" p));
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
       created empty, with permissions 0o700 (readable, writable and listable \
       only by the file owner)."
    [
      ("", Lang.string_t, None, Some "Directory prefix");
      ("", Lang.string_t, None, Some "Directory suffix");
    ]
    Lang.string_t
    (fun p ->
      try
        Lang.string
          (Extralib.Filename.mk_temp_dir
             (Lang.to_string (Lang.assoc "" 1 p))
             (Lang.to_string (Lang.assoc "" 2 p)))
      with exn ->
        let bt = Printexc.get_raw_backtrace () in
        Lang.raise_as_runtime ~bt ~kind:"file" exn)

let () =
  Lang.add_builtin "file.exists" ~category:`File
    [("", Lang.string_t, None, None)]
    Lang.bool_t ~descr:"Returns true if the file or directory exists."
    (fun p ->
      let f = Lang.to_string (List.assoc "" p) in
      let f = Utils.home_unrelate f in
      Lang.bool (Sys.file_exists f))

let () =
  Lang.add_builtin "file.is_directory" ~category:`File
    [("", Lang.string_t, None, None)]
    Lang.bool_t ~descr:"Returns true if the file exists and is a directory."
    (fun p ->
      let f = Lang.to_string (List.assoc "" p) in
      let f = Utils.home_unrelate f in
      Lang.bool (try Sys.is_directory f with Sys_error _ -> false))

let () =
  Lang.add_builtin "file.open" ~category:`File
    [
      ( "write",
        Lang.bool_t,
        Some (Lang.bool false),
        Some "Open file for writing" );
      ( "create",
        Lang.nullable_t Lang.bool_t,
        Some Lang.null,
        Some
          "Create if nonexistent. Default: `false` in read-only mode, `true` \
           when writing." );
      ( "append",
        Lang.bool_t,
        Some (Lang.bool false),
        Some "Append data if file exists." );
      ( "non_blocking",
        Lang.bool_t,
        Some (Lang.bool false),
        Some "Open in non-blocking mode." );
      ( "perms",
        Lang.int_t,
        Some (Lang.int 0o644),
        Some "Default file rights if created. Default: `0o644`" );
      ("", Lang.string_t, None, None);
    ]
    Builtins_socket.SocketValue.t ~descr:"Open a file."
    (fun p ->
      let write = Lang.to_bool (List.assoc "write" p) in
      let access_flag = if write then Unix.O_RDWR else Unix.O_RDONLY in
      let create = Lang.to_valued_option Lang.to_bool (List.assoc "create" p) in
      let create_flags =
        Option.value
          ~default:(if write then [Unix.O_CREAT] else [])
          (Option.map (fun x -> if x then [Unix.O_CREAT] else []) create)
      in
      let data_flags =
        match (write, Lang.to_bool (List.assoc "append" p)) with
          | true, true -> [Unix.O_APPEND]
          | true, false -> [Unix.O_TRUNC]
          | false, _ -> []
      in
      let non_blocking_flags =
        if Lang.to_bool (List.assoc "non_blocking" p) then [Unix.O_NONBLOCK]
        else []
      in
      let flags =
        [access_flag] @ create_flags @ data_flags @ non_blocking_flags
      in
      let file_perms = Lang.to_int (List.assoc "perms" p) in
      let path = Utils.home_unrelate (Lang.to_string (List.assoc "" p)) in
      try
        Builtins_socket.SocketValue.to_value
          (Unix.openfile path flags file_perms)
      with exn ->
        let bt = Printexc.get_raw_backtrace () in
        Lang.raise_as_runtime ~bt ~kind:"file" exn)

let () =
  Lang.add_builtin "file.watch" ~category:`File
    [
      ("", Lang.string_t, None, Some "File to watch.");
      ("", Lang.fun_t [] Lang.unit_t, None, Some "Handler function.");
    ]
    (Lang.method_t Lang.unit_t
       [
         ( "unwatch",
           ([], Lang.fun_t [] Lang.unit_t),
           "Function to remove the watch on the file." );
       ])
    ~descr:
      "Call a function when a file is modified. Returns unwatch function in \
       `unwatch` method."
    (fun p ->
      let fname = Lang.to_string (List.assoc_nth "" 0 p) in
      let fname = Utils.home_unrelate fname in
      let f = List.assoc_nth "" 1 p in
      let f () = ignore (Lang.apply f []) in
      let watch = !Configure.file_watcher in
      let unwatch = watch ~pos:(Lang.pos p) [`Modify] fname f in
      Lang.meth Lang.unit
        [
          ( "unwatch",
            Lang.val_fun [] (fun _ ->
                unwatch ();
                Lang.unit) );
        ])

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
        |> Option.map (fun s -> Pcre.replace ~pat:"\\." ~templ:"\\." s)
        |> Option.map (fun s -> Pcre.replace ~pat:"\\*" ~templ:".*" s)
        |> Option.map (fun s -> "^" ^ s ^ "$")
      in
      let pattern = Option.value ~default:"" pattern in
      let rex = Pcre.regexp pattern in
      let dir = Lang.to_string (List.assoc "" p) in
      let dir = Utils.home_unrelate dir in
      let readdir dir =
        Array.to_list (Sys.readdir dir)
        |> List.filter (fun s -> Pcre.pmatch ~rex s)
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
      Lang.list files)

let () =
  Lang.add_builtin "file.metadata" ~category:`File
    [
      ( "",
        Lang.string_t,
        None,
        Some "File from which the metadata should be read." );
    ]
    Lang.metadata_t ~descr:"Read metadata from a file."
    (fun p ->
      let uri = Lang.to_string (List.assoc "" p) in
      let r = Request.create uri in
      if Request.resolve ~ctype:None r 30. = Request.Resolved then (
        Request.read_metadata r;
        Lang.metadata (Request.get_all_metadata r))
      else Lang.metadata (Hashtbl.create 0))

let () =
  List.iter
    (fun (name, decoder) ->
      let name = String.lowercase_ascii name in
      Lang.add_builtin ("file.metadata." ^ name) ~category:`File
        [
          ( "",
            Lang.string_t,
            None,
            Some "File from which the metadata should be read." );
        ]
        Lang.metadata_t
        ~descr:("Read metadata from a file using the " ^ name ^ " decoder.")
        (fun p ->
          let uri = Lang.to_string (List.assoc "" p) in
          let m = try decoder uri with _ -> [] in
          let m = List.map (fun (k, v) -> (String.lowercase_ascii k, v)) m in
          Lang.metadata (Frame.metadata_of_list m)))
    Request.mresolvers#get_all

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
      Lang.string (Utils.home_unrelate f))

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

(************** MP3 ********************)

let () =
  Lang.add_builtin "file.which" ~category:`File
    ~descr:
      "`file.which(\"progname\")` looks for an executable named \"progname\" \
       using directories from the PATH environment variable and returns \"\" \
       if it could not find one."
    [("", Lang.string_t, None, None)]
    (Lang.nullable_t Lang.string_t)
    (fun p ->
      let file = Lang.to_string (List.assoc "" p) in
      try Lang.string (Utils.which ~path:Configure.path file)
      with Not_found -> Lang.null)

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
        Lang.raise_error ~pos:(Lang.pos p) ~message "file"))

let () =
  if not Sys.win32 then (
    let umask_m = Mutex.create () in
    let get_umask =
      Tutils.mutexify umask_m (fun () ->
          let umask = Unix.umask 0 in
          ignore (Unix.umask umask);
          umask)
    in
    let set_umask =
      Tutils.mutexify umask_m (fun umask -> ignore (Unix.umask umask))
    in
    let () =
      Lang.add_builtin "file.umask" ~category:`File
        ~descr:"Get the process's file mode creation mask." [] Lang.int_t
        (fun _ -> Lang.int (get_umask ()))
    in
    Lang.add_builtin "file.umask.set" ~category:`File
      ~descr:"Set process's file mode creation mask."
      [("", Lang.int_t, None, None)]
      Lang.unit_t
      (fun p ->
        set_umask (Lang.to_int (List.assoc "" p));
        Lang.unit))
