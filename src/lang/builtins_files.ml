(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2021 Savonet team

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

open Builtin
open Extralib

let log = Log.make ["lang.file"]
let () = Lang.add_module "file"

let () =
  add_builtin "file.extension" ~cat:Sys ~descr:"Returns a file's extension."
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
  add_builtin "file.remove" ~cat:Sys ~descr:"Remove a file."
    [("", Lang.string_t, None, None)] Lang.unit_t (fun p ->
      try
        Unix.unlink (Lang.to_string (List.assoc "" p));
        Lang.unit
      with _ -> Lang.unit)

let () =
  add_builtin "file.size" ~cat:Sys ~descr:"File size in bytes."
    [("", Lang.string_t, None, None)] Lang.int_t (fun p ->
      try
        let ic = open_in_bin (Lang.to_string (List.assoc "" p)) in
        let ret = in_channel_length ic in
        close_in ic;
        Lang.int ret
      with _ -> Lang.int 0)

let () =
  add_builtin "file.mkdir" ~cat:Sys ~descr:"Create a directory."
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
  add_builtin "file.rmdir" ~cat:Sys ~descr:"Remove a directory and its content."
    [("", Lang.string_t, None, None)] Lang.unit_t (fun p ->
      try
        Extralib.Unix.rm_dir (Lang.to_string (List.assoc "" p));
        Lang.unit
      with _ -> Lang.unit)

let () =
  add_builtin "file.temp" ~cat:Sys
    ~descr:
      "Return a fresh temporary filename. The temporary file is created empty, \
       with permissions 0o600 (readable and writable only by the file owner)."
    [
      ("", Lang.string_t, None, Some "File prefix");
      ("", Lang.string_t, None, Some "File suffix");
    ] Lang.string_t (fun p ->
      Lang.string
        (Filename.temp_file
           (Lang.to_string (Lang.assoc "" 1 p))
           (Lang.to_string (Lang.assoc "" 2 p))))

let () =
  add_builtin "file.temp_dir" ~cat:Sys
    ~descr:
      "Return a fresh temporary directory name. The temporary directory is \
       created empty, with permissions 0o700 (readable, writable and listable \
       only by the file owner)."
    [
      ("", Lang.string_t, None, Some "Directory prefix");
      ("", Lang.string_t, None, Some "Directory suffix");
    ] Lang.string_t (fun p ->
      Lang.string
        (Extralib.Filename.mk_temp_dir
           (Lang.to_string (Lang.assoc "" 1 p))
           (Lang.to_string (Lang.assoc "" 2 p))))

let () =
  add_builtin "file.exists" ~cat:Sys [("", Lang.string_t, None, None)]
    Lang.bool_t ~descr:"Returns true if the file or directory exists." (fun p ->
      let f = Lang.to_string (List.assoc "" p) in
      let f = Utils.home_unrelate f in
      Lang.bool (Sys.file_exists f))

let () =
  add_builtin "file.is_directory" ~cat:Sys [("", Lang.string_t, None, None)]
    Lang.bool_t ~descr:"Returns true if the file exists and is a directory."
    (fun p ->
      let f = Lang.to_string (List.assoc "" p) in
      let f = Utils.home_unrelate f in
      Lang.bool (try Sys.is_directory f with Sys_error _ -> false))

let () =
  add_builtin "file.read" ~cat:Sys [("", Lang.string_t, None, None)]
    (Lang.fun_t [] Lang.string_t)
    ~descr:
      "Read the content of a file. Returns a function of type `()->string`. \
       File is done reading when function returns the empty string `\"\"`."
    (fun p ->
      let f = Lang.to_string (List.assoc "" p) in
      let mk_fn fn = Lang.val_fun [] (fun _ -> Lang.string (fn ())) in
      try
        let ic = ref (Some (open_in_bin f)) in
        let buflen = Utils.pagesize in
        let buf = Bytes.create buflen in
        let fn () =
          match !ic with
            | Some c ->
                let n = input c buf 0 buflen in
                if n = 0 then (
                  close_in c;
                  ic := None);
                Bytes.sub_string buf 0 n
            | None -> ""
        in
        mk_fn fn
      with e ->
        let message =
          Printf.sprintf "The file %s could not be read: %s" f
            (Printexc.to_string e)
        in
        Lang.error ~message "file")

let () =
  add_builtin "file.write" ~cat:Sys
    [
      ( "data",
        Lang.getter_t (Lang.nullable_t Lang.string_t),
        None,
        Some
          "Data to write. If passing a callback `() -> string?`, the callback \
           must return `null` when it has finished sending all its data." );
      ( "append",
        Lang.bool_t,
        Some (Lang.bool false),
        Some "Append data if file exists." );
      ( "perms",
        Lang.int_t,
        Some (Lang.int 0o644),
        Some "Default file rights if created" );
      ("", Lang.string_t, None, Some "Path to write to");
    ]
    Lang.unit_t ~descr:"Write data to a file."
    (fun p ->
      let data =
        let data = List.assoc "data" p in
        match (Lang.demeth data).Lang.value with
          | Lang.Ground (Lang.Ground.String s) ->
              let is_done = ref false in
              fun () ->
                if not !is_done then (
                  is_done := true;
                  Lang.string s)
                else Lang.null
          | _ -> Lang.to_getter data
      in
      let append = Lang.to_bool (List.assoc "append" p) in
      let perms = Lang.to_int (List.assoc "perms" p) in
      let f = Lang.to_string (List.assoc "" p) in
      let flag = if append then Open_append else Open_trunc in
      let flags = [flag; Open_wronly; Open_creat; Open_binary] in
      try
        let oc = open_out_gen flags perms f in
        let rec f () =
          match Lang.to_option (data ()) with
            | None -> ()
            | Some data ->
                output_string oc (Lang.to_string data);
                f ()
        in
        f ();
        close_out oc;
        Lang.unit
      with e ->
        let message =
          Printf.sprintf "The file %s could not be written: %s" f
            (Printexc.to_string e)
        in
        Lang.error ~message "file")

let () =
  add_builtin "file.write.stream" ~cat:Sys
    [
      ( "append",
        Lang.bool_t,
        Some (Lang.bool false),
        Some "Append data if file exists." );
      ( "perms",
        Lang.int_t,
        Some (Lang.int 0o644),
        Some "Default file rights if created" );
      ("", Lang.string_t, None, Some "Path to write to");
    ]
    (Lang.fun_t [(false, "", Lang.nullable_t Lang.string_t)] Lang.unit_t)
    ~descr:
      "Stream data to a file. Returns a callback to write to the file. Execute \
       with `null` to signify the end of the writing operation."
    (fun p ->
      let append = Lang.to_bool (List.assoc "append" p) in
      let perms = Lang.to_int (List.assoc "perms" p) in
      let f = Lang.to_string (List.assoc "" p) in
      let flag = if append then Open_append else Open_trunc in
      let flags = [flag; Open_wronly; Open_creat; Open_binary] in
      let oc =
        try open_out_gen flags perms f
        with e ->
          let message =
            Printf.sprintf "The file %s could not be opened: %s" f
              (Printexc.to_string e)
          in
          Lang.error ~message "file"
      in
      Lang.val_fun [("", "", None)] (fun p ->
          (match Lang.to_option (List.assoc "" p) with
            | None -> close_out oc
            | Some s -> output_string oc (Lang.to_string s));
          Lang.unit))

let () =
  add_builtin "file.watch" ~cat:Sys
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
      let unwatch = watch [`Modify] fname f in
      Lang.meth Lang.unit
        [
          ( "unwatch",
            Lang.val_fun [] (fun _ ->
                unwatch ();
                Lang.unit) );
        ])

let () =
  add_builtin "file.ls" ~cat:Sys
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
                  aux subdir (aux f acc (readdir df)) l)
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
  add_builtin "file.metadata" ~cat:Sys
    [
      ( "",
        Lang.string_t,
        None,
        Some "File from which the metadata should be read." );
    ] Lang.metadata_t ~descr:"Read metadata from a file." (fun p ->
      let uri = Lang.to_string (List.assoc "" p) in
      let r = Request.create uri in
      if Request.resolve ~ctype:None r 30. = Request.Resolved then (
        Request.read_metadata r;
        Lang.metadata (Request.get_all_metadata r))
      else Lang.metadata (Hashtbl.create 0))

(************** Paths ********************)

let () =
  Lang.add_module "path";
  Lang.add_module "path.home"

let () =
  add_builtin "path.home.unrelate" ~cat:Sys [("", Lang.string_t, None, None)]
    Lang.string_t
    ~descr:"Expand path that start with '~' with the current home directory."
    (fun p ->
      let f = Lang.to_string (List.assoc "" p) in
      Lang.string (Utils.home_unrelate f))

let () =
  add_builtin "path.basename" ~cat:Sys [("", Lang.string_t, None, None)]
    Lang.string_t ~descr:"Get the base name of a path." (fun p ->
      let f = Lang.to_string (List.assoc "" p) in
      Lang.string (Filename.basename f))

let () =
  add_builtin "path.dirname" ~cat:Sys [("", Lang.string_t, None, None)]
    Lang.string_t ~descr:"Get the directory name of a path." (fun p ->
      let f = Lang.to_string (List.assoc "" p) in
      Lang.string (Filename.dirname f))

let () =
  add_builtin "path.concat" ~cat:Sys
    [("", Lang.string_t, None, None); ("", Lang.string_t, None, None)]
    Lang.string_t
    ~descr:"Concatenate two paths, using the appropriate directory separator."
    (fun p ->
      let f = Lang.to_string (Lang.assoc "" 1 p) in
      let s = Lang.to_string (Lang.assoc "" 2 p) in
      Lang.string (Filename.concat f s))

let () =
  add_builtin "path.remove_extension" ~cat:Sys [("", Lang.string_t, None, None)]
    Lang.string_t ~descr:"Remove the file extension from a path." (fun p ->
      let f = Lang.to_string (List.assoc "" p) in
      Lang.string (Filename.remove_extension f))

(************** MP3 ********************)

let () = Lang.add_module "file.mp3"

let () =
  add_builtin "file.mp3.metadata" ~cat:Sys
    [
      ( "",
        Lang.string_t,
        None,
        Some "MP3 file of which the metadata should be read." );
    ]
    (Lang.list_t (Lang.product_t Lang.string_t Lang.string_t))
    ~descr:
      "Read the tags from an MP3 file using the builtin functions. Only ID3v2 \
       tags are supported for now."
    (fun p ->
      let f = Lang.to_string (List.assoc "" p) in
      let ic = open_in f in
      let ans =
        try
          let ans = Id3v2.parse (input ic) in
          close_in ic;
          ans
        with _ ->
          close_in ic;
          []
      in
      Lang.list
        (List.map
           (fun (l, v) -> Lang.product (Lang.string l) (Lang.string v))
           ans))

let () =
  add_builtin "file.mp3.parse_apic" ~cat:Sys
    [("", Lang.string_t, None, Some "APIC data.")]
    (Lang.tuple_t [Lang.string_t; Lang.int_t; Lang.string_t; Lang.string_t])
    ~descr:
      "Parse APIC ID3v2 tags (such as those obtained in the APIC tag from \
       `file.mp3.tags`). The returned values are: mime, picture type, \
       description, and picture data."
    (fun p ->
      let apic = Lang.to_string (List.assoc "" p) in
      let apic = Id3v2.parse_apic apic in
      Lang.tuple
        [
          Lang.string apic.Id3v2.mime;
          Lang.int apic.Id3v2.picture_type;
          Lang.string apic.Id3v2.description;
          Lang.string apic.Id3v2.data;
        ])

let () =
  add_builtin "file.which" ~cat:Sys
    ~descr:
      "`file.which(\"progname\")` looks for an executable named \"progname\" \
       using directories from the PATH environment variable and returns \"\" \
       if it could not find one." [("", Lang.string_t, None, None)]
    (Lang.nullable_t Lang.string_t) (fun p ->
      let file = Lang.to_string (List.assoc "" p) in
      try Lang.string (Utils.which ~path:Configure.path file)
      with Not_found -> Lang.null)

let () =
  add_builtin "file.digest" ~cat:Sys
    ~descr:"Return an MD5 digest for the given file."
    [("", Lang.string_t, None, None)] Lang.string_t (fun p ->
      let file = Lang.to_string (List.assoc "" p) in
      if Sys.file_exists file then
        Lang.string (Digest.to_hex (Digest.file file))
      else (
        let message = Printf.sprintf "The file %s does not exist." file in
        Lang.error ~message "file"))
