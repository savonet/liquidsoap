module Http = Liq_http

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

let file = Modules.file

let _ =
  Lang.add_builtin ~base:file "extension" ~category:`File
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

let _ =
  Lang.add_builtin ~base:file "remove" ~category:`File ~descr:"Remove a file."
    [("", Lang.string_t, None, None)]
    Lang.unit_t
    (fun p ->
      try
        Unix.unlink (Lang.to_string (List.assoc "" p));
        Lang.unit
      with _ -> Lang.unit)

let _ =
  Lang.add_builtin ~base:file "size" ~category:`File
    ~descr:"File size in bytes."
    [("", Lang.string_t, None, None)]
    Lang.int_t
    (fun p ->
      try
        let ic = open_in_bin (Lang.to_string (List.assoc "" p)) in
        let ret = in_channel_length ic in
        close_in ic;
        Lang.int ret
      with _ -> Lang.int 0)

let _ =
  Lang.add_builtin ~base:file "mtime" ~category:`File
    ~descr:"Last modification time."
    [("", Lang.string_t, None, None)]
    Lang.float_t
    (fun p ->
      let fname = List.assoc "" p |> Lang.to_string in
      try Lang.float (Unix.stat fname).st_mtime with _ -> Lang.float 0.)

let _ =
  Lang.add_builtin ~base:file "mkdir" ~category:`File
    ~descr:"Create a directory."
    [
      ( "parents",
        Lang.bool_t,
        Some (Lang.bool false),
        Some "Also create parent directories if they do not exist." );
      ( "perms",
        Lang.int_t,
        Some (Lang.octal_int 0o755),
        Some "Default file rights if created." );
      ("", Lang.string_t, None, None);
    ]
    Lang.unit_t
    (fun p ->
      let parents = List.assoc "parents" p |> Lang.to_bool in
      let perms = List.assoc "perms" p |> Lang.to_int in
      let dir = List.assoc "" p |> Lang.to_string in
      try
        let rec recmkdir dir =
          if not (Sys.file_exists dir) then (
            recmkdir (Filename.dirname dir);
            Unix.mkdir dir perms)
        in
        if parents then recmkdir dir else Unix.mkdir dir perms;
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

let _ =
  Lang.add_builtin ~base:file "rmdir" ~category:`File
    ~descr:"Remove a directory and its content."
    [("", Lang.string_t, None, None)]
    Lang.unit_t
    (fun p ->
      try
        rm_dir (Lang.to_string (List.assoc "" p));
        Lang.unit
      with _ -> Lang.unit)

let _ =
  Lang.add_builtin ~base:file "temp" ~category:`File
    ~descr:
      "Return a fresh temporary filename. The temporary file is created empty, \
       with permissions 0o600 (readable and writable only by the file owner)."
    [
      ( "directory",
        Lang.nullable_t Lang.string_t,
        Some Lang.null,
        Some "Directory where to create the file." );
      ("", Lang.string_t, None, Some "File prefix");
      ("", Lang.string_t, None, Some "File suffix");
    ]
    Lang.string_t
    (fun p ->
      let temp_dir =
        Lang.to_valued_option Lang.to_string (List.assoc "directory" p)
      in
      try
        Lang.string
          (Filename.temp_file ?temp_dir
             (Lang.to_string (Lang.assoc "" 1 p))
             (Lang.to_string (Lang.assoc "" 2 p)))
      with exn ->
        let bt = Printexc.get_raw_backtrace () in
        Lang.raise_as_runtime ~bt ~kind:"file" exn)

let _ =
  Lang.add_builtin ~base:file "temp_dir" ~category:`File
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

let _ =
  Lang.add_builtin ~base:file "exists" ~category:`File
    [("", Lang.string_t, None, None)]
    Lang.bool_t ~descr:"Returns true if the file or directory exists."
    (fun p ->
      let f = Lang.to_string (List.assoc "" p) in
      let f = Lang_string.home_unrelate f in
      Lang.bool (Sys.file_exists f))

let _ =
  Lang.add_builtin ~base:file "is_directory" ~category:`File
    [("", Lang.string_t, None, None)]
    Lang.bool_t ~descr:"Returns true if the file exists and is a directory."
    (fun p ->
      let f = Lang.to_string (List.assoc "" p) in
      let f = Lang_string.home_unrelate f in
      Lang.bool (try Sys.is_directory f with Sys_error _ -> false))

let _ =
  Lang.add_builtin ~base:file "ls" ~category:`File
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
            String.concat "\\." (String.split_on_char '.' s))
        |> Option.map (fun s -> String.concat ".*" (String.split_on_char '*' s))
        |> Option.map (fun s -> "^" ^ s ^ "$")
        |> Option.value ~default:""
      in
      let sorted = List.assoc "sorted" p |> Lang.to_bool in
      let rex = Re.Pcre.regexp pattern in
      let dir = Lang.to_string (List.assoc "" p) in
      let dir = Lang_string.home_unrelate dir in
      let readdir dir =
        Array.to_list (Sys.readdir dir)
        |> List.filter (fun s -> Re.Pcre.pmatch ~rex s)
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

let path = Lang.add_module "path"
let path_home = Lang.add_module ~base:path "home"

let _ =
  Lang.add_builtin ~base:path_home "unrelate" ~category:`File
    [("", Lang.string_t, None, None)]
    Lang.string_t
    ~descr:"Expand path that start with '~' with the current home directory."
    (fun p ->
      let f = Lang.to_string (List.assoc "" p) in
      Lang.string (Lang_string.home_unrelate f))

let _ =
  Lang.add_builtin ~base:path "basename" ~category:`File
    [("", Lang.string_t, None, None)]
    Lang.string_t
    ~descr:
      "Get the base name of a path, i.e. the name of the file without the full \
       path. For instance `file.basename(\"/tmp/folder/bla.mp3\")` returns \
       `\"bla.mp3\"`."
    (fun p ->
      let f = Lang.to_string (List.assoc "" p) in
      Lang.string (Filename.basename f))

let _ =
  Lang.add_builtin ~base:path "dirname" ~category:`File
    [("", Lang.string_t, None, None)]
    Lang.string_t ~descr:"Get the directory name of a path."
    (fun p ->
      let f = Lang.to_string (List.assoc "" p) in
      Lang.string (Filename.dirname f))

let _ =
  Lang.add_builtin ~base:path "concat" ~category:`File
    [("", Lang.string_t, None, None); ("", Lang.string_t, None, None)]
    Lang.string_t
    ~descr:"Concatenate two paths, using the appropriate directory separator."
    (fun p ->
      let f = Lang.to_string (Lang.assoc "" 1 p) in
      let s = Lang.to_string (Lang.assoc "" 2 p) in
      Lang.string (Filename.concat f s))

let _ =
  Lang.add_builtin ~base:path "remove_extension" ~category:`File
    [("", Lang.string_t, None, None)]
    Lang.string_t ~descr:"Remove the file extension from a path."
    (fun p ->
      let f = Lang.to_string (List.assoc "" p) in
      Lang.string (Filename.remove_extension f))

let _ =
  Lang.add_builtin ~base:file "digest" ~category:`File
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

let _ =
  Lang.add_builtin ~base:file "open" ~category:`File
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
        Some (Lang.octal_int 0o644),
        Some "Default file rights if created." );
      ("", Lang.string_t, None, None);
    ]
    Builtins_socket.Socket_value.t ~descr:"Open a file."
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
      let path = Lang_string.home_unrelate (Lang.to_string (List.assoc "" p)) in
      try
        Builtins_socket.Socket_value.(
          to_value
            (Http.unix_socket ~pos:(Lang.pos p)
               (Unix.openfile path flags file_perms)))
      with exn ->
        let bt = Printexc.get_raw_backtrace () in
        Lang.raise_as_runtime ~bt ~kind:"file" exn)

let _ =
  Lang.add_builtin ~base:file "watch" ~category:`File
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
      let fname = Lang.to_string (Extralib.List.assoc_nth "" 0 p) in
      let fname = Lang_string.home_unrelate fname in
      let f = Extralib.List.assoc_nth "" 1 p in
      let f () =
        try ignore (Lang.apply f [])
        with exn ->
          let bt = Printexc.get_raw_backtrace () in
          Lang.raise_as_runtime ~bt ~kind:"file" exn
      in
      let unwatch =
        Liq_file_watcher.watch ~pos:(Lang.pos p) [`Modify] fname f
      in
      Lang.meth Lang.unit
        [
          ( "unwatch",
            Lang.val_fun [] (fun _ ->
                unwatch ();
                Lang.unit) );
        ])

let file_metadata =
  Lang.add_builtin ~base:file "metadata" ~category:`File
    [
      ( "",
        Lang.string_t,
        None,
        Some "File from which the metadata should be read." );
      ( "exclude",
        Lang.list_t Lang.string_t,
        Some (Lang.list []),
        Some "Decoders to exclude" );
    ]
    Lang.metadata_t ~descr:"Read metadata from a file."
    (fun p ->
      let uri = Lang.to_string (List.assoc "" p) in
      let excluded =
        List.map Lang.to_string (Lang.to_list (List.assoc "exclude" p))
      in
      Lang.metadata
        (Request.resolve_metadata ~initial_metadata:Frame.Metadata.empty
           ~excluded uri))

let () =
  Lifecycle.on_load ~name:"metadata resolvers registration" (fun () ->
      Plug.iter Request.mresolvers (fun name decoder ->
          let name = String.lowercase_ascii name in
          ignore
            (Lang.add_builtin ~base:file_metadata name ~category:`File
               [
                 ( "",
                   Lang.string_t,
                   None,
                   Some "File from which the metadata should be read." );
               ]
               Lang.metadata_t
               ~descr:
                 ("Read metadata from a file using the " ^ name ^ " decoder.")
               (fun p ->
                 let uri = Lang.to_string (List.assoc "" p) in
                 let extension =
                   try Some (Utils.get_ext uri) with _ -> None
                 in
                 let mime = Magic_mime.lookup uri in
                 let m =
                   try
                     decoder.Request.resolver ~metadata:Frame.Metadata.empty
                       ~extension ~mime uri
                   with _ -> []
                 in
                 let m =
                   List.map (fun (k, v) -> (String.lowercase_ascii k, v)) m
                 in
                 Lang.metadata (Frame.Metadata.from_list m)))))

let _ =
  Lang.add_builtin ~base:file_metadata "native" ~category:`File
    [
      ( "",
        Lang.string_t,
        None,
        Some "File from which the metadata should be read." );
    ]
    Lang.metadata_t ~descr:"Read metadata from a file using the native decoder."
    (fun p ->
      let file = List.assoc "" p |> Lang.to_string in
      let m = try Metadata.parse_file file with _ -> [] in
      let m = List.map (fun (k, v) -> (String.lowercase_ascii k, v)) m in
      Lang.metadata (Frame.Metadata.from_list m))

let _ =
  Lang.add_builtin ~base:file "which" ~category:`File
    ~descr:
      "`file.which(\"progname\")` looks for an executable named \"progname\" \
       using directories from the PATH environment variable and returns \"\" \
       if it could not find one."
    [("", Lang.string_t, None, None)]
    (Lang.nullable_t Lang.string_t)
    (fun p ->
      let file = Lang.to_string (List.assoc "" p) in
      try Lang.string (Utils.which ~path:(Configure.path ()) file)
      with Not_found -> Lang.null)

let _ =
  Lang.add_builtin ~base:file "copy" ~category:`File
    ~descr:
      "Copy a file. Arguments and implementation follows the POSIX `cp` \
       command line specifications."
    [
      ( "recursive",
        Lang.bool_t,
        Some (Lang.bool false),
        Some "Copy file hierarchies." );
      ( "force",
        Lang.bool_t,
        Some (Lang.bool true),
        Some
          "If a file descriptor for a destination file cannot be obtained \
           attempt to unlink the destination file and proceed." );
      ( "preserve",
        Lang.bool_t,
        Some (Lang.bool false),
        Some "Duplicate source files attributes in the destination file." );
      ("", Lang.string_t, None, Some "Source");
      ("", Lang.string_t, None, Some "Destination");
    ]
    Lang.unit_t
    (fun p ->
      let recurse = Lang.to_bool (List.assoc "recursive" p) in
      let force =
        if Lang.to_bool (List.assoc "force" p) then FileUtil.Force
        else FileUtil.Ask (fun _ -> false)
      in
      let preserve = Lang.to_bool (List.assoc "preserve" p) in
      let src = Lang.to_string (Lang.assoc "" 1 p) in
      let dst = Lang.to_string (Lang.assoc "" 2 p) in
      let error message _ =
        Runtime_error.raise ~pos:(Lang.pos p) ~message "file"
      in
      try
        FileUtil.cp ~recurse ~force ~preserve ~error [src] dst;
        Lang.unit
      with exn ->
        let bt = Printexc.get_raw_backtrace () in
        Lang.raise_as_runtime ~bt ~kind:"file" exn)

let _ =
  Lang.add_builtin ~base:file "move" ~category:`File ~descr:"Move a file"
    [
      ( "force",
        Lang.bool_t,
        Some (Lang.bool false),
        Some "Do not prompt for confirmation if the destination path exists." );
      ( "atomic",
        Lang.bool_t,
        Some (Lang.bool false),
        Some
          "Move the file atomically. Implies `force` and raises \
           `error.file.cross_device` if atomic move fails because the source \
           and destination files are not on the same partition." );
      ("", Lang.string_t, None, Some "Source");
      ("", Lang.string_t, None, Some "Destination");
    ]
    Lang.unit_t
    (fun p ->
      let force =
        if Lang.to_bool (List.assoc "force" p) then FileUtil.Force
        else FileUtil.Ask (fun _ -> false)
      in
      let atomic = Lang.to_bool (List.assoc "atomic" p) in
      let src = Lang.to_string (Lang.assoc "" 1 p) in
      let dst = Lang.to_string (Lang.assoc "" 2 p) in
      let error message _ =
        Runtime_error.raise ~pos:(Lang.pos p) ~message "file"
      in
      try
        if atomic then Unix.rename src dst
        else FileUtil.mv ~force ~error src dst;
        Lang.unit
      with
        | Unix.Unix_error (Unix.EXDEV, _, _) ->
            Runtime_error.raise ~pos:(Lang.pos p)
              ~message:
                "Rename failed! Directory for temporary files appears to be on \
                 a different filesystem"
              "file.cross_device"
        | exn ->
            let bt = Printexc.get_raw_backtrace () in
            Lang.raise_as_runtime ~bt ~kind:"file" exn)

let () =
  if not Sys.win32 then (
    let umask_m = Mutex.create () in
    let get_umask =
      Mutex_utils.mutexify umask_m (fun () ->
          let umask = Unix.umask 0 in
          ignore (Unix.umask umask);
          umask)
    in
    let set_umask =
      Mutex_utils.mutexify umask_m (fun umask -> ignore (Unix.umask umask))
    in
    let umask =
      Lang.add_builtin ~base:file "umask" ~category:`File
        ~descr:"Get the process's file mode creation mask." [] Lang.int_t
        (fun _ -> Lang.int (get_umask ()))
    in
    ignore
      (Lang.add_builtin ~base:umask "set" ~category:`File
         ~descr:"Set process's file mode creation mask."
         [("", Lang.int_t, None, None)]
         Lang.unit_t
         (fun p ->
           set_umask (Lang.to_int (List.assoc "" p));
           Lang.unit)))

let _ =
  Lang.add_builtin ~base:Modules.file_mime "magic" ~category:`File
    ~descr:"Get the MIME type of a file."
    [("", Lang.string_t, None, None)]
    Lang.string_t
    (fun p ->
      let file = Lang.to_string (Lang.assoc "" 1 p) in
      Lang.string (Magic_mime.lookup file))
