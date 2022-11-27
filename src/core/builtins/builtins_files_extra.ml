let file = Modules.file

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
        Some (Lang.int 0o644),
        Some "Default file rights if created. Default: `0o644`" );
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
          to_value (Http.unix_socket (Unix.openfile path flags file_perms)))
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
      let f () = ignore (Lang.apply f []) in
      let unwatch = File_watcher.watch ~pos:(Lang.pos p) [`Modify] fname f in
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
  Lifecycle.before_script_parse (fun () ->
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
                 let m = try decoder uri with _ -> [] in
                 let m =
                   List.map (fun (k, v) -> (String.lowercase_ascii k, v)) m
                 in
                 Lang.metadata (Frame.metadata_of_list m)))))

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
