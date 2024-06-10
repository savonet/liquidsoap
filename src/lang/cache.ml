let enabled () =
  try
    let venv = Unix.getenv "LIQ_CACHE" in
    venv = "1" || venv = "true"
  with Not_found -> true

let default_dir =
  ref (fun () ->
      try
        match Sys.os_type with
          | "Win32" ->
              let dir = Filename.dirname Sys.executable_name in
              let cwd = Sys.getcwd () in
              Sys.chdir dir;
              let dir = Sys.getcwd () in
              Sys.chdir cwd;
              Some (Filename.concat dir ".cache")
          | _ ->
              Some
                (Filename.concat
                   (Filename.concat (Unix.getenv "HOME") ".cache")
                   "liquidsoap")
      with Not_found -> None)

let rec recmkdir dir =
  if not (Sys.file_exists dir) then (
    recmkdir (Filename.dirname dir);
    Sys.mkdir dir 0o755)

let dir () =
  if enabled () then (
    match
      try Some (Unix.getenv "LIQ_CACHE_DIR")
      with Not_found ->
        let fn = !default_dir in
        let dir = fn () in
        Option.iter recmkdir dir;
        dir
    with
      | None ->
          Startup.message
            "Could not find default cache directory! You can set it using the \
             `$LIQ_CACHE_DIR` environment variable.";
          None
      | Some _ as v -> v)
  else (
    Startup.message "Cache disabled!";
    None)

let retrieve fname =
  try
    match dir () with
      | None -> None
      | Some dir ->
          let fname = Filename.concat dir fname in
          if Sys.file_exists fname then (
            let ic = open_in_bin fname in
            Fun.protect
              ~finally:(fun () -> close_in ic)
              (fun () ->
                let term = Marshal.from_channel ic in
                Startup.message "Loading script from cache!";
                Some term))
          else None
  with
    | Failure msg
      when String.starts_with ~prefix:"input_value: unknown code module" msg ->
        Startup.message "Liquidsoap binary changed: cache invalidated!";
        None
    | exn ->
        let bt = Printexc.get_backtrace () in
        let exn = Printexc.to_string exn in
        if Sys.getenv_opt "LIQ_DEBUG_CACHE" <> None then
          Startup.message "Error while loading cache: %s\n%s" exn bt
        else Startup.message "Error while loading cache: %s" exn;
        None

let store fname value =
  try
    match dir () with
      | None -> ()
      | Some dir ->
          let fname = Filename.concat dir fname in
          let oc = open_out fname in
          Fun.protect
            ~finally:(fun () -> close_out oc)
            (fun () ->
              Marshal.to_channel oc value [Marshal.Closures];
              let fn = !Hooks.cache_maintenance in
              fn ())
  with exn ->
    Startup.message "Error while saving cache: %s" (Printexc.to_string exn)
