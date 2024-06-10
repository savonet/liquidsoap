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

let dir () =
  if enabled () then (
    match
      try Some (Unix.getenv "LIQ_CACHE_DIR")
      with Not_found ->
        let fn = !default_dir in
        fn ()
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
