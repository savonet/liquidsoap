let cache_enabled () =
  try
    let venv = Unix.getenv "LIQ_CACHE" in
    venv = "1" || venv = "true"
  with Not_found -> true

let default_cache_dir =
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

let cache_dir () =
  if cache_enabled () then (
    match
      try Some (Unix.getenv "LIQ_CACHE_DIR")
      with Not_found ->
        let fn = !default_cache_dir in
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

let rec recmkdir dir =
  if not (Sys.file_exists dir) then (
    recmkdir (Filename.dirname dir);
    Sys.mkdir dir 0o755)

let cache_filename ~toplevel term =
  match cache_dir () with
    | None -> None
    | Some dir ->
        recmkdir dir;
        let hash = Parsed_term.hash term in
        let fname =
          Printf.sprintf "%s%s.liq-cache" hash
            (if toplevel then "-toplevel" else "")
        in
        Some (Filename.concat dir fname)

let retrieve ~toplevel parsed_term : Term.t option =
  Startup.time "Cache retrieval" (fun () ->
      try
        match cache_filename ~toplevel parsed_term with
          | None -> None
          | Some filename ->
              if Sys.file_exists filename then (
                let ic = open_in_bin filename in
                Fun.protect
                  ~finally:(fun () -> close_in ic)
                  (fun () ->
                    let term = Marshal.from_channel ic in
                    Startup.message "Loading script from cache!";
                    Some term))
              else None
      with
        | Failure msg
          when String.starts_with ~prefix:"input_value: unknown code module" msg
          ->
            Startup.message "Liquidsoap binary changed: cache invalidated!";
            None
        | exn ->
            let bt = Printexc.get_backtrace () in
            let exn = Printexc.to_string exn in
            if Sys.getenv_opt "LIQ_DEBUG_CACHE" <> None then
              Startup.message "Error while loading cache: %s\n%s" exn bt
            else Startup.message "Error while loading cache: %s" exn;
            None)

let cache ~toplevel ~parsed_term term =
  try
    match cache_filename ~toplevel parsed_term with
      | None -> ()
      | Some filename ->
          let oc = open_out filename in
          Fun.protect
            ~finally:(fun () -> close_out oc)
            (fun () ->
              let term = Marshal.to_channel oc term [Marshal.Closures] in
              let fn = !Hooks.cache_maintenance in
              fn ();
              term)
  with exn ->
    Startup.message "Error while saving cache: %s" (Printexc.to_string exn)
