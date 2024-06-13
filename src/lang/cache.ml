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

let retrieve filename =
        try
        match dir () with
          | None -> None
          | Some dir ->
            let filename = Filename.concat dir filename in
              if Sys.file_exists filename then (
                let ic = open_in_bin filename in
                Fun.protect
                  ~finally:(fun () -> close_in_noerr ic)
                  (fun () ->
                    let term = Marshal.from_channel ic in
                    (match name with
                      | Some name ->
                          Startup.message "Loading %s from cache!" name
                      | None -> ());
                    Some term))
              else None
      with
        | Failure msg
          when String.starts_with ~prefix:"input_value: unknown code module" msg
          ->
            (match name with
              | Some name ->
                  Startup.message
                    "Liquidsoap binary changed: %s cache invalidated!" name
              | None -> ());
            None
        | exn ->
            let bt = Printexc.get_backtrace () in
            let exn = Printexc.to_string exn in
            if Sys.getenv_opt "LIQ_DEBUG_CACHE" <> None then
              Startup.message "Error while loading cache: %s\n%s" exn bt
            else Startup.message "Error while loading cache: %s" exn;
            None

let store filename value =
  try
    match dir () with
      | None -> ()
      | Some dir ->
        let filename = Filename.concat dir filename in
          let tmp_file, oc =
            Filename.open_temp_file
              ~temp_dir:(Filename.dirname filename)
              "tmp" ".liq-cache"
          in
          Fun.protect
            ~finally:(fun () ->
              close_out_noerr oc;
              if Sys.file_exists tmp_file then Sys.remove tmp_file)
            (fun () ->
              Marshal.to_channel oc term [Marshal.Closures];
              Sys.rename tmp_file filename);
          let fn = !Hooks.cache_maintenance in
          fn ()
  with exn ->
    Startup.message "Error while saving cache: %s" (Printexc.to_string exn)

(** A key-value table in cache. *)
module Table = struct
  module Map = Map.Make (String)

  type 'a t = {
    fname : string;
    mutable table : 'a Map.t;
    mutable changed : bool;
  }

  let load fname =
    {
      fname;
      table = Option.value ~default:Map.empty (retrieve fname);
      changed = false;
    }

  (* Get an element, and provide a function to compute it if not cached. *)
  let get t k f =
    match Map.find_opt k t.table with
      | Some v -> v
      | None ->
          let v = f () in
          t.table <- Map.add k v t.table;
          t.changed <- true;
          v

  let store t = if t.changed then store t.fname t.table
end
