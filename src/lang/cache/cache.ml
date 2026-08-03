type dirtype = [ `System | `User ]

let enabled () =
  try
    let venv = Unix.getenv "LIQ_CACHE" in
    venv = "1" || venv = "true"
  with Not_found -> true

let log = Hooks.log ["cache"]

let int_from_env ~default name =
  try int_of_string (Sys.getenv name) with _ -> default

let system_dir_override = ref (fun () -> None)
let user_dir_override = ref (fun () -> None)

let system_dir_perms =
  ref (int_from_env ~default:0o755 "LIQ_CACHE_SYSTEM_DIR_PERMS")

let system_file_perms =
  ref (int_from_env ~default:0o644 "LIQ_CACHE_SYSTEM_FILE_PERMS")

let user_dir_perms =
  ref (int_from_env ~default:0o700 "LIQ_CACHE_USER_DIR_PERMS")

let user_file_perms =
  ref (int_from_env ~default:0o600 "LIQ_CACHE_USER_FILE_PERMS")

(** Cached entries are evicted once they are this old, and the oldest ones are
    evicted past this count. *)
let max_days = int_from_env ~default:10 "LIQ_CACHE_MAX_DAYS"

let max_files = int_from_env ~default:20 "LIQ_CACHE_MAX_FILES"

let default_user_dir () =
  try Some (Unix.getenv "LIQ_CACHE_USER_DIR")
  with Not_found -> (
    let fn = !user_dir_override in
    match fn () with
      | Some d -> Some d
      | _ ->
          Some
            (Filename.concat
               (Filename.concat (Unix.getenv "HOME") ".cache")
               "liquidsoap"))

let default_system_dir () =
  try Some (Unix.getenv "LIQ_CACHE_SYSTEM_DIR")
  with Not_found -> (
    let fn = !system_dir_override in
    match (fn (), Sites.Sites.cache) with
      | Some d, _ | _, d :: _ -> Some d
      | _ -> None)

let rec recmkdir ~dirtype dir =
  let perms =
    match dirtype with `System -> !system_dir_perms | `User -> !user_dir_perms
  in
  if not (Sys.file_exists dir) then (
    recmkdir ~dirtype (Filename.dirname dir);
    Sys.mkdir dir perms)

let dir dirtype =
  if enabled () then (
    match
      let fn =
        match dirtype with
          | `User -> default_user_dir
          | `System -> default_system_dir
      in
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

let retrieve ?name ~dirtype filename =
  try
    match dir dirtype with
      | None -> None
      | Some dir ->
          let filename = Filename.concat dir filename in
          if Sys.file_exists filename then (
            let ic = open_in_bin filename in
            Fun.protect
              ~finally:(fun () -> close_in_noerr ic)
              (fun () ->
                let value = Marshal.from_channel ic in
                (match name with
                  | Some name -> Startup.message "Loading %s from cache!" name
                  | None -> ());
                Some value))
          else None
  with
    | Failure msg
      when String.starts_with ~prefix:"input_value: unknown code module" msg ->
        (match name with
          | Some name ->
              Startup.message "Liquidsoap binary changed: %s cache invalidated!"
                name
          | None -> ());
        None
    | exn ->
        let bt = Printexc.get_backtrace () in
        let exn = Printexc.to_string exn in
        if Sys.getenv_opt "LIQ_DEBUG_CACHE" <> None then
          Startup.message "Error while loading cache: %s\n%s" exn bt
        else Startup.message "Error while loading cache: %s" exn;
        None

(** Evict entries that are too old, then the oldest ones if there are still too
    many. Run after every store. *)
let maintenance dirtype =
  let max_timestamp = Unix.time () -. (float max_days *. 86400.) in
  try
    match dir dirtype with
      | Some dir when Sys.file_exists dir && Sys.is_directory dir ->
          let files =
            Array.fold_left
              (fun files fname ->
                if String.ends_with ~suffix:".liq-cache" fname then (
                  let filename = Filename.concat dir fname in
                  let stats = Unix.stat filename in
                  match stats with
                    | { Unix.st_atime } when st_atime < max_timestamp ->
                        log#info "File %s is too old, deleting.." fname;
                        Unix.unlink filename;
                        files
                    | _ -> (stats, filename) :: files)
                else files)
              [] (Sys.readdir dir)
          in
          let len = List.length files in
          if max_files < len then (
            let len = len - max_files in
            log#info "Too many cached files! Deleting %d oldest ones.." len;
            let files =
              List.sort
                (fun ({ Unix.st_atime = t }, _) ({ Unix.st_atime = t' }, _) ->
                  Stdlib.compare t t')
                files
            in
            List.iteri
              (fun pos (_, filename) ->
                if pos < len then (
                  log#info "Deleting %s.." (Filename.basename filename);
                  Unix.unlink filename))
              files)
      | _ -> ()
  with exn ->
    log#severe "Error while cleaning up cache: %s" (Printexc.to_string exn)

let store ~dirtype filename value =
  try
    match dir dirtype with
      | None -> ()
      | Some dir ->
          recmkdir ~dirtype dir;
          let filename = Filename.concat dir filename in
          let perms =
            match dirtype with
              | `User -> !user_file_perms
              | `System -> !system_file_perms
          in
          let tmp_file, oc =
            Filename.open_temp_file ~mode:[Open_binary]
              ~temp_dir:(Filename.dirname filename)
              ~perms "tmp" ".liq-cache"
          in
          Fun.protect
            ~finally:(fun () ->
              close_out_noerr oc;
              if Sys.file_exists tmp_file then Sys.remove tmp_file)
            (fun () ->
              Marshal.to_channel oc value [Marshal.Closures];
              close_out_noerr oc;
              Sys.rename tmp_file filename);
          maintenance dirtype
  with exn ->
    let bt = Printexc.get_backtrace () in
    let exn = Printexc.to_string exn in
    if Sys.getenv_opt "LIQ_DEBUG_CACHE" <> None then
      Startup.message "Error while storing cache: %s\n%s" exn bt
    else Startup.message "Error while storing cache: %s" exn

(** A key-value table in cache. *)
module Table = struct
  module Map = Map.Make (String)

  type 'a t = {
    fname : string;
    mutable table : 'a Map.t;
    mutable changed : bool;
  }

  let load ?name ~dirtype fname =
    {
      fname;
      table = Option.value ~default:Map.empty (retrieve ?name ~dirtype fname);
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

  let store ~dirtype t = if t.changed then store ~dirtype t.fname t.table
end
