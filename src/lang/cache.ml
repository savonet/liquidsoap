type dirtype = [ `System | `User ]

let enabled () =
  try
    let venv = Unix.getenv "LIQ_CACHE" in
    venv = "1" || venv = "true"
  with Not_found -> true

let system_dir_override = ref (fun () -> None)
let user_dir_override = ref (fun () -> None)
let system_dir_perms = ref 0o755
let system_file_perms = ref 0o644
let user_dir_perms = ref 0o700
let user_file_perms = ref 0o600

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
              Sys.rename tmp_file filename);
          let fn = !Hooks.cache_maintenance in
          fn dirtype
  with exn ->
    let bt = Printexc.get_backtrace () in
    let exn = Printexc.to_string exn in
    if Sys.getenv_opt "LIQ_DEBUG_CACHE" <> None then
      Startup.message "Error while loading cache: %s\n%s" exn bt
    else Startup.message "Error while loading cache: %s" exn

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
