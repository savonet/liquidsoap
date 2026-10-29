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
          let env_var =
            match dirtype with
              | `User -> "LIQ_CACHE_USER_DIR"
              | `System -> "LIQ_CACHE_SYSTEM_DIR"
          in
          Startup.message
            "Could not find default cache directory! You can set it using the \
             `$%s` environment variable."
            env_var;
          None
      | Some _ as v -> v)
  else (
    Startup.message "Cache disabled!";
    None)

type kind = [ `Term | `Table ]

(* Every file starts with a fixed header, integers little-endian: magic (8
   bytes), header version (u16), payload kind (u16), build ID (u64), payload
   length (u64) and payload hash (u64), then the marshalled payload. *)
let magic = "\x89LIQ\r\n\x1a\n"
let header_version = 1
let header_length = 36
let kind_code = function `Term -> 1 | `Table -> 2

let hash_channel ic length =
  let { Hooks.update; digest } = Hooks.get Hooks.hasher () in
  let buffer = Bytes.create 65536 in
  let rec feed remaining =
    if remaining > 0 then (
      let read = input ic buffer 0 (min remaining (Bytes.length buffer)) in
      if read = 0 then raise End_of_file;
      update (Bytes.unsafe_to_string buffer) 0 read;
      feed (remaining - read))
  in
  feed length;
  digest ()

let hash_string s =
  let { Hooks.update; digest } = Hooks.get Hooks.hasher () in
  update s 0 (String.length s);
  digest ()

let header ~kind payload =
  let b = Bytes.create header_length in
  Bytes.blit_string magic 0 b 0 (String.length magic);
  Bytes.set_uint16_le b 8 header_version;
  Bytes.set_uint16_le b 10 (kind_code kind);
  Bytes.set_int64_le b 12 (Hooks.get Hooks.build_id ());
  Bytes.set_int64_le b 20 (Int64.of_int (String.length payload));
  Bytes.set_int64_le b 28 (hash_string payload);
  Bytes.unsafe_to_string b

(* Leaves [ic] at the start of a payload that passed every check. *)
let check ~kind ic =
  let file_length = in_channel_length ic in
  if file_length < header_length then Error `Corrupted
  else (
    let h = really_input_string ic header_length in
    let payload_length = file_length - header_length in
    if String.sub h 0 (String.length magic) <> magic then Error `Format
    else if String.get_uint16_le h 8 <> header_version then Error `Format
    else if String.get_uint16_le h 10 <> kind_code kind then Error `Kind
    else if String.get_int64_le h 12 <> Hooks.get Hooks.build_id () then
      Error `Binary
    else if String.get_int64_le h 20 <> Int64.of_int payload_length then
      Error `Corrupted
    else if hash_channel ic payload_length <> String.get_int64_le h 28 then
      Error `Corrupted
    else (
      seek_in ic header_length;
      Ok ()))

let rejection_message = function
  | `Format -> "Cache format changed"
  | `Kind -> "Not a cache of this kind"
  | `Binary -> "Liquidsoap binary changed"
  | `Corrupted -> "Corrupted cache file"

let retrieve ?name ~dirtype ~kind filename =
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
                match check ~kind ic with
                  | Error reason ->
                      (match name with
                        | Some name ->
                            Startup.message "%s: %s cache invalidated!"
                              (rejection_message reason) name
                        | None ->
                            log#info "%s: %s ignored" (rejection_message reason)
                              (Filename.basename filename));
                      None
                  | Ok () ->
                      let value = Marshal.from_channel ic in
                      (match name with
                        | Some name ->
                            Startup.message "Loading %s from cache!" name
                        | None -> ());
                      Some value))
          else None
  with exn ->
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

let store ~dirtype ~kind filename value =
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
              let payload = Marshal.to_string value [Marshal.Closures] in
              output_string oc (header ~kind payload);
              output_string oc payload;
              close_out oc;
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
      table =
        Option.value ~default:Map.empty
          (retrieve ?name ~dirtype ~kind:`Table fname);
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

  let store ~dirtype t =
    if t.changed then store ~dirtype ~kind:`Table t.fname t.table
end
