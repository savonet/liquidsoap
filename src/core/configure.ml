open Liquidsoap_lang
include Build_config
include Liquidsoap_paths

let git_snapshot = git_sha <> None
let requests_max_id = 50
let requests_table_size = 50
let default_font = "/usr/share/fonts/truetype/dejavu/DejaVuSans.ttf"

(** General configuration *)
let conf = Dtools.Conf.void "Liquidsoap configuration"

let libs_versions () =
  Build_info.V1.Statically_linked_libraries.to_list ()
  |> List.map (fun lib ->
         let name = Build_info.V1.Statically_linked_library.name lib in
         let version =
           Build_info.V1.Statically_linked_library.version lib
           |> Option.map Build_info.V1.Version.to_string
           |> Option.value ~default:"?"
         in
         (name, version))
  |> List.sort compare
  |> List.map (fun (name, version) ->
         if version = "?" then name else name ^ "=" ^ version)
  |> String.concat " "

let restart = ref false

let vendor =
  Printf.sprintf "Liquidsoap/%s (%s; OCaml %s)" version Sys.os_type
    Sys.ocaml_version

let path () =
  let s = try Sys.getenv "PATH" with Not_found -> "" in
  bin_dir () :: Str.split (Str.regexp_string ":") s

let () = conf#plug "log" Dtools.Log.conf

let conf_init =
  conf#plug "init" Dtools.Init.conf;
  Dtools.Init.conf

let conf_debug =
  Dtools.Conf.bool ~p:(conf#plug "debug") ~d:!Term.conf_debug
    "Debug language features such as type inference and reduction."

let conf_debug_errors =
  Dtools.Conf.bool ~p:(conf#plug "debug_errors") ~d:!Term.conf_debug_errors
    "Debug errors by showing stacktraces instead of printing messages."

let () =
  conf_debug#on_change (fun v -> Term.conf_debug := v);
  conf_debug_errors#on_change (fun v -> Term.conf_debug_errors := v)
