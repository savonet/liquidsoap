include Dune_config
include Liquidsoap_paths

(* See: https://github.com/ocaml/dune/issues/4453 *)
let git_snapshot = false
let requests_max_id = 50
let requests_table_size = 50
let default_font = "/usr/share/fonts/truetype/dejavu/DejaVuSans.ttf"

(** General configuration *)
let conf = Dtools.Conf.void "Liquidsoap configuration"

let libs_versions () =
  let libs =
    List.filter
      (fun lib ->
        not
          (Pcre.pmatch ~pat:"^liquidsoap_"
             (Build_info.V1.Statically_linked_library.name lib)))
      (Build_info.V1.Statically_linked_libraries.to_list ())
  in
  String.concat " "
    (List.map
       (fun lib ->
         String.concat "="
           ([Build_info.V1.Statically_linked_library.name lib]
           @
           match Build_info.V1.Statically_linked_library.version lib with
             | Some v -> [Build_info.V1.Version.to_string v]
             | None -> []))
       libs)

let () =
  Lifecycle.before_init (fun () ->
      Utils.add_subst "<sysrundir>" (rundir ());
      Utils.add_subst "<syslogdir>" (logdir ()))

let restart = ref false
let display_types = ref false

let version () =
  match Build_info.V1.version () with
    | Some v -> Build_info.V1.Version.to_string v
    | None -> "dev"

let vendor () =
  Printf.sprintf "Liquidsoap/%s (%s; OCaml %s)" (version ()) Sys.os_type
    Sys.ocaml_version

let path () =
  let s = try Sys.getenv "PATH" with Not_found -> "" in
  bin_dir () :: Str.split (Str.regexp_string ":") s
