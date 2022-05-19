include Dune_config

(* See: https://github.com/ocaml/dune/issues/4453 *)
let git_snapshot = false
let requests_max_id = 50
let requests_table_size = 50
let default_font = "/usr/share/fonts/truetype/dejavu/DejaVuSans.ttf"

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

let get_site name = match name with [] -> "" | s :: _ -> s

(* This is a hack. *)
let prefix () =
  List.fold_left Filename.concat
    (get_site Liquidsoap_sites.Sites.lib_root)
    [".."; ".."]

let rundir () =
  List.fold_left Filename.concat (prefix ()) ["var"; "run"; "liquidsoap"]

let logdir () =
  List.fold_left Filename.concat (prefix ()) ["var"; "log"; "liquidsoap"]

let liq_libs_dir () = get_site Liquidsoap_sites.Sites.libs
let bin_dir () = get_site Liquidsoap_sites.Sites.bin

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
