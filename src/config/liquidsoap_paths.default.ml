module Sites = Liquidsoap_lang.Sites.Sites

type mode = [ `Default | `Standalone | `Posix ]

let mode = `Default
let get_site name = match name with [] -> "" | s :: _ -> s

(* This is a hack. *)
let prefix () = Filename.(dirname (get_site Sites.lib_root))

let rundir () =
  List.fold_left Filename.concat (prefix ()) ["var"; "liquidsoap"; "run"]

let rundir_descr = "(set by dune-site)"

let logdir () =
  List.fold_left Filename.concat (prefix ()) ["var"; "liquidsoap"; "log"]

let logdir_descr = "(set by dune-site)"
let liq_libs_dir () = get_site Sites.libs
let liq_libs_dir_descr = "(set by dune-site)"
let bin_dir () = get_site Sites.bin
let bin_dir_descr = "(set by dune-site)"
let camomile_dir () = Filename.dirname CamomileLib.Config.Default.datadir
let camomile_dir_descr = "(set by dune-site)"
let user_cache_override () = None
let user_cache_override_descr = "$HOME/.cache/liquidsoap"

let system_cache_override () =
  match Sites.cache with [] -> None | d :: _ -> Some d

let system_cache_override_descr = "(set by dune-site)"
