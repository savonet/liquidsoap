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
let display_types = Typechecking.display_types

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

let conf_console =
  Dtools.Conf.void ~p:(conf#plug "console") "Console configuration"

let conf_colorize =
  Dtools.Conf.string
    ~p:(conf_console#plug "colorize")
    ~d:
      (match !Console.color_conf with
        | `Auto -> "auto"
        | `Always -> "always"
        | `Never -> "never")
    "Use color in console output when available. One of: \"always\", \"never\" \
     or \"auto\"."

let () =
  let log = Log.make ["console"] in
  conf_colorize#on_change
    (function
      | "auto" -> Console.color_conf := `Auto
      | "always" -> Console.color_conf := `Always
      | "never" -> Console.color_conf := `Never
      | _ ->
          log#important "Invalid color configuration, using default \"auto\"";
          Console.color_conf := `Auto)
    i

let conf_debug =
  Dtools.Conf.bool ~p:(conf#plug "debug") ~d:!Term.conf_debug
    "Debug language features such as type inference and reduction."

let conf_debug_errors =
  Dtools.Conf.bool ~p:(conf#plug "debug_errors") ~d:!Term.conf_debug_errors
    "Debug errors by showing stacktraces instead of printing messages."

let () =
  conf_debug#on_change (fun v -> conf_debug := v);
  conf_debug_errors#on_change (fun v -> conf_debug_errors := v)

let () =
  let on_change v =
    Evaluation.log_path :=
      if v then Some Dtools.Log.conf_file_path#get else None
  in
  Dtools.Log.conf_file#on_change on_change;
  on_change Dtools.Log.conf_file_#get
