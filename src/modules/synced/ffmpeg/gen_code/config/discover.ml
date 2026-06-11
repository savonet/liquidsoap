module C = Configurator.V1

let packages =
  [
    "libavutil";
    "libavformat";
    "libavfilter";
    "libavcodec";
    "libavdevice";
    "libswresample";
    "libswscale";
  ]

let default_paths = ["/usr/local/include"; "/usr/include"]

let set_pkg_config_for_context context_name =
  let sanitized =
    String.map (fun c -> if c = '.' then '_' else c) context_name
  in
  (match Sys.getenv_opt ("PKG_CONFIG_PATH_" ^ sanitized) with
    | Some path -> Unix.putenv "PKG_CONFIG_PATH" path
    | None -> ());
  match Sys.getenv_opt ("PKG_CONFIG_" ^ sanitized) with
    | Some path -> Unix.putenv "PKG_CONFIG" path
    | None -> ()

let trim s =
  match String.split_on_char '\n' (String.trim s) with s :: _ -> s | _ -> s

let split_flags flags =
  let l = String.split_on_char ' ' flags in
  List.fold_left
    (fun cur flag ->
      match String.split_on_char 'I' flag with
        | [_; inc] -> trim inc :: cur
        | _ -> cur)
    [] l

let get_includedir c pkg_config package =
  match C.Process.run c pkg_config ["--variable=includedir"; package] with
    | { C.Process.exit_code; stdout; _ } when exit_code = 0 ->
        let dir = trim stdout in
        if dir = "" then None else Some dir
    | _ -> None

let add_path c pkg_config cur package =
  match C.Process.run c pkg_config ["--cflags-only-I"; package] with
    | { C.Process.exit_code; stdout; _ } when exit_code = 0 -> (
        let paths = split_flags stdout in
        match paths with
          | [] -> (
              match get_includedir c pkg_config package with
                | Some dir -> dir :: cur
                | None -> default_paths @ cur)
          | _ -> paths @ cur)
    | _ -> default_paths @ cur

let () =
  (match Array.to_list Sys.argv with
    | _ :: context_name :: _ ->
        let context =
          Option.value ~default:context_name
            (Sys.getenv_opt "LIQUIDSOAP_DUNE_TARGET")
        in
        set_pkg_config_for_context context;
        Arg.current := 1
    | _ -> (
        match Sys.getenv_opt "LIQUIDSOAP_DUNE_TARGET" with
          | Some ctx -> set_pkg_config_for_context ctx
          | None -> ()));
  C.main ~name:"ffmpeg-gen_code-pkg-config" (fun c ->
      let paths =
        let pkg_config =
          match Sys.getenv_opt "PKG_CONFIG" with
            | Some s -> Some s
            | None -> C.which c "pkg-config"
        in
        match pkg_config with
          | None -> default_paths
          | Some pkg_config ->
              List.fold_left (add_path c pkg_config) [] packages
      in
      let paths =
        List.map
          (fun path -> String.trim (Printf.sprintf "%S" path))
          (List.sort_uniq compare paths)
      in
      Printf.printf "let paths = [%s]" (String.concat "; " paths))
