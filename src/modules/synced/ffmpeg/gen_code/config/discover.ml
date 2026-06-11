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

let add_path c pkg_config cur package =
  match C.Process.run c pkg_config ["--cflags-only-I"; package] with
    | { C.Process.exit_code; stdout; _ } when exit_code = 0 ->
        let paths = split_flags stdout in
        (if paths = [] then default_paths else paths) @ cur
    | _ -> default_paths @ cur

let () =
  (match Array.to_list Sys.argv with
    | _ :: context_name :: _ ->
        Printf.eprintf "discover: context=%s\n%!" context_name;
        set_pkg_config_for_context context_name;
        Printf.eprintf "discover: PKG_CONFIG=%s\n%!"
          (Option.value ~default:"(not set)" (Sys.getenv_opt "PKG_CONFIG"));
        Printf.eprintf "discover: PKG_CONFIG_PATH=%s\n%!"
          (Option.value ~default:"(not set)" (Sys.getenv_opt "PKG_CONFIG_PATH"));
        Arg.current := 1
    | _ -> ());
  C.main ~name:"ffmpeg-gen_code-pkg-config" (fun c ->
      let paths =
        let pkg_config =
          match Sys.getenv_opt "PKG_CONFIG" with
            | Some s ->
                Printf.eprintf "discover: using PKG_CONFIG=%s\n%!" s;
                Some s
            | None ->
                let found = C.which c "pkg-config" in
                Printf.eprintf "discover: pkg-config from PATH=%s\n%!"
                  (Option.value ~default:"(not found)" found);
                found
        in
        match pkg_config with
          | None ->
              Printf.eprintf "discover: no pkg-config found, using defaults\n%!";
              default_paths
          | Some pkg_config ->
              let paths = List.fold_left (add_path c pkg_config) [] packages in
              Printf.eprintf "discover: found paths: %s\n%!"
                (String.concat ", " paths);
              paths
      in
      let paths =
        List.map
          (fun path -> String.trim (Printf.sprintf "%S" path))
          (List.sort_uniq compare paths)
      in
      Printf.printf "let paths = [%s]" (String.concat "; " paths))
