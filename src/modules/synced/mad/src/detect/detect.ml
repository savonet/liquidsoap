let write_sexp file lines =
  let oc = open_out file in
  output_string oc "(";
  output_string oc (String.concat " " lines);
  output_string oc ")";
  close_out oc

let write_bool file value =
  let oc = open_out file in
  output_string oc (if value then "true" else "false");
  close_out oc

let is_excluded name =
  match Sys.getenv_opt "LIQUIDSOAP_MINIMAL_EXCLUDE_DEPS" with
    | None -> false
    | Some excluded -> List.mem name (String.split_on_char ' ' excluded)

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

let () =
  let argv = Array.to_list Sys.argv |> List.tl in
  let context, argv =
    match argv with
      | "--context" :: ctx :: rest -> (Some ctx, rest)
      | _ -> (None, argv)
  in
  (match Sys.getenv_opt "LIQUIDSOAP_DUNE_TARGET" with
    | Some ctx -> set_pkg_config_for_context ctx
    | None -> Option.iter set_pkg_config_for_context context);
  match argv with
    | name :: packages ->
        let open Configurator.V1 in
        let c = create "mad-detect" in
        let available, cflags, libs =
          if is_excluded name then (false, [], [])
          else (
            match Pkg_config.get c with
              | None -> (false, [], [])
              | Some pc ->
                  List.fold_left
                    (fun (found, cflags, libs) package ->
                      if found then (found, cflags, libs)
                      else (
                        match Pkg_config.query pc ~package with
                          | None -> (false, cflags, libs)
                          | Some conf -> (true, conf.cflags, conf.libs)))
                    (false, [], []) packages)
        in
        write_bool (name ^ "_available") available;
        write_sexp (name ^ "_c_flags.sexp") cflags;
        write_sexp (name ^ "_c_library_flags.sexp") libs
    | _ ->
        Printf.eprintf "Usage: detect --context <context> <name> <package...>\n";
        exit 1
