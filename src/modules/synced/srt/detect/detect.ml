let write_sexp file lines =
  let oc = open_out file in
  output_string oc "(";
  output_string oc (String.concat " " lines);
  output_string oc ")";
  close_out oc

let write_lines file lines =
  let oc = open_out file in
  List.iter
    (fun s ->
      output_string oc s;
      output_char oc '\n')
    lines;
  close_out oc

let write_bool file value =
  let oc = open_out file in
  output_string oc (if value then "true" else "false");
  close_out oc

let filter_win32_libs os_type libs =
  if os_type = "Win32" then
    List.filter
      (fun flag ->
        String.length flag < 3
        || String.sub flag 0 3 <> "-Wl"
           && flag <> "-static-libgcc" && flag <> "-lssp" && flag <> "-lmingw32")
      libs
  else libs

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
        let c = create "srt-detect" in
        let available, cflags, libs =
          if is_excluded name then (false, [], [])
          else (
            match Pkg_config.get c with
              | None -> (false, [], [])
              | Some pc -> (
                  match
                    Pkg_config.query pc ~package:(String.concat " " packages)
                  with
                    | None -> (false, [], [])
                    | Some conf -> (true, conf.cflags, conf.libs)))
        in
        let libs = filter_win32_libs Sys.os_type libs in
        write_bool (name ^ "_available") available;
        write_sexp (name ^ "_c_flags.sexp") cflags;
        write_lines (name ^ "_c_flags") cflags;
        write_sexp (name ^ "_c_library_flags.sexp") libs
    | _ ->
        Printf.eprintf "Usage: detect --context <context> <name> <package...>\n";
        exit 1
