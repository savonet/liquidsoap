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

let () =
  match Array.to_list Sys.argv |> List.tl with
    | name :: packages ->
        let open Configurator.V1 in
        let c = create "xiph-detect" in
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
        write_bool (name ^ "_available") available;
        write_sexp (name ^ "_c_flags.sexp") cflags;
        write_sexp (name ^ "_c_library_flags.sexp") libs
    | _ ->
        Printf.eprintf "Usage: detect <name> <package...>\n";
        exit 1
