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

external is_big_endian : unit -> bool = "ocaml_shine_is_big_endian"

let write_config_h () =
  let oc = open_out "shine_config" in
  if is_big_endian () then output_string oc "#define IS_BIGENDIAN 1\n";
  close_out oc

let is_excluded name =
  match Sys.getenv_opt "LIQUIDSOAP_MINIMAL_EXCLUDE_DEPS" with
    | None -> false
    | Some excluded -> List.mem name (String.split_on_char ' ' excluded)

let set_pkg_config_path_for_context context_name =
  let sanitized =
    String.map (fun c -> if c = '.' then '_' else c) context_name
  in
  match Sys.getenv_opt ("PKG_CONFIG_PATH_" ^ sanitized) with
    | Some path -> Unix.putenv "PKG_CONFIG_PATH" path
    | None -> ()

let () =
  match Array.to_list Sys.argv |> List.tl with
    | "--context" :: context_name :: _ ->
        set_pkg_config_path_for_context context_name;
        write_config_h ();
        let open Configurator.V1 in
        let c = create "shine-detect" in
        let available, cflags, libs =
          if is_excluded "shine" then (false, [], [])
          else (
            match Pkg_config.get c with
              | None -> (false, [], [])
              | Some pc -> (
                  match Pkg_config.query pc ~package:"shine" with
                    | None -> (false, [], [])
                    | Some conf -> (true, conf.cflags, conf.libs)))
        in
        write_bool "shine_available" available;
        write_sexp "shine_c_flags.sexp" cflags;
        write_sexp "shine_c_library_flags.sexp" libs
    | _ ->
        Printf.eprintf "Usage: detect --context <context>\n";
        exit 1
