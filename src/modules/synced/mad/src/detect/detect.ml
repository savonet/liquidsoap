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

let is_excluded () =
  match Sys.getenv_opt "LIQUIDSOAP_MINIMAL_EXCLUDE_DEPS" with
    | None -> false
    | Some excluded -> List.mem "mad" (String.split_on_char ' ' excluded)

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

let has_mad_h_code =
  {|
#include <mad.h>

int main()
{
  struct mad_stream stream;
  mad_stream_init(&stream);
  return 0;
}
|}

let default_flags = ["-lmad"]

let () =
  let argv = Array.to_list Sys.argv |> List.tl in
  let context, packages =
    match argv with
      | "--context" :: ctx :: rest -> (Some ctx, rest)
      | _ -> (None, argv)
  in
  (match Sys.getenv_opt "LIQUIDSOAP_DUNE_TARGET" with
    | Some ctx -> set_pkg_config_for_context ctx
    | None -> Option.iter set_pkg_config_for_context context);
  let open Configurator.V1 in
  let c = create "mad-detect" in
  let available, cflags, libs =
    if is_excluded () then (false, [], [])
    else (
      match Pkg_config.get c with
        | Some pc -> (
            match
              List.fold_left
                (fun found package ->
                  match found with
                    | Some _ -> found
                    | None -> Pkg_config.query pc ~package)
                None packages
            with
              | Some conf -> (true, conf.cflags, conf.libs)
              | None ->
                  if c_test c ~link_flags:default_flags has_mad_h_code then
                    (true, [], default_flags)
                  else (false, [], []))
        | None ->
            if c_test c ~link_flags:default_flags has_mad_h_code then
              (true, [], default_flags)
            else (false, [], []))
  in
  write_bool "mad_available" available;
  write_sexp "mad_c_flags.sexp" cflags;
  write_sexp "mad_c_library_flags.sexp" libs
