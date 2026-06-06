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

external is_big_endian : unit -> bool = "ocaml_lame_is_big_endian"

let write_config_h () =
  let oc = open_out "lame_config" in
  if is_big_endian () then output_string oc "#define BIGENDIAN 1\n";
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

let has_lame_h_code =
  {|
#include <lame/lame.h>

int main()
{
  lame_init();
  return 0;
}
|}

let default_flags = ["-lmp3lame"; "-lm"]

let () =
  match Array.to_list Sys.argv |> List.tl with
    | "--context" :: context_name :: _ ->
        set_pkg_config_path_for_context context_name;
        write_config_h ();
        let open Configurator.V1 in
        let c = create "lame-detect" in
        let available, cflags, libs =
          if is_excluded "lame" then (false, [], [])
          else (
            match Pkg_config.get c with
              | Some pc -> (
                  match Pkg_config.query pc ~package:"mp3lame" with
                    | Some conf -> (true, conf.cflags, conf.libs)
                    | None -> (
                        match Pkg_config.query pc ~package:"lame" with
                          | Some conf -> (true, conf.cflags, conf.libs)
                          | None ->
                              if
                                c_test c ~link_flags:default_flags
                                  has_lame_h_code
                              then (true, [], default_flags)
                              else (false, [], [])))
              | None ->
                  if c_test c ~link_flags:default_flags has_lame_h_code then
                    (true, [], default_flags)
                  else (false, [], []))
        in
        write_bool "lame_available" available;
        write_sexp "lame_c_flags.sexp" cflags;
        write_sexp "lame_c_library_flags.sexp" libs
    | _ ->
        Printf.eprintf "Usage: detect --context <context>\n";
        exit 1
