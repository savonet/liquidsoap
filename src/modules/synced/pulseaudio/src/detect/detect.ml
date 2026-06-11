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

let has_pulseaudio_code =
  {|
#include <pulse/simple.h>

int main()
{
  pa_simple_new(NULL, "test", PA_STREAM_PLAYBACK, NULL, "test", NULL, NULL, NULL, NULL);
  return 0;
}
|}

let default_flags = ["-lpulse"; "-lpulse-simple"]

let () =
  let argv = Array.to_list Sys.argv |> List.tl in
  (match argv with
    | "--context" :: context_name :: _ ->
        set_pkg_config_for_context context_name
    | _ ->
        Option.iter set_pkg_config_for_context
          (Sys.getenv_opt "LIQUIDSOAP_DUNE_TARGET"));
  let open Configurator.V1 in
  let c = create "pulseaudio-detect" in
  let available, cflags, libs =
    if is_excluded "pulseaudio" then (false, [], [])
    else (
      match Pkg_config.get c with
        | Some pc -> (
            match Pkg_config.query pc ~package:"libpulse" with
              | Some libpulse -> (
                  match Pkg_config.query pc ~package:"libpulse-simple" with
                    | Some libpulse_simple ->
                        ( true,
                          libpulse.cflags @ libpulse_simple.cflags,
                          libpulse.libs @ libpulse_simple.libs )
                    | None ->
                        if
                          c_test c ~link_flags:default_flags has_pulseaudio_code
                        then (true, [], default_flags)
                        else (false, [], []))
              | None ->
                  if c_test c ~link_flags:default_flags has_pulseaudio_code then
                    (true, [], default_flags)
                  else (false, [], []))
        | None ->
            if c_test c ~link_flags:default_flags has_pulseaudio_code then
              (true, [], default_flags)
            else (false, [], []))
  in
  write_bool "pulseaudio_available" available;
  write_sexp "pulseaudio_c_flags.sexp" cflags;
  write_sexp "pulseaudio_c_library_flags.sexp" libs
