(* The peer is a program of its own: against a static libsrt, only pkg-config's
   private libraries bring what libsrt needs, the C++ runtime and its TLS
   library. A shared libsrt needs nothing beyond the binding's flags. *)
open Configurator.V1

let shared_library c =
  match Process.run c "pkg-config" ["--variable=libdir"; "srt"] with
    | { exit_code = 0; stdout } ->
        let libdir = String.trim stdout in
        List.exists
          (fun name -> Sys.file_exists (Filename.concat libdir name))
          ["libsrt.so"; "libsrt.dylib"; "libsrt.dll.a"]
    | _ -> true

let static_libs c =
  Unix.putenv "PKG_CONFIG_ARGN" "--static";
  match Pkg_config.get c with
    | None -> []
    | Some pc -> (
        match Pkg_config.query pc ~package:"srt" with
          | Some conf -> conf.libs
          | None -> [])

let () =
  let c = create "srt-peer" in
  let libs = if shared_library c then [] else static_libs c in
  Flags.write_sexp "srt_peer_link_flags.sexp"
    (List.concat_map (fun flag -> ["-cclib"; flag]) libs)
