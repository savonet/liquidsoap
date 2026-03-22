let detect_pkg c name =
  let open Configurator.V1 in
  match Pkg_config.get c with
    | None -> false
    | Some pc -> (
        match Pkg_config.query pc ~package:name with
          | None -> false
          | Some _ -> true)

let pkg_libs c name =
  let open Configurator.V1 in
  match Pkg_config.get c with
    | None -> []
    | Some pc -> (
        match Pkg_config.query pc ~package:name with
          | None -> []
          | Some conf -> conf.libs)

let write_sexp_bool file value =
  let oc = open_out file in
  output_string oc (if value then "true" else "false");
  close_out oc

let write_sexp_list file lst =
  let oc = open_out file in
  output_string oc "(";
  List.iter (fun s -> output_string oc (Printf.sprintf "%s " s)) lst;
  output_string oc ")";
  close_out oc

let () =
  match Sys.argv with
    | [| _; "detect"; pkg; output |] ->
        let c = Configurator.V1.create "liquidsoap-external-config" in
        write_sexp_bool output (detect_pkg c pkg)
    | [| _; "generate"; pkg; output |] ->
        let c = Configurator.V1.create "liquidsoap-external-config" in
        write_sexp_list output (pkg_libs c pkg)
    | _ ->
        Printf.eprintf "Usage: discover (detect|generate) <pkg> <output>\n";
        exit 1
