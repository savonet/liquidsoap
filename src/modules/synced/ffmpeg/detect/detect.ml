let write_lines file lines =
  let oc = open_out file in
  List.iter (fun line -> output_string oc (line ^ "\n")) lines;
  close_out oc

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

let filter_win32_libs os_type libs =
  if os_type = "Win32" then
    List.filter
      (fun flag ->
        String.length flag < 3
        || (String.sub flag 0 3 <> "-Wl" && flag <> "-static-libgcc"))
      libs
  else libs

let usage () =
  Printf.eprintf
    "Usage: detect [--os-type <type>] <name> <package> <expr> [extra-cflags...]\n";
  exit 1

let () =
  let argv = Array.to_list Sys.argv |> List.tl in
  let os_type, argv =
    match argv with
      | "--os-type" :: v :: rest -> (v, rest)
      | _ -> (Sys.os_type, argv)
  in
  match argv with
    | name :: package :: expr :: extra_cflags ->
        let open Configurator.V1 in
        let c = create "ffmpeg-detect" in
        let available, cflags, libs =
          match Pkg_config.get c with
            | None -> (false, [], [])
            | Some pc -> (
                match Pkg_config.query_expr_err pc ~package ~expr with
                  | Error _ -> (false, [], [])
                  | Ok conf -> (true, conf.cflags @ extra_cflags, conf.libs))
        in
        let libs = filter_win32_libs os_type libs in
        write_bool (name ^ "_available") available;
        write_sexp (name ^ "_c_flags.sexp") cflags;
        write_lines (name ^ "_c_flags") cflags;
        write_sexp (name ^ "_c_library_flags.sexp") libs
    | _ -> usage ()
