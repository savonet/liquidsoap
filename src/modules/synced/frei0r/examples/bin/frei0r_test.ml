open Frei0r

let () =
  let v, v' = Frei0r.version () in
  Printf.printf "Using frei0r %d.%d\n\n%!" v v';
  let test_plugin fname =
    let p = Frei0r.load fname in
    let info = Frei0r.info p in
    Printf.printf "%s %d.%d (by %s) for frei0r %d:\n%s.\n%!" info.name
      info.major_version info.minor_version info.author info.frei0r_version
      info.explanation;
    Printf.printf "\nPlugin type: %s.\n%!"
      (Frei0r.string_of_plugin_type info.plugin_type);
    Printf.printf "Color model: %s.\n%!"
      (Frei0r.string_of_color_model info.color_model);
    Printf.printf "\n%d parameters:\n%!" info.num_params;
    for i = 0 to info.num_params - 1 do
      let info = Frei0r.param_info p i in
      Printf.printf " - %s (%s): %s\n%!" info.param_name
        (Frei0r.string_of_param_type info.param_type)
        info.param_explanation
    done;
    Printf.printf "\n"
  in
  let try_test_plugin fname =
    try test_plugin fname
    with exn ->
      Printf.eprintf "Skipping %s: %s\n" fname (Printexc.to_string exn)
  in
  let test_plugin_dir dir =
    try
      let d = Unix.opendir dir in
      (try
         while true do
           let f = Unix.readdir d in
           if f <> "." && f <> ".." then try_test_plugin (dir ^ "/" ^ f)
         done
       with End_of_file -> ());
      Unix.closedir d
    with Unix.Unix_error (Unix.ENOENT, _, _) -> ()
  in
  let paths =
    if Array.length Sys.argv > 1 then List.tl (Array.to_list Sys.argv)
    else Frei0r.default_paths
  in
  List.iter test_plugin_dir paths;
  Gc.full_major ()
