let plugins_dir = "/usr/lib/dssi"

let plugins =
  if Array.length Sys.argv > 1 then [Sys.argv.(1)]
  else if Sys.file_exists plugins_dir && Sys.is_directory plugins_dir then (
    let dir = Unix.opendir plugins_dir in
    let ans = ref [] in
    (try
       while true do
         let f = Unix.readdir dir in
         if f <> "." && f <> ".." then (
           let f = plugins_dir ^ "/" ^ f in
           if not (Sys.is_directory f) then ans := f :: !ans)
       done
     with End_of_file -> ());
    Unix.closedir dir;
    List.rev !ans)
  else []

let () =
  Dssi.init ();
  List.iter
    (fun fname ->
      Printf.printf "Loading %s\n%!" fname;
      let p = Dssi.Plugin.load fname in
      let d = Dssi.Descriptor.descriptor p 0 in
      Printf.printf "API version: %d\n%!" (Dssi.Descriptor.api_version d);
      try
        let ladspa = Dssi.Descriptor.ladspa d in
        let inst = Ladspa.Descriptor.instantiate ladspa 44100 in
        let p_bank, p_program, p_name = Dssi.Descriptor.get_program d inst 0 in
        Printf.printf "Program %d,%d: %s\n\n%!" p_bank p_program p_name
      with Dssi.Descriptor.Not_implemented ->
        Printf.printf "Error: no descriptor\n\n%!")
    plugins
