open Ladspa

let plugins_dir = "/usr/lib/ladspa"

let plugins =
  if Array.length Sys.argv > 1 then [Sys.argv.(1)]
  else if Sys.file_exists plugins_dir && Sys.is_directory plugins_dir then (
    let dir = Unix.opendir plugins_dir in
    let ans = ref [] in
    (try
       while true do
         let f = Unix.readdir dir in
         if f <> "." && f <> ".." then ans := (plugins_dir ^ "/" ^ f) :: !ans
       done
     with End_of_file -> ());
    Unix.closedir dir;
    List.rev !ans)
  else []

let () = Printf.printf "LADSPA %s\n\n%!" (Ladspa.version ())

let print_port d n =
  Printf.printf ". %s: %s %s\n%!" (Descriptor.port_name d n)
    (if Descriptor.port_is_input d n then "input" else "output")
    (if Descriptor.port_is_control d n then "control" else "audio")

let print_descr d =
  for i = 0 to Descriptor.port_count d - 1 do
    print_port d i
  done;
  Printf.printf "\n%!"

let print_plugin pname =
  Printf.printf "* Loading plugin %s... %!" pname;
  try
    let p = Plugin.load pname in
    Printf.printf "done.\n\n%!";
    let descr = Descriptor.descriptors p in
    Printf.printf "- Found %d descriptors.\n%!" (Array.length descr);
    Array.iteri
      (fun i d ->
        Printf.printf "- Descriptor %d (%d ports): %s (%s) by %s, %s.\n%!" i
          (Descriptor.port_count d) (Descriptor.label d) (Descriptor.name d)
          (Descriptor.maker d)
          (match Descriptor.copyright d with Some c -> c | None -> "");
        print_descr d)
      descr;
    Printf.printf "\n%!";
    Plugin.unload p
  with Plugin.Not_a_plugin -> Printf.printf "failed.\n\n%!"

let () =
  List.iter print_plugin plugins;
  Gc.full_major ()
