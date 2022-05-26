let () =
  let location = Filename.dirname Sys.executable_name in
  let libs =
    List.filter
      (fun f -> Filename.extension f = ".liq")
      (Array.to_list (Sys.readdir location))
  in
  Printf.printf
    {|
(install
 (section
  (site
   (liquidsoap-core libs)))
 (package liquidsoap-stdlib)
 (files
    %s))
  |}
    (String.concat "\n" libs)
