let () =
  let location = Filename.dirname Sys.executable_name in
  let libs =
    List.sort Stdlib.compare
      (List.filter
         (fun f -> Filename.extension f = ".liq")
         (Array.to_list (Sys.readdir location)))
  in
  Printf.printf
    {|
(install
 (section
  (site
   (liquidsoap-lang libs)))
 (package liquidsoap-libs)
 (files
    %s))
  |}
    (String.concat "\n" libs)
