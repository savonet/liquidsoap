let () =
  let location = Filename.dirname Sys.executable_name in
  let libs =
    List.filter
      (fun f -> Filename.extension f = ".liq")
      (Build_tools.read_files ~location "")
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
