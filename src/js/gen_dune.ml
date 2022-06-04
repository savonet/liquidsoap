let () =
  let location = Filename.dirname Sys.executable_name in
  let location = Filename.concat location ".." in
  let location = Filename.concat location "libs" in
  let libs =
    List.filter
      (fun f -> Filename.extension f = ".liq")
      (Array.to_list (Sys.readdir location))
  in
  Printf.printf
    {|
(rule
 (target filesystem.js)
 (deps (source_tree ../libs))
 (action
  (run js_of_ocaml build-fs -I ../libs -o %%{target} %s)))
  |}
    (String.concat "\n" libs)
