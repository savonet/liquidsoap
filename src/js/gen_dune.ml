let () =
  let location = Filename.dirname Sys.executable_name in
  let location = Filename.concat location ".." in
  let location = Filename.concat location "libs" in
  let libs =
    List.filter
      (fun f -> Filename.extension f = ".liq")
      (Build_tools.read_files ~location "")
  in
  Printf.printf
    {|
(rule
  (target filesystem.js)
  (enabled_if (not %%{bin-available:js_of_ocaml}))
  (action (write-file %%{target} "console.log(\"no js_of_ocaml available!\")")))

(rule
 (target filesystem.js)
 (enabled_if %%{bin-available:js_of_ocaml})
 (deps
  (:stdlib_js ./stdlib_js.liq)
  (source_tree ../libs))
 (action
  (run js_of_ocaml build-fs -I . -I ../libs -o %%{target} %%{stdlib_js} %s)))
  |}
    (String.concat "\n" libs)
