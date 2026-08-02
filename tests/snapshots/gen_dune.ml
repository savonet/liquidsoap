let () =
  let location = Sys.getcwd () in
  let cases =
    List.filter
      (fun f -> Filename.extension f = ".liq")
      (Build_tools.read_files ~location "cases")
  in
  List.iter
    (fun case ->
      let name = Filename.remove_extension case in
      Printf.printf
        {|
(rule
 (target %s.actual)
 (deps
  (:case cases/%s)
  (:snapshot snapshot.exe))
 (action
  (with-stdout-to %%{target} (run %%{snapshot} %%{case}))))

(rule
 (aliases lang_snapshot runtest)
 (package liquidsoap)
 (action
  (diff expected/%s.expected %s.actual)))
|}
        name case name name)
    cases
