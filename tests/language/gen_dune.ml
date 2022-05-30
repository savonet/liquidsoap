let () =
  let location = Filename.dirname Sys.executable_name in
  let tests =
    List.filter
      (fun f -> Filename.extension f = ".liq")
      (Array.to_list (Sys.readdir location))
  in
  let runtests =
    List.map
      (Printf.sprintf "(run %%{run_test} \"%%{liquidsoap} %%{test_liq} -\" %s)")
      tests
  in
  Printf.printf
    {|
(rule
 (alias runtest)
 (package liquidsoap)
 (deps
  %s
  (:liquidsoap ../../src/bin/liquidsoap.exe)
  (:test_liq ../test.liq)
  (:run_test ../run_test.sh))
 (action
  (progn
    %s)))
  |}
    (String.concat "\n" tests)
    (String.concat "\n" runtests)
