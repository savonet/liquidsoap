let () =
  let location = Sys.getcwd () in
  let tests =
    List.filter
      (fun f -> Filename.extension f = ".liq")
      (Array.to_list (Sys.readdir location))
  in
  List.iter
    (fun test ->
      Printf.printf
        {|
(rule
 (alias runtest)
 (package liquidsoap)
 (deps
  %s
  ../media/all_media_files
  (:liquidsoap ../../src/bin/liquidsoap.exe)
  (:test_liq ../test.liq)
  (:run_test ../run_test.sh))
 (action (run %%{run_test} "%%{liquidsoap} %%{test_liq} -" %s)))
  |}
        test test)
    tests;

  let output_tests =
    List.filter
      (fun f -> Filename.extension f = ".output")
      (Array.to_list (Sys.readdir location))
  in
  List.iter
    (fun test ->
      Printf.printf
        {|
(rule
 (alias runtest)
 (package liquidsoap)
 (deps
  %s
  (:check_output ../check_output.exe)
  (:run_test ../run_test.sh))
 (action (run %%{run_test} %%{check_output} %s)))
  |}
        test test)
    output_tests
