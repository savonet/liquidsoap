let () =
  let location = Sys.getcwd () in
  let tests =
    List.filter
      (fun f -> Filename.extension f = ".liq")
      (Build_tools.read_files ~location "")
  in
  List.iter
    (fun test ->
      Printf.printf
        {|
(rule
 (alias citest)
 (package liquidsoap)
 (deps
  %s
  ../media/all_media_files
  ../../src/bin/liquidsoap.exe
  ../streams/file1.png
  ../streams/file1.mp3
  (package liquidsoap)
  (:stdlib ../../src/libs/stdlib.liq)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %%{run_test} %s liquidsoap --stdlib %%{stdlib} %%{test_liq} %s)))
  |}
        test test test)
    tests;

  let output_tests =
    List.filter
      (fun f -> Filename.extension f = ".output")
      (Build_tools.read_files ~location "")
  in
  List.iter
    (fun test ->
      Printf.printf
        {|
(rule
 (alias citest)
 (package liquidsoap)
 (deps
  %s
  (:check_output ../check_output.exe)
  (:run_test ../run_test.exe))
 (action (run %%{run_test} %s %%{check_output})))
  |}
        test test)
    output_tests
