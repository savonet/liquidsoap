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
  (alias fmt)
  (deps %s (source_tree ../../src/tooling/prettier) ../../src/tooling/prettier/prettier-plugin-liquidsoap/dist/liquidsoap.js ../../src/tooling/prettier/node_modules)
  (action
    (progn
      (with-stdout-to %s.prettier
       (chdir ../../src/tooling/prettier
         (run pnpm prettier --config ./config.json ../../../tests/regression/%s)))
      (diff %s %s.prettier))))

(rule
 (alias citest)
 (package liquidsoap)
 (deps
  %s
  ../media/all_media_files
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %%{run_test} %s liquidsoap %%{test_liq} %s)))
  |}
        test test test test test test test test)
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
