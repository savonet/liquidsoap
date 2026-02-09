let all_tests = ref []

let extra_deps =
  [("GH4882", {|
  (glob_files ./*.dylib)
  (glob_files ./*.so)|})]

let () =
  let location = Sys.getcwd () in
  let tests =
    List.filter
      (fun f -> Filename.extension f = ".liq")
      (Build_tools.read_files ~location "")
  in
  List.iter
    (fun test ->
      let target = Filename.remove_extension test in
      all_tests := Printf.sprintf "(alias %s)" target :: !all_tests;
      let extra_deps =
        List.assoc_opt target extra_deps |> Option.value ~default:""
      in
      Printf.printf
        {|
(rule
 (alias %s)
 (package liquidsoap)
 (deps
  %s
  ../media/all_media_files
  ../../src/bin/liquidsoap.exe
  ../streams/file1.png
  ../streams/file1.mp3
  ./theora-test.mp4%s
  (package liquidsoap)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %%{run_test} %s liquidsoap %%{test_liq} %s)))
  |}
        target test extra_deps test test)
    tests

let () =
  Printf.printf
    {|(alias
  (name citest)
  (deps
    %s))
|}
    (String.concat "\n    " (List.sort_uniq Stdlib.compare !all_tests))
