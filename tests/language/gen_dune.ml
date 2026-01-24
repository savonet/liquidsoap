let test_names = ref []

let test_name s =
  let test_name = Filename.remove_extension s in
  test_names := Printf.sprintf "(alias %s)" test_name :: !test_names;
  test_name

let excluded_files = ["write_cover.liq"; "test_cover.liq"]

let () =
  let location = Sys.getcwd () in
  let tests =
    List.filter
      (fun f ->
        Filename.extension f = ".liq" && not (List.mem f excluded_files))
      (Build_tools.read_files ~location "")
  in
  List.iter
    (fun test ->
      Printf.printf
        {|
(rule
 (alias %s)
 (package liquidsoap)
 (deps
  %s
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (source_tree ../../src/libs)
  crontab_test_cases.json
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %%{run_test} %s liquidsoap %%{test_liq} %s)))
  |}
        (test_name test) test test test)
    tests

let () =
  Printf.printf
    {|(alias
  (name citest)
  (deps
    %s))
|}
    (String.concat "\n    " (List.sort_uniq Stdlib.compare !test_names))
