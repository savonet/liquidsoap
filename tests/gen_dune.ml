let test_names = ref []

let test_name s =
  let base = Filename.remove_extension s in
  let test_name = "test_" ^ base in
  test_names := Printf.sprintf "(alias %s)" test_name :: !test_names;
  test_name

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
 (alias %s)
 (package liquidsoap)
 (deps
  %s
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (source_tree ../../src/libs)
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
