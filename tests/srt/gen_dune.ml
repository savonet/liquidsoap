let test_names = ref []

let () =
  let location = Sys.getcwd () in
  let tests =
    List.filter
      (fun f -> Filename.extension f = ".liq")
      (Build_tools.read_files ~location "")
  in
  List.iter
    (fun test ->
      let name = "test_" ^ Filename.remove_extension test in
      test_names := Printf.sprintf "(alias %s)" name :: !test_names;
      Printf.printf
        {|
(rule
 (alias %s)
 (package liquidsoap)
 (deps
  %s
  ./srt_peer.exe
  ./helpers.inc
  ./tone.ts
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  ../test.liq
  (:run_test ../run_test.exe))
 (action (run %%{run_test} %s liquidsoap %s)))
  |}
        name test test test)
    tests

let () =
  Printf.printf
    {|(alias
  (name citest)
  (deps
    %s))
|}
    (String.concat "\n    " (List.sort_uniq Stdlib.compare !test_names))
