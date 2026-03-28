let all_tests = ref []

let extra_deps =
  [
    ("GH4882", {|
  (glob_files ./*.dylib)
  (glob_files ./*.so)|});
    ( "GH4995",
      {|
  ../streams/playlist
  ../streams/file2.mp3
  ../streams/file3.mp3|}
    );
  ]

let extra_targets = [("GH5019", "GH5019.wav")]

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
      let alias = "test_" ^ target in
      all_tests := Printf.sprintf "(alias %s)" alias :: !all_tests;
      let extra_deps =
        List.assoc_opt target extra_deps |> Option.value ~default:""
      in
      let targets_stanza =
        match List.assoc_opt target extra_targets with
          | None -> ""
          | Some t -> Printf.sprintf "\n (targets %s)" t
      in
      Printf.printf
        {|
(rule
 (alias %s)
 (package liquidsoap)%s
 (deps
  %s
  (alias ../media/all_media_files)
  ../liquidsoap-test-assets
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
        alias targets_stanza test extra_deps target test)
    tests

let () =
  Printf.printf
    {|(alias
  (name citest)
  (deps
    %s))
|}
    (String.concat "\n    " (List.sort_uniq Stdlib.compare !all_tests))
