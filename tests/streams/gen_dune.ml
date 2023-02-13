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
 (alias citest)
 (package liquidsoap)
 (deps
  %s
  ./file1.mp3
  ./file2.mp3
  ./file3.mp3
  ./jingle1.mp3
  ./jingle2.mp3
  ./jingle3.mp3
  ./file1.png
  ./file2.png
  ./jingles
  ./playlist
  ./huge_playlist
  ../media/all_media_files
  ../../src/bin/liquidsoap.exe
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %%{run_test} %s liquidsoap %%{test_liq} %s)))
  |}
        test test test)
    tests
