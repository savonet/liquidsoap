let () =
  let location = Filename.dirname Sys.executable_name in
  let tests =
    List.filter
      (fun f -> Filename.extension f = ".liq")
      (Array.to_list (Sys.readdir location))
  in
  let runtests =
    List.map
      (Printf.sprintf
         "(run %%{run_test} \"%%{liquidsoap} --no-stdlib %%{stdlib} \
          %%{test_liq} -\" %s)")
      tests
  in
  Printf.printf
    {|
(rule
 (alias runtest)
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
  (:liquidsoap ../../src/bin/liquidsoap.exe)
  (source_tree ../libs)
  (:stdlib ../../libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.sh))
 (action
  (progn
    %s)))
  |}
    (String.concat "\n" tests)
    (String.concat "\n" runtests)
