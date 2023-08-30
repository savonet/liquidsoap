let static_tests =
  [
    "icecast_ssl.liq";
    "icecast_tls.liq";
    "icecast_tls_ssl.liq";
    "icecast_ssl_tls.liq";
  ]

let () =
  let location = Sys.getcwd () in
  let tests =
    List.filter
      (fun f ->
        (not (List.mem (Filename.basename f) static_tests))
        && Filename.extension f = ".liq")
      (Build_tools.read_files ~location "")
  in
  List.iter
    (fun test ->
      Printf.printf
        {|
(rule
  (alias tree-sitter-test)
  (deps
   %s
   (source_tree ../../src/tooling/tree-sitter-liquidsoap)
   ../../src/tooling/tree-sitter-liquidsoap/node_modules
 )
  (action
    (chdir ../../src/tooling/tree-sitter-liquidsoap
     (ignore-stdout
      (run npm exec tree-sitter -- parse ../../../tests/streams/%s)))))

(rule
  (alias fmt)
  (deps %s ../../src/tooling/liq-prettier ../../src/tooling/prettier-plugin-liquidsoap/dist/liquidsoap.js)
  (action
    (progn
      (with-stdout-to %s.prettier
       (chdir ../../src/tooling/liq-prettier
         (run pnpm prettier --config ./config.json ../../../tests/streams/%s)))
      (diff %s %s.prettier))))

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
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %%{run_test} %s liquidsoap %%{test_liq} %s)))
  |}
        test test test test test test test test test test)
    tests
