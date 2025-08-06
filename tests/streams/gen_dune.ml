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
  ./replaygain_track_gain.mp3
  ./r128_track_gain.mp3
  ./replaygain_r128_track_gain.mp3
  ./replaygain_track_gain.opus
  ./r128_track_gain.opus
  ./replaygain_r128_track_gain.opus
  ./without_replaygain_track_gain.mp3
  ./crossfade-plot.old.txt
  ./crossfade-plot.new.txt
  ./autocue-plot.0.new.txt
  ./autocue-plot.0.old.txt
  ./autocue-plot.1.new.txt
  ./autocue-plot.1.old.txt
  ./autocue-plot.2.new.txt
  ./autocue-plot.2.old.txt
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %%{run_test} %s liquidsoap %%{test_liq} %s)))
  |}
        test test test)
    tests
