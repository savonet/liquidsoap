
(rule
 (alias citest)
 (package liquidsoap)
 (deps
  GH1159.liq
  ../media/all_media_files
  ../../src/bin/liquidsoap.exe
  (source_tree ../../src/libs)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} GH1159.liq liquidsoap %{test_liq} GH1159.liq)))

(rule
 (alias citest)
 (package liquidsoap)
 (deps
  LS460.liq
  ../media/all_media_files
  ../../src/bin/liquidsoap.exe
  (source_tree ../../src/libs)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} LS460.liq liquidsoap %{test_liq} LS460.liq)))

(rule
 (alias citest)
 (package liquidsoap)
 (deps
  LS503.liq
  ../media/all_media_files
  ../../src/bin/liquidsoap.exe
  (source_tree ../../src/libs)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} LS503.liq liquidsoap %{test_liq} LS503.liq)))

(rule
 (alias citest)
 (package liquidsoap)
 (deps
  115-1.liq
  ../media/all_media_files
  ../../src/bin/liquidsoap.exe
  (source_tree ../../src/libs)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} 115-1.liq liquidsoap %{test_liq} 115-1.liq)))

(rule
 (alias citest)
 (package liquidsoap)
 (deps
  GH2867.liq
  ../media/all_media_files
  ../../src/bin/liquidsoap.exe
  (source_tree ../../src/libs)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} GH2867.liq liquidsoap %{test_liq} GH2867.liq)))

(rule
 (alias citest)
 (package liquidsoap)
 (deps
  GH2872.liq
  ../media/all_media_files
  ../../src/bin/liquidsoap.exe
  (source_tree ../../src/libs)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} GH2872.liq liquidsoap %{test_liq} GH2872.liq)))

(rule
 (alias citest)
 (package liquidsoap)
 (deps
  115-2.liq
  ../media/all_media_files
  ../../src/bin/liquidsoap.exe
  (source_tree ../../src/libs)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} 115-2.liq liquidsoap %{test_liq} 115-2.liq)))

(rule
 (alias citest)
 (package liquidsoap)
 (deps
  GH2871.liq
  ../media/all_media_files
  ../../src/bin/liquidsoap.exe
  (source_tree ../../src/libs)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} GH2871.liq liquidsoap %{test_liq} GH2871.liq)))

(rule
 (alias citest)
 (package liquidsoap)
 (deps
  123.liq
  ../media/all_media_files
  ../../src/bin/liquidsoap.exe
  (source_tree ../../src/libs)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} 123.liq liquidsoap %{test_liq} 123.liq)))

(rule
 (alias citest)
 (package liquidsoap)
 (deps
  LS354-2.liq
  ../media/all_media_files
  ../../src/bin/liquidsoap.exe
  (source_tree ../../src/libs)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} LS354-2.liq liquidsoap %{test_liq} LS354-2.liq)))

(rule
 (alias citest)
 (package liquidsoap)
 (deps
  GH2585.liq
  ../media/all_media_files
  ../../src/bin/liquidsoap.exe
  (source_tree ../../src/libs)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} GH2585.liq liquidsoap %{test_liq} GH2585.liq)))

(rule
 (alias citest)
 (package liquidsoap)
 (deps
  init-error.liq
  ../media/all_media_files
  ../../src/bin/liquidsoap.exe
  (source_tree ../../src/libs)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} init-error.liq liquidsoap %{test_liq} init-error.liq)))

(rule
 (alias citest)
 (package liquidsoap)
 (deps
  LS354-1.liq
  ../media/all_media_files
  ../../src/bin/liquidsoap.exe
  (source_tree ../../src/libs)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} LS354-1.liq liquidsoap %{test_liq} LS354-1.liq)))

(rule
 (alias citest)
 (package liquidsoap)
 (deps
  GH1129.liq
  ../media/all_media_files
  ../../src/bin/liquidsoap.exe
  (source_tree ../../src/libs)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} GH1129.liq liquidsoap %{test_liq} GH1129.liq)))

(rule
 (alias citest)
 (package liquidsoap)
 (deps
  metadata_cache.liq
  ../media/all_media_files
  ../../src/bin/liquidsoap.exe
  (source_tree ../../src/libs)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} metadata_cache.liq liquidsoap %{test_liq} metadata_cache.liq)))

(rule
 (alias citest)
 (package liquidsoap)
 (deps
  GH2758.liq
  ../media/all_media_files
  ../../src/bin/liquidsoap.exe
  (source_tree ../../src/libs)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} GH2758.liq liquidsoap %{test_liq} GH2758.liq)))

(rule
 (alias citest)
 (package liquidsoap)
 (deps
  GH1279.liq
  ../media/all_media_files
  ../../src/bin/liquidsoap.exe
  (source_tree ../../src/libs)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} GH1279.liq liquidsoap %{test_liq} GH1279.liq)))

(rule
 (alias citest)
 (package liquidsoap)
 (deps
  GH1327.liq
  ../media/all_media_files
  ../../src/bin/liquidsoap.exe
  (source_tree ../../src/libs)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} GH1327.liq liquidsoap %{test_liq} GH1327.liq)))

(rule
 (alias citest)
 (package liquidsoap)
 (deps
  BUG403.liq
  ../media/all_media_files
  ../../src/bin/liquidsoap.exe
  (source_tree ../../src/libs)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} BUG403.liq liquidsoap %{test_liq} BUG403.liq)))

(rule
 (alias citest)
 (package liquidsoap)
 (deps
  GH2602.liq
  ../media/all_media_files
  ../../src/bin/liquidsoap.exe
  (source_tree ../../src/libs)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} GH2602.liq liquidsoap %{test_liq} GH2602.liq)))

(rule
 (alias citest)
 (package liquidsoap)
 (deps
  input_rtmp.liq
  ../media/all_media_files
  ../../src/bin/liquidsoap.exe
  (source_tree ../../src/libs)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} input_rtmp.liq liquidsoap %{test_liq} input_rtmp.liq)))

(rule
 (alias citest)
 (package liquidsoap)
 (deps
  infallible-shutdown.liq
  ../media/all_media_files
  ../../src/bin/liquidsoap.exe
  (source_tree ../../src/libs)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} infallible-shutdown.liq liquidsoap %{test_liq} infallible-shutdown.liq)))

(rule
 (alias citest)
 (package liquidsoap)
 (deps
  GH-action-919422659.liq
  ../media/all_media_files
  ../../src/bin/liquidsoap.exe
  (source_tree ../../src/libs)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} GH-action-919422659.liq liquidsoap %{test_liq} GH-action-919422659.liq)))

(rule
 (alias citest)
 (package liquidsoap)
 (deps
  GH1151.liq
  ../media/all_media_files
  ../../src/bin/liquidsoap.exe
  (source_tree ../../src/libs)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} GH1151.liq liquidsoap %{test_liq} GH1151.liq)))

(rule
 (alias citest)
 (package liquidsoap)
 (deps
  playlist-id.liq
  ../media/all_media_files
  ../../src/bin/liquidsoap.exe
  (source_tree ../../src/libs)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} playlist-id.liq liquidsoap %{test_liq} playlist-id.liq)))

(rule
 (alias citest)
 (package liquidsoap)
 (deps
  GH1146.liq
  ../media/all_media_files
  ../../src/bin/liquidsoap.exe
  (source_tree ../../src/libs)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} GH1146.liq liquidsoap %{test_liq} GH1146.liq)))

(rule
 (alias citest)
 (package liquidsoap)
 (deps
  AC5109.liq
  ../media/all_media_files
  ../../src/bin/liquidsoap.exe
  (source_tree ../../src/libs)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} AC5109.liq liquidsoap %{test_liq} AC5109.liq)))

(rule
 (alias citest)
 (package liquidsoap)
 (deps
  ffmpeg-copy-input-http.liq
  ../media/all_media_files
  ../../src/bin/liquidsoap.exe
  (source_tree ../../src/libs)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} ffmpeg-copy-input-http.liq liquidsoap %{test_liq} ffmpeg-copy-input-http.liq)))

(rule
 (alias citest)
 (package liquidsoap)
 (deps
  LS268.liq
  ../media/all_media_files
  ../../src/bin/liquidsoap.exe
  (source_tree ../../src/libs)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} LS268.liq liquidsoap %{test_liq} LS268.liq)))

(rule
 (alias citest)
 (package liquidsoap)
 (deps
  GH2850.liq
  ../media/all_media_files
  ../../src/bin/liquidsoap.exe
  (source_tree ../../src/libs)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} GH2850.liq liquidsoap %{test_liq} GH2850.liq)))

(rule
 (alias citest)
 (package liquidsoap)
 (deps
  GH2573.output
  (:check_output ../check_output.exe)
  (:run_test ../run_test.exe))
 (action (run %{run_test} GH2573.output %{check_output})))
