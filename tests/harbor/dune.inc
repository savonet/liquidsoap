
(rule
 (alias citest)
 (package liquidsoap)
 (deps
  http.liq
  ../media/all_media_files
  ../../src/bin/liquidsoap.exe
  (source_tree ../../src/libs)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} http.liq liquidsoap %{test_liq} http.liq)))

(rule
 (alias citest)
 (package liquidsoap)
 (deps
  put.liq
  ../media/all_media_files
  ../../src/bin/liquidsoap.exe
  (source_tree ../../src/libs)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} put.liq liquidsoap %{test_liq} put.liq)))

(rule
 (alias citest)
 (package liquidsoap)
 (deps
  post.liq
  ../media/all_media_files
  ../../src/bin/liquidsoap.exe
  (source_tree ../../src/libs)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} post.liq liquidsoap %{test_liq} post.liq)))
