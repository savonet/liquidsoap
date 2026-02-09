(rule
 (alias citest)
 (package liquidsoap)
 (deps
  http.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} http.liq liquidsoap %{test_liq} http.liq)))

(rule
 (alias citest)
 (package liquidsoap)
 (deps
  http2.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} http2.liq liquidsoap %{test_liq} http2.liq)))

(rule
 (alias citest)
 (package liquidsoap)
 (deps
  http3.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} http3.liq liquidsoap %{test_liq} http3.liq)))

(rule
 (alias citest)
 (package liquidsoap)
 (deps
  post.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} post.liq liquidsoap %{test_liq} post.liq)))

(rule
 (alias citest)
 (package liquidsoap)
 (deps
  put.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} put.liq liquidsoap %{test_liq} put.liq)))
