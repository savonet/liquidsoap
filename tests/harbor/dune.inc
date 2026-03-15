(rule
 (alias citest)
 (package liquidsoap)
 (deps
  test_http.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} test_http.liq liquidsoap %{test_liq} test_http.liq)))

(rule
 (alias citest)
 (package liquidsoap)
 (deps
  test_http2.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} test_http2.liq liquidsoap %{test_liq} test_http2.liq)))

(rule
 (alias citest)
 (package liquidsoap)
 (deps
  test_http3.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} test_http3.liq liquidsoap %{test_liq} test_http3.liq)))

(rule
 (alias citest)
 (package liquidsoap)
 (deps
  test_post.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} test_post.liq liquidsoap %{test_liq} test_post.liq)))

(rule
 (alias citest)
 (package liquidsoap)
 (deps
  test_put.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} test_put.liq liquidsoap %{test_liq} test_put.liq)))
