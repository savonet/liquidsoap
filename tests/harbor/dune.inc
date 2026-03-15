(rule
 (alias citest)
 (package liquidsoap)
 (deps
  harbor_output_auth.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run
   %{run_test}
   harbor_output_auth.liq
   liquidsoap
   %{test_liq}
   harbor_output_auth.liq)))

(rule
 (alias citest)
 (package liquidsoap)
 (deps
  harbor_output_burst_null.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run
   %{run_test}
   harbor_output_burst_null.liq
   liquidsoap
   %{test_liq}
   harbor_output_burst_null.liq)))

(rule
 (alias citest)
 (package liquidsoap)
 (deps
  harbor_output_dedicated_encoder.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run
   %{run_test}
   harbor_output_dedicated_encoder.liq
   liquidsoap
   %{test_liq}
   harbor_output_dedicated_encoder.liq)))

(rule
 (alias citest)
 (package liquidsoap)
 (deps
  harbor_output_on_connect_disconnect.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run
   %{run_test}
   harbor_output_on_connect_disconnect.liq
   liquidsoap
   %{test_liq}
   harbor_output_on_connect_disconnect.liq)))

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
