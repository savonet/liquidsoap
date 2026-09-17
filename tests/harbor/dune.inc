(rule
 (alias test_harbor_output_auth)
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
 (alias test_harbor_output_burst)
 (package liquidsoap)
 (deps
  harbor_output_burst.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run
   %{run_test}
   harbor_output_burst.liq
   liquidsoap
   %{test_liq}
   harbor_output_burst.liq)))

(rule
 (alias test_harbor_output_burst_null)
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
 (alias test_harbor_output_dedicated_encoder)
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
 (alias test_harbor_output_icy_metaint)
 (package liquidsoap)
 (deps
  harbor_output_icy_metaint.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run
   %{run_test}
   harbor_output_icy_metaint.liq
   liquidsoap
   %{test_liq}
   harbor_output_icy_metaint.liq)))

(rule
 (alias test_harbor_output_listener_lifecycle)
 (package liquidsoap)
 (deps
  harbor_output_listener_lifecycle.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run
   %{run_test}
   harbor_output_listener_lifecycle.liq
   liquidsoap
   %{test_liq}
   harbor_output_listener_lifecycle.liq)))

(rule
 (alias test_harbor_output_many_listeners)
 (package liquidsoap)
 (deps
  harbor_output_many_listeners.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run
   %{run_test}
   harbor_output_many_listeners.liq
   liquidsoap
   %{test_liq}
   harbor_output_many_listeners.liq)))

(rule
 (alias test_harbor_output_on_connect_disconnect)
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
 (alias test_http)
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
 (alias test_http2)
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
 (alias test_http3)
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
 (alias test_icecast_server_logs)
 (package liquidsoap)
 (deps
  icecast_server_logs.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run
   %{run_test}
   icecast_server_logs.liq
   liquidsoap
   %{test_liq}
   icecast_server_logs.liq)))

(rule
 (alias test_post)
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
 (alias test_put)
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

(alias
 (name citest)
 (deps
  (alias test_harbor_output_auth)
  (alias test_harbor_output_burst)
  (alias test_harbor_output_burst_null)
  (alias test_harbor_output_dedicated_encoder)
  (alias test_harbor_output_icy_metaint)
  (alias test_harbor_output_listener_lifecycle)
  (alias test_harbor_output_many_listeners)
  (alias test_harbor_output_on_connect_disconnect)
  (alias test_http)
  (alias test_http2)
  (alias test_http3)
  (alias test_icecast_server_logs)
  (alias test_post)
  (alias test_put)))
