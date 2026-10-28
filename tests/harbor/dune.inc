(rule
 (alias test_http)
 (package liquidsoap)
 (deps
  http.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (source_tree ../../src/libs)
  ../test.liq
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} http.liq liquidsoap http.liq)))

(rule
 (alias test_http2)
 (package liquidsoap)
 (deps
  http2.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (source_tree ../../src/libs)
  ../test.liq
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} http2.liq liquidsoap http2.liq)))

(rule
 (alias test_http3)
 (package liquidsoap)
 (deps
  http3.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (source_tree ../../src/libs)
  ../test.liq
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} http3.liq liquidsoap http3.liq)))

(rule
 (alias test_icecast_server_fallback)
 (package liquidsoap)
 (deps
  icecast_server_fallback.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (source_tree ../../src/libs)
  ../test.liq
  (:run_test ../run_test.exe))
 (action
  (run
   %{run_test}
   icecast_server_fallback.liq
   liquidsoap
   icecast_server_fallback.liq)))

(rule
 (alias test_icecast_server_logs)
 (package liquidsoap)
 (deps
  icecast_server_logs.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (source_tree ../../src/libs)
  ../test.liq
  (:run_test ../run_test.exe))
 (action
  (run
   %{run_test}
   icecast_server_logs.liq
   liquidsoap
   icecast_server_logs.liq)))

(rule
 (alias test_icecast_server_ogg_late_listener)
 (package liquidsoap)
 (deps
  icecast_server_ogg_late_listener.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (source_tree ../../src/libs)
  ../test.liq
  (:run_test ../run_test.exe))
 (action
  (run
   %{run_test}
   icecast_server_ogg_late_listener.liq
   liquidsoap
   icecast_server_ogg_late_listener.liq)))

(rule
 (alias test_icecast_server_reload)
 (package liquidsoap)
 (deps
  icecast_server_reload.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (source_tree ../../src/libs)
  ../test.liq
  (:run_test ../run_test.exe))
 (action
  (run
   %{run_test}
   icecast_server_reload.liq
   liquidsoap
   icecast_server_reload.liq)))

(rule
 (alias test_icecast_server_shutdown)
 (package liquidsoap)
 (deps
  icecast_server_shutdown.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (source_tree ../../src/libs)
  ../test.liq
  (:run_test ../run_test.exe))
 (action
  (run
   %{run_test}
   icecast_server_shutdown.liq
   liquidsoap
   icecast_server_shutdown.liq)))

(rule
 (alias test_post)
 (package liquidsoap)
 (deps
  post.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (source_tree ../../src/libs)
  ../test.liq
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} post.liq liquidsoap post.liq)))

(rule
 (alias test_put)
 (package liquidsoap)
 (deps
  put.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (source_tree ../../src/libs)
  ../test.liq
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} put.liq liquidsoap put.liq)))

(alias
 (name citest)
 (deps
  (alias test_http)
  (alias test_http2)
  (alias test_http3)
  (alias test_icecast_server_fallback)
  (alias test_icecast_server_logs)
  (alias test_icecast_server_ogg_late_listener)
  (alias test_icecast_server_reload)
  (alias test_icecast_server_shutdown)
  (alias test_post)
  (alias test_put)))
