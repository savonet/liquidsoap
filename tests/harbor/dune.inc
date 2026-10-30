(rule
 (alias test_harbor_http_remove)
 (package liquidsoap)
 (deps
  harbor_http_remove.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (source_tree ../../src/libs)
  ../test.liq
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} harbor_http_remove.liq liquidsoap harbor_http_remove.liq)))

(rule
 (alias test_harbor_icy_port)
 (package liquidsoap)
 (deps
  harbor_icy_port.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (source_tree ../../src/libs)
  ../test.liq
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} harbor_icy_port.liq liquidsoap harbor_icy_port.liq)))

(rule
 (alias test_harbor_output_accept)
 (package liquidsoap)
 (deps
  harbor_output_accept.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (source_tree ../../src/libs)
  ../test.liq
  (:run_test ../run_test.exe))
 (action
  (run
   %{run_test}
   harbor_output_accept.liq
   liquidsoap
   harbor_output_accept.liq)))

(rule
 (alias test_harbor_output_auth)
 (package liquidsoap)
 (deps
  harbor_output_auth.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (source_tree ../../src/libs)
  ../test.liq
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} harbor_output_auth.liq liquidsoap harbor_output_auth.liq)))

(rule
 (alias test_harbor_output_burst)
 (package liquidsoap)
 (deps
  harbor_output_burst.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (source_tree ../../src/libs)
  ../test.liq
  (:run_test ../run_test.exe))
 (action
  (run
   %{run_test}
   harbor_output_burst.liq
   liquidsoap
   harbor_output_burst.liq)))

(rule
 (alias test_harbor_output_clock)
 (package liquidsoap)
 (deps
  harbor_output_clock.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (source_tree ../../src/libs)
  ../test.liq
  (:run_test ../run_test.exe))
 (action
  (run
   %{run_test}
   harbor_output_clock.liq
   liquidsoap
   harbor_output_clock.liq)))

(rule
 (alias test_harbor_output_construction)
 (package liquidsoap)
 (deps
  harbor_output_construction.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (source_tree ../../src/libs)
  ../test.liq
  (:run_test ../run_test.exe))
 (action
  (run
   %{run_test}
   harbor_output_construction.liq
   liquidsoap
   harbor_output_construction.liq)))

(rule
 (alias test_harbor_output_dedicated_encoder)
 (package liquidsoap)
 (deps
  harbor_output_dedicated_encoder.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (source_tree ../../src/libs)
  ../test.liq
  (:run_test ../run_test.exe))
 (action
  (run
   %{run_test}
   harbor_output_dedicated_encoder.liq
   liquidsoap
   harbor_output_dedicated_encoder.liq)))

(rule
 (alias test_harbor_output_final_bytes)
 (package liquidsoap)
 (deps
  harbor_output_final_bytes.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (source_tree ../../src/libs)
  ../test.liq
  (:run_test ../run_test.exe))
 (action
  (run
   %{run_test}
   harbor_output_final_bytes.liq
   liquidsoap
   harbor_output_final_bytes.liq)))

(rule
 (alias test_harbor_output_icy_listeners)
 (package liquidsoap)
 (deps
  harbor_output_icy_listeners.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (source_tree ../../src/libs)
  ../test.liq
  (:run_test ../run_test.exe))
 (action
  (run
   %{run_test}
   harbor_output_icy_listeners.liq
   liquidsoap
   harbor_output_icy_listeners.liq)))

(rule
 (alias test_harbor_output_icy_metaint)
 (package liquidsoap)
 (deps
  harbor_output_icy_metaint.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (source_tree ../../src/libs)
  ../test.liq
  (:run_test ../run_test.exe))
 (action
  (run
   %{run_test}
   harbor_output_icy_metaint.liq
   liquidsoap
   harbor_output_icy_metaint.liq)))

(rule
 (alias test_harbor_output_icy_stamping)
 (package liquidsoap)
 (deps
  harbor_output_icy_stamping.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (source_tree ../../src/libs)
  ../test.liq
  (:run_test ../run_test.exe))
 (action
  (run
   %{run_test}
   harbor_output_icy_stamping.liq
   liquidsoap
   harbor_output_icy_stamping.liq)))

(rule
 (alias test_harbor_output_idle_cost)
 (package liquidsoap)
 (deps
  harbor_output_idle_cost.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (source_tree ../../src/libs)
  ../test.liq
  (:run_test ../run_test.exe))
 (action
  (run
   %{run_test}
   harbor_output_idle_cost.liq
   liquidsoap
   harbor_output_idle_cost.liq)))

(rule
 (alias test_harbor_output_join_points)
 (package liquidsoap)
 (deps
  harbor_output_join_points.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (source_tree ../../src/libs)
  ../test.liq
  (:run_test ../run_test.exe))
 (action
  (run
   %{run_test}
   harbor_output_join_points.liq
   liquidsoap
   harbor_output_join_points.liq)))

(rule
 (alias test_harbor_output_large_output)
 (package liquidsoap)
 (deps
  harbor_output_large_output.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (source_tree ../../src/libs)
  ../test.liq
  (:run_test ../run_test.exe))
 (action
  (run
   %{run_test}
   harbor_output_large_output.liq
   liquidsoap
   harbor_output_large_output.liq)))

(rule
 (alias test_harbor_output_listener_lifecycle)
 (package liquidsoap)
 (deps
  harbor_output_listener_lifecycle.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (source_tree ../../src/libs)
  ../test.liq
  (:run_test ../run_test.exe))
 (action
  (run
   %{run_test}
   harbor_output_listener_lifecycle.liq
   liquidsoap
   harbor_output_listener_lifecycle.liq)))

(rule
 (alias test_harbor_output_many_listeners)
 (package liquidsoap)
 (deps
  harbor_output_many_listeners.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (source_tree ../../src/libs)
  ../test.liq
  (:run_test ../run_test.exe))
 (action
  (run
   %{run_test}
   harbor_output_many_listeners.liq
   liquidsoap
   harbor_output_many_listeners.liq)))

(rule
 (alias test_harbor_output_on_connect_disconnect)
 (package liquidsoap)
 (deps
  harbor_output_on_connect_disconnect.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (source_tree ../../src/libs)
  ../test.liq
  (:run_test ../run_test.exe))
 (action
  (run
   %{run_test}
   harbor_output_on_connect_disconnect.liq
   liquidsoap
   harbor_output_on_connect_disconnect.liq)))

(rule
 (alias test_harbor_output_overflow)
 (package liquidsoap)
 (deps
  harbor_output_overflow.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (source_tree ../../src/libs)
  ../test.liq
  (:run_test ../run_test.exe))
 (action
  (run
   %{run_test}
   harbor_output_overflow.liq
   liquidsoap
   harbor_output_overflow.liq)))

(rule
 (alias test_harbor_output_preroll)
 (package liquidsoap)
 (deps
  harbor_output_preroll.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (source_tree ../../src/libs)
  ../test.liq
  (:run_test ../run_test.exe))
 (action
  (run
   %{run_test}
   harbor_output_preroll.liq
   liquidsoap
   harbor_output_preroll.liq)))

(rule
 (alias test_harbor_output_routing)
 (package liquidsoap)
 (deps
  harbor_output_routing.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (source_tree ../../src/libs)
  ../test.liq
  (:run_test ../run_test.exe))
 (action
  (run
   %{run_test}
   harbor_output_routing.liq
   liquidsoap
   harbor_output_routing.liq)))

(rule
 (alias test_harbor_output_shutdown)
 (package liquidsoap)
 (deps
  harbor_output_shutdown.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (source_tree ../../src/libs)
  ../test.liq
  (:run_test ../run_test.exe))
 (action
  (run
   %{run_test}
   harbor_output_shutdown.liq
   liquidsoap
   harbor_output_shutdown.liq)))

(rule
 (alias test_harbor_output_stop_during_admission)
 (package liquidsoap)
 (deps
  harbor_output_stop_during_admission.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (source_tree ../../src/libs)
  ../test.liq
  (:run_test ../run_test.exe))
 (action
  (run
   %{run_test}
   harbor_output_stop_during_admission.liq
   liquidsoap
   harbor_output_stop_during_admission.liq)))

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
 (alias test_icecast_server_listeners)
 (package liquidsoap)
 (deps
  icecast_server_listeners.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (source_tree ../../src/libs)
  ../test.liq
  (:run_test ../run_test.exe))
 (action
  (run
   %{run_test}
   icecast_server_listeners.liq
   liquidsoap
   icecast_server_listeners.liq)))

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
 (alias test_icecast_server_preroll)
 (package liquidsoap)
 (deps
  icecast_server_preroll.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (source_tree ../../src/libs)
  ../test.liq
  (:run_test ../run_test.exe))
 (action
  (run
   %{run_test}
   icecast_server_preroll.liq
   liquidsoap
   icecast_server_preroll.liq)))

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
 (alias test_icecast_server_source_reconnect)
 (package liquidsoap)
 (deps
  icecast_server_source_reconnect.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (source_tree ../../src/libs)
  ../test.liq
  (:run_test ../run_test.exe))
 (action
  (run
   %{run_test}
   icecast_server_source_reconnect.liq
   liquidsoap
   icecast_server_source_reconnect.liq)))

(rule
 (alias test_icecast_server_url_auth)
 (package liquidsoap)
 (deps
  icecast_server_url_auth.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (source_tree ../../src/libs)
  ../test.liq
  (:run_test ../run_test.exe))
 (action
  (run
   %{run_test}
   icecast_server_url_auth.liq
   liquidsoap
   icecast_server_url_auth.liq)))

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
  (alias test_harbor_http_remove)
  (alias test_harbor_icy_port)
  (alias test_harbor_output_accept)
  (alias test_harbor_output_auth)
  (alias test_harbor_output_burst)
  (alias test_harbor_output_clock)
  (alias test_harbor_output_construction)
  (alias test_harbor_output_dedicated_encoder)
  (alias test_harbor_output_final_bytes)
  (alias test_harbor_output_icy_listeners)
  (alias test_harbor_output_icy_metaint)
  (alias test_harbor_output_icy_stamping)
  (alias test_harbor_output_idle_cost)
  (alias test_harbor_output_join_points)
  (alias test_harbor_output_large_output)
  (alias test_harbor_output_listener_lifecycle)
  (alias test_harbor_output_many_listeners)
  (alias test_harbor_output_on_connect_disconnect)
  (alias test_harbor_output_overflow)
  (alias test_harbor_output_preroll)
  (alias test_harbor_output_routing)
  (alias test_harbor_output_shutdown)
  (alias test_harbor_output_stop_during_admission)
  (alias test_http)
  (alias test_http2)
  (alias test_http3)
  (alias test_icecast_server_fallback)
  (alias test_icecast_server_listeners)
  (alias test_icecast_server_logs)
  (alias test_icecast_server_ogg_late_listener)
  (alias test_icecast_server_preroll)
  (alias test_icecast_server_reload)
  (alias test_icecast_server_shutdown)
  (alias test_icecast_server_source_reconnect)
  (alias test_icecast_server_url_auth)
  (alias test_post)
  (alias test_put)))
