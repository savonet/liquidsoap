(rule
 (alias test_srt-attempt-race)
 (package liquidsoap)
 (deps
  srt-attempt-race.liq
  ./srt_peer.exe
  ./helpers.inc
  ./tone.ts
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  ../test.liq
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} srt-attempt-race.liq liquidsoap srt-attempt-race.liq)))

(rule
 (alias test_srt-av-reconnect)
 (package liquidsoap)
 (deps
  srt-av-reconnect.liq
  ./srt_peer.exe
  ./helpers.inc
  ./tone.ts
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  ../test.liq
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} srt-av-reconnect.liq liquidsoap srt-av-reconnect.liq)))

(rule
 (alias test_srt-bind-failure)
 (package liquidsoap)
 (deps
  srt-bind-failure.liq
  ./srt_peer.exe
  ./helpers.inc
  ./tone.ts
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  ../test.liq
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} srt-bind-failure.liq liquidsoap srt-bind-failure.liq)))

(rule
 (alias test_srt-busy-listener)
 (package liquidsoap)
 (deps
  srt-busy-listener.liq
  ./srt_peer.exe
  ./helpers.inc
  ./tone.ts
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  ../test.liq
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} srt-busy-listener.liq liquidsoap srt-busy-listener.liq)))

(rule
 (alias test_srt-callback-registers)
 (package liquidsoap)
 (deps
  srt-callback-registers.liq
  ./srt_peer.exe
  ./helpers.inc
  ./tone.ts
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  ../test.liq
  (:run_test ../run_test.exe))
 (action
  (run
   %{run_test}
   srt-callback-registers.liq
   liquidsoap
   srt-callback-registers.liq)))

(rule
 (alias test_srt-callbacks-reentrant)
 (package liquidsoap)
 (deps
  srt-callbacks-reentrant.liq
  ./srt_peer.exe
  ./helpers.inc
  ./tone.ts
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  ../test.liq
  (:run_test ../run_test.exe))
 (action
  (run
   %{run_test}
   srt-callbacks-reentrant.liq
   liquidsoap
   srt-callbacks-reentrant.liq)))

(rule
 (alias test_srt-caller-reconnect)
 (package liquidsoap)
 (deps
  srt-caller-reconnect.liq
  ./srt_peer.exe
  ./helpers.inc
  ./tone.ts
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  ../test.liq
  (:run_test ../run_test.exe))
 (action
  (run
   %{run_test}
   srt-caller-reconnect.liq
   liquidsoap
   srt-caller-reconnect.liq)))

(rule
 (alias test_srt-client-failure)
 (package liquidsoap)
 (deps
  srt-client-failure.liq
  ./srt_peer.exe
  ./helpers.inc
  ./tone.ts
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  ../test.liq
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} srt-client-failure.liq liquidsoap srt-client-failure.liq)))

(rule
 (alias test_srt-client-limit)
 (package liquidsoap)
 (deps
  srt-client-limit.liq
  ./srt_peer.exe
  ./helpers.inc
  ./tone.ts
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  ../test.liq
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} srt-client-limit.liq liquidsoap srt-client-limit.liq)))

(rule
 (alias test_srt-copy-encoder)
 (package liquidsoap)
 (deps
  srt-copy-encoder.liq
  ./srt_peer.exe
  ./helpers.inc
  ./tone.ts
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  ../test.liq
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} srt-copy-encoder.liq liquidsoap srt-copy-encoder.liq)))

(rule
 (alias test_srt-disconnect-loop)
 (package liquidsoap)
 (deps
  srt-disconnect-loop.liq
  ./srt_peer.exe
  ./helpers.inc
  ./tone.ts
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  ../test.liq
  (:run_test ../run_test.exe))
 (action
  (run
   %{run_test}
   srt-disconnect-loop.liq
   liquidsoap
   srt-disconnect-loop.liq)))

(rule
 (alias test_srt-dual-stack)
 (package liquidsoap)
 (deps
  srt-dual-stack.liq
  ./srt_peer.exe
  ./helpers.inc
  ./tone.ts
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  ../test.liq
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} srt-dual-stack.liq liquidsoap srt-dual-stack.liq)))

(rule
 (alias test_srt-encryption)
 (package liquidsoap)
 (deps
  srt-encryption.liq
  ./srt_peer.exe
  ./helpers.inc
  ./tone.ts
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  ../test.liq
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} srt-encryption.liq liquidsoap srt-encryption.liq)))

(rule
 (alias test_srt-end-of-stream)
 (package liquidsoap)
 (deps
  srt-end-of-stream.liq
  ./srt_peer.exe
  ./helpers.inc
  ./tone.ts
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  ../test.liq
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} srt-end-of-stream.liq liquidsoap srt-end-of-stream.liq)))

(rule
 (alias test_srt-event-pairing)
 (package liquidsoap)
 (deps
  srt-event-pairing.liq
  ./srt_peer.exe
  ./helpers.inc
  ./tone.ts
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  ../test.liq
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} srt-event-pairing.liq liquidsoap srt-event-pairing.liq)))

(rule
 (alias test_srt-interop-ffmpeg)
 (package liquidsoap)
 (deps
  srt-interop-ffmpeg.liq
  ./srt_peer.exe
  ./helpers.inc
  ./tone.ts
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  ../test.liq
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} srt-interop-ffmpeg.liq liquidsoap srt-interop-ffmpeg.liq)))

(rule
 (alias test_srt-late-joiner)
 (package liquidsoap)
 (deps
  srt-late-joiner.liq
  ./srt_peer.exe
  ./helpers.inc
  ./tone.ts
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  ../test.liq
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} srt-late-joiner.liq liquidsoap srt-late-joiner.liq)))

(rule
 (alias test_srt-latency-reset)
 (package liquidsoap)
 (deps
  srt-latency-reset.liq
  ./srt_peer.exe
  ./helpers.inc
  ./tone.ts
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  ../test.liq
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} srt-latency-reset.liq liquidsoap srt-latency-reset.liq)))

(rule
 (alias test_srt-listener-survives)
 (package liquidsoap)
 (deps
  srt-listener-survives.liq
  ./srt_peer.exe
  ./helpers.inc
  ./tone.ts
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  ../test.liq
  (:run_test ../run_test.exe))
 (action
  (run
   %{run_test}
   srt-listener-survives.liq
   liquidsoap
   srt-listener-survives.liq)))

(rule
 (alias test_srt-log-flood)
 (package liquidsoap)
 (deps
  srt-log-flood.liq
  ./srt_peer.exe
  ./helpers.inc
  ./tone.ts
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  ../test.liq
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} srt-log-flood.liq liquidsoap srt-log-flood.liq)))

(rule
 (alias test_srt-low-bitrate-ts)
 (package liquidsoap)
 (deps
  srt-low-bitrate-ts.liq
  ./srt_peer.exe
  ./helpers.inc
  ./tone.ts
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  ../test.liq
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} srt-low-bitrate-ts.liq liquidsoap srt-low-bitrate-ts.liq)))

(rule
 (alias test_srt-low-bitrate)
 (package liquidsoap)
 (deps
  srt-low-bitrate.liq
  ./srt_peer.exe
  ./helpers.inc
  ./tone.ts
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  ../test.liq
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} srt-low-bitrate.liq liquidsoap srt-low-bitrate.liq)))

(rule
 (alias test_srt-no-leak)
 (package liquidsoap)
 (deps
  srt-no-leak.liq
  ./srt_peer.exe
  ./helpers.inc
  ./tone.ts
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  ../test.liq
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} srt-no-leak.liq liquidsoap srt-no-leak.liq)))

(rule
 (alias test_srt-output-payloads)
 (package liquidsoap)
 (deps
  srt-output-payloads.liq
  ./srt_peer.exe
  ./helpers.inc
  ./tone.ts
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  ../test.liq
  (:run_test ../run_test.exe))
 (action
  (run
   %{run_test}
   srt-output-payloads.liq
   liquidsoap
   srt-output-payloads.liq)))

(rule
 (alias test_srt-pacing)
 (package liquidsoap)
 (deps
  srt-pacing.liq
  ./srt_peer.exe
  ./helpers.inc
  ./tone.ts
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  ../test.liq
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} srt-pacing.liq liquidsoap srt-pacing.liq)))

(rule
 (alias test_srt-process-lifecycle)
 (package liquidsoap)
 (deps
  srt-process-lifecycle.liq
  ./srt_peer.exe
  ./helpers.inc
  ./tone.ts
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  ../test.liq
  (:run_test ../run_test.exe))
 (action
  (run
   %{run_test}
   srt-process-lifecycle.liq
   liquidsoap
   srt-process-lifecycle.liq)))

(rule
 (alias test_srt-raw-pcm)
 (package liquidsoap)
 (deps
  srt-raw-pcm.liq
  ./srt_peer.exe
  ./helpers.inc
  ./tone.ts
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  ../test.liq
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} srt-raw-pcm.liq liquidsoap srt-raw-pcm.liq)))

(rule
 (alias test_srt-shutdown-drain)
 (package liquidsoap)
 (deps
  srt-shutdown-drain.liq
  ./srt_peer.exe
  ./helpers.inc
  ./tone.ts
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  ../test.liq
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} srt-shutdown-drain.liq liquidsoap srt-shutdown-drain.liq)))

(rule
 (alias test_srt-silent-peer)
 (package liquidsoap)
 (deps
  srt-silent-peer.liq
  ./srt_peer.exe
  ./helpers.inc
  ./tone.ts
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  ../test.liq
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} srt-silent-peer.liq liquidsoap srt-silent-peer.liq)))

(rule
 (alias test_srt-slow-peer)
 (package liquidsoap)
 (deps
  srt-slow-peer.liq
  ./srt_peer.exe
  ./helpers.inc
  ./tone.ts
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  ../test.liq
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} srt-slow-peer.liq liquidsoap srt-slow-peer.liq)))

(rule
 (alias test_srt-socket-options)
 (package liquidsoap)
 (deps
  srt-socket-options.liq
  ./srt_peer.exe
  ./helpers.inc
  ./tone.ts
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  ../test.liq
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} srt-socket-options.liq liquidsoap srt-socket-options.liq)))

(rule
 (alias test_srt-unreachable-callers)
 (package liquidsoap)
 (deps
  srt-unreachable-callers.liq
  ./srt_peer.exe
  ./helpers.inc
  ./tone.ts
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  ../test.liq
  (:run_test ../run_test.exe))
 (action
  (run
   %{run_test}
   srt-unreachable-callers.liq
   liquidsoap
   srt-unreachable-callers.liq)))

(rule
 (alias test_srt-validation)
 (package liquidsoap)
 (deps
  srt-validation.liq
  ./srt_peer.exe
  ./helpers.inc
  ./tone.ts
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  ../test.liq
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} srt-validation.liq liquidsoap srt-validation.liq)))

(alias
 (name citest)
 (deps
  (alias test_srt-attempt-race)
  (alias test_srt-av-reconnect)
  (alias test_srt-bind-failure)
  (alias test_srt-busy-listener)
  (alias test_srt-callback-registers)
  (alias test_srt-callbacks-reentrant)
  (alias test_srt-caller-reconnect)
  (alias test_srt-client-failure)
  (alias test_srt-client-limit)
  (alias test_srt-copy-encoder)
  (alias test_srt-disconnect-loop)
  (alias test_srt-dual-stack)
  (alias test_srt-encryption)
  (alias test_srt-end-of-stream)
  (alias test_srt-event-pairing)
  (alias test_srt-interop-ffmpeg)
  (alias test_srt-late-joiner)
  (alias test_srt-latency-reset)
  (alias test_srt-listener-survives)
  (alias test_srt-log-flood)
  (alias test_srt-low-bitrate)
  (alias test_srt-low-bitrate-ts)
  (alias test_srt-no-leak)
  (alias test_srt-output-payloads)
  (alias test_srt-pacing)
  (alias test_srt-process-lifecycle)
  (alias test_srt-raw-pcm)
  (alias test_srt-shutdown-drain)
  (alias test_srt-silent-peer)
  (alias test_srt-slow-peer)
  (alias test_srt-socket-options)
  (alias test_srt-unreachable-callers)
  (alias test_srt-validation)))
