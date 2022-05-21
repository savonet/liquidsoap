
(rule
 (alias runtest)
 (package liquidsoap)
 (deps
  GH1159.liq
LS460.liq
LS503.liq
115-1.liq
115-2.liq
123.liq
LS354-2.liq
init-error.liq
LS354-1.liq
GH1129.liq
metadata_cache.liq
GH1279.liq
GH1327.liq
BUG403.liq
input_rtmp.liq
infallible-shutdown.liq
GH-action-919422659.liq
GH1151.liq
playlist-id.liq
GH1146.liq
AC5109.liq
LS268.liq
  (:liquidsoap ../../src/bin/liquidsoap.exe)
  (source_tree ../libs)
  (:stdlib ../../libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.sh))
 (action
  (progn
    (run %{run_test} "%{liquidsoap} --no-stdlib %{stdlib} %{test_liq} -" GH1159.liq)
(run %{run_test} "%{liquidsoap} --no-stdlib %{stdlib} %{test_liq} -" LS460.liq)
(run %{run_test} "%{liquidsoap} --no-stdlib %{stdlib} %{test_liq} -" LS503.liq)
(run %{run_test} "%{liquidsoap} --no-stdlib %{stdlib} %{test_liq} -" 115-1.liq)
(run %{run_test} "%{liquidsoap} --no-stdlib %{stdlib} %{test_liq} -" 115-2.liq)
(run %{run_test} "%{liquidsoap} --no-stdlib %{stdlib} %{test_liq} -" 123.liq)
(run %{run_test} "%{liquidsoap} --no-stdlib %{stdlib} %{test_liq} -" LS354-2.liq)
(run %{run_test} "%{liquidsoap} --no-stdlib %{stdlib} %{test_liq} -" init-error.liq)
(run %{run_test} "%{liquidsoap} --no-stdlib %{stdlib} %{test_liq} -" LS354-1.liq)
(run %{run_test} "%{liquidsoap} --no-stdlib %{stdlib} %{test_liq} -" GH1129.liq)
(run %{run_test} "%{liquidsoap} --no-stdlib %{stdlib} %{test_liq} -" metadata_cache.liq)
(run %{run_test} "%{liquidsoap} --no-stdlib %{stdlib} %{test_liq} -" GH1279.liq)
(run %{run_test} "%{liquidsoap} --no-stdlib %{stdlib} %{test_liq} -" GH1327.liq)
(run %{run_test} "%{liquidsoap} --no-stdlib %{stdlib} %{test_liq} -" BUG403.liq)
(run %{run_test} "%{liquidsoap} --no-stdlib %{stdlib} %{test_liq} -" input_rtmp.liq)
(run %{run_test} "%{liquidsoap} --no-stdlib %{stdlib} %{test_liq} -" infallible-shutdown.liq)
(run %{run_test} "%{liquidsoap} --no-stdlib %{stdlib} %{test_liq} -" GH-action-919422659.liq)
(run %{run_test} "%{liquidsoap} --no-stdlib %{stdlib} %{test_liq} -" GH1151.liq)
(run %{run_test} "%{liquidsoap} --no-stdlib %{stdlib} %{test_liq} -" playlist-id.liq)
(run %{run_test} "%{liquidsoap} --no-stdlib %{stdlib} %{test_liq} -" GH1146.liq)
(run %{run_test} "%{liquidsoap} --no-stdlib %{stdlib} %{test_liq} -" AC5109.liq)
(run %{run_test} "%{liquidsoap} --no-stdlib %{stdlib} %{test_liq} -" LS268.liq))))
  