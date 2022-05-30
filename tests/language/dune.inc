
(rule
 (alias runtest)
 (package liquidsoap)
 (deps
  file.liq
pp.liq
pattern.liq
interactive.liq
encoders.liq
predicate.liq
metadata.liq
file.watch2.liq
doc.liq
bool.liq
null.liq
error.liq
string.liq
eval.liq
stdlib.liq
file_protocol.liq
getter.liq
typing.liq
loop.liq
functions.liq
ref.liq
process.liq
math.liq
json.liq
argsof.liq
rec.liq
regexp.liq
file.watch.liq
record.liq
list.liq
mem_usage.liq
time.liq
  (:liquidsoap ../../src/bin/liquidsoap.exe)
  (:test_liq ../test.liq)
  (:run_test ../run_test.sh))
 (action
  (progn
    (run %{run_test} "%{liquidsoap} %{test_liq} -" file.liq)
(run %{run_test} "%{liquidsoap} %{test_liq} -" pp.liq)
(run %{run_test} "%{liquidsoap} %{test_liq} -" pattern.liq)
(run %{run_test} "%{liquidsoap} %{test_liq} -" interactive.liq)
(run %{run_test} "%{liquidsoap} %{test_liq} -" encoders.liq)
(run %{run_test} "%{liquidsoap} %{test_liq} -" predicate.liq)
(run %{run_test} "%{liquidsoap} %{test_liq} -" metadata.liq)
(run %{run_test} "%{liquidsoap} %{test_liq} -" file.watch2.liq)
(run %{run_test} "%{liquidsoap} %{test_liq} -" doc.liq)
(run %{run_test} "%{liquidsoap} %{test_liq} -" bool.liq)
(run %{run_test} "%{liquidsoap} %{test_liq} -" null.liq)
(run %{run_test} "%{liquidsoap} %{test_liq} -" error.liq)
(run %{run_test} "%{liquidsoap} %{test_liq} -" string.liq)
(run %{run_test} "%{liquidsoap} %{test_liq} -" eval.liq)
(run %{run_test} "%{liquidsoap} %{test_liq} -" stdlib.liq)
(run %{run_test} "%{liquidsoap} %{test_liq} -" file_protocol.liq)
(run %{run_test} "%{liquidsoap} %{test_liq} -" getter.liq)
(run %{run_test} "%{liquidsoap} %{test_liq} -" typing.liq)
(run %{run_test} "%{liquidsoap} %{test_liq} -" loop.liq)
(run %{run_test} "%{liquidsoap} %{test_liq} -" functions.liq)
(run %{run_test} "%{liquidsoap} %{test_liq} -" ref.liq)
(run %{run_test} "%{liquidsoap} %{test_liq} -" process.liq)
(run %{run_test} "%{liquidsoap} %{test_liq} -" math.liq)
(run %{run_test} "%{liquidsoap} %{test_liq} -" json.liq)
(run %{run_test} "%{liquidsoap} %{test_liq} -" argsof.liq)
(run %{run_test} "%{liquidsoap} %{test_liq} -" rec.liq)
(run %{run_test} "%{liquidsoap} %{test_liq} -" regexp.liq)
(run %{run_test} "%{liquidsoap} %{test_liq} -" file.watch.liq)
(run %{run_test} "%{liquidsoap} %{test_liq} -" record.liq)
(run %{run_test} "%{liquidsoap} %{test_liq} -" list.liq)
(run %{run_test} "%{liquidsoap} %{test_liq} -" mem_usage.liq)
(run %{run_test} "%{liquidsoap} %{test_liq} -" time.liq))))
  