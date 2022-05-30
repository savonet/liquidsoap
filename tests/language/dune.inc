
(rule
 (alias runtest)
 (package liquidsoap)
 (deps
  rec.liq
loop.liq
process.liq
ref.liq
regexp.liq
null.liq
math.liq
pp.liq
error.liq
string.liq
stdlib.liq
argsof.liq
file.watch.liq
interactive.liq
predicate.liq
file.liq
getter.liq
pattern.liq
list.liq
metadata.liq
file.watch2.liq
functions.liq
record.liq
file_protocol.liq
eval.liq
bool.liq
json.liq
encoders.liq
doc.liq
typing.liq
time.liq
  (:liquidsoap ../../src/bin/liquidsoap.exe)
  (:test_liq ../test.liq)
  (:run_test ../run_test.sh))
 (action
  (progn
    (run %{run_test} "%{liquidsoap} %{test_liq} -" rec.liq)
(run %{run_test} "%{liquidsoap} %{test_liq} -" loop.liq)
(run %{run_test} "%{liquidsoap} %{test_liq} -" process.liq)
(run %{run_test} "%{liquidsoap} %{test_liq} -" ref.liq)
(run %{run_test} "%{liquidsoap} %{test_liq} -" regexp.liq)
(run %{run_test} "%{liquidsoap} %{test_liq} -" null.liq)
(run %{run_test} "%{liquidsoap} %{test_liq} -" math.liq)
(run %{run_test} "%{liquidsoap} %{test_liq} -" pp.liq)
(run %{run_test} "%{liquidsoap} %{test_liq} -" error.liq)
(run %{run_test} "%{liquidsoap} %{test_liq} -" string.liq)
(run %{run_test} "%{liquidsoap} %{test_liq} -" stdlib.liq)
(run %{run_test} "%{liquidsoap} %{test_liq} -" argsof.liq)
(run %{run_test} "%{liquidsoap} %{test_liq} -" file.watch.liq)
(run %{run_test} "%{liquidsoap} %{test_liq} -" interactive.liq)
(run %{run_test} "%{liquidsoap} %{test_liq} -" predicate.liq)
(run %{run_test} "%{liquidsoap} %{test_liq} -" file.liq)
(run %{run_test} "%{liquidsoap} %{test_liq} -" getter.liq)
(run %{run_test} "%{liquidsoap} %{test_liq} -" pattern.liq)
(run %{run_test} "%{liquidsoap} %{test_liq} -" list.liq)
(run %{run_test} "%{liquidsoap} %{test_liq} -" metadata.liq)
(run %{run_test} "%{liquidsoap} %{test_liq} -" file.watch2.liq)
(run %{run_test} "%{liquidsoap} %{test_liq} -" functions.liq)
(run %{run_test} "%{liquidsoap} %{test_liq} -" record.liq)
(run %{run_test} "%{liquidsoap} %{test_liq} -" file_protocol.liq)
(run %{run_test} "%{liquidsoap} %{test_liq} -" eval.liq)
(run %{run_test} "%{liquidsoap} %{test_liq} -" bool.liq)
(run %{run_test} "%{liquidsoap} %{test_liq} -" json.liq)
(run %{run_test} "%{liquidsoap} %{test_liq} -" encoders.liq)
(run %{run_test} "%{liquidsoap} %{test_liq} -" doc.liq)
(run %{run_test} "%{liquidsoap} %{test_liq} -" typing.liq)
(run %{run_test} "%{liquidsoap} %{test_liq} -" time.liq))))
  