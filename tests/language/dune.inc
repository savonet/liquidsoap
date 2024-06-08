
(rule
 (alias citest)
 (package liquidsoap)
 (deps
  argsof.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:stdlib ../../src/libs/stdlib.liq)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} argsof.liq liquidsoap --stdlib %{stdlib} %{test_liq} argsof.liq)))

(rule
 (alias citest)
 (package liquidsoap)
 (deps
  bool.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:stdlib ../../src/libs/stdlib.liq)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} bool.liq liquidsoap --stdlib %{stdlib} %{test_liq} bool.liq)))

(rule
 (alias citest)
 (package liquidsoap)
 (deps
  comments.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:stdlib ../../src/libs/stdlib.liq)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} comments.liq liquidsoap --stdlib %{stdlib} %{test_liq} comments.liq)))

(rule
 (alias citest)
 (package liquidsoap)
 (deps
  conversions.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:stdlib ../../src/libs/stdlib.liq)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} conversions.liq liquidsoap --stdlib %{stdlib} %{test_liq} conversions.liq)))

(rule
 (alias citest)
 (package liquidsoap)
 (deps
  doc.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:stdlib ../../src/libs/stdlib.liq)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} doc.liq liquidsoap --stdlib %{stdlib} %{test_liq} doc.liq)))

(rule
 (alias citest)
 (package liquidsoap)
 (deps
  encoders.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:stdlib ../../src/libs/stdlib.liq)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} encoders.liq liquidsoap --stdlib %{stdlib} %{test_liq} encoders.liq)))

(rule
 (alias citest)
 (package liquidsoap)
 (deps
  error.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:stdlib ../../src/libs/stdlib.liq)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} error.liq liquidsoap --stdlib %{stdlib} %{test_liq} error.liq)))

(rule
 (alias citest)
 (package liquidsoap)
 (deps
  eval.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:stdlib ../../src/libs/stdlib.liq)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} eval.liq liquidsoap --stdlib %{stdlib} %{test_liq} eval.liq)))

(rule
 (alias citest)
 (package liquidsoap)
 (deps
  file.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:stdlib ../../src/libs/stdlib.liq)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} file.liq liquidsoap --stdlib %{stdlib} %{test_liq} file.liq)))

(rule
 (alias citest)
 (package liquidsoap)
 (deps
  file.watch.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:stdlib ../../src/libs/stdlib.liq)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} file.watch.liq liquidsoap --stdlib %{stdlib} %{test_liq} file.watch.liq)))

(rule
 (alias citest)
 (package liquidsoap)
 (deps
  file.watch2.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:stdlib ../../src/libs/stdlib.liq)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} file.watch2.liq liquidsoap --stdlib %{stdlib} %{test_liq} file.watch2.liq)))

(rule
 (alias citest)
 (package liquidsoap)
 (deps
  file_protocol.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:stdlib ../../src/libs/stdlib.liq)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} file_protocol.liq liquidsoap --stdlib %{stdlib} %{test_liq} file_protocol.liq)))

(rule
 (alias citest)
 (package liquidsoap)
 (deps
  functions.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:stdlib ../../src/libs/stdlib.liq)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} functions.liq liquidsoap --stdlib %{stdlib} %{test_liq} functions.liq)))

(rule
 (alias citest)
 (package liquidsoap)
 (deps
  getter.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:stdlib ../../src/libs/stdlib.liq)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} getter.liq liquidsoap --stdlib %{stdlib} %{test_liq} getter.liq)))

(rule
 (alias citest)
 (package liquidsoap)
 (deps
  interactive.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:stdlib ../../src/libs/stdlib.liq)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} interactive.liq liquidsoap --stdlib %{stdlib} %{test_liq} interactive.liq)))

(rule
 (alias citest)
 (package liquidsoap)
 (deps
  json.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:stdlib ../../src/libs/stdlib.liq)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} json.liq liquidsoap --stdlib %{stdlib} %{test_liq} json.liq)))

(rule
 (alias citest)
 (package liquidsoap)
 (deps
  list.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:stdlib ../../src/libs/stdlib.liq)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} list.liq liquidsoap --stdlib %{stdlib} %{test_liq} list.liq)))

(rule
 (alias citest)
 (package liquidsoap)
 (deps
  loop.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:stdlib ../../src/libs/stdlib.liq)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} loop.liq liquidsoap --stdlib %{stdlib} %{test_liq} loop.liq)))

(rule
 (alias citest)
 (package liquidsoap)
 (deps
  math.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:stdlib ../../src/libs/stdlib.liq)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} math.liq liquidsoap --stdlib %{stdlib} %{test_liq} math.liq)))

(rule
 (alias citest)
 (package liquidsoap)
 (deps
  mem_usage.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:stdlib ../../src/libs/stdlib.liq)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} mem_usage.liq liquidsoap --stdlib %{stdlib} %{test_liq} mem_usage.liq)))

(rule
 (alias citest)
 (package liquidsoap)
 (deps
  metadata.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:stdlib ../../src/libs/stdlib.liq)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} metadata.liq liquidsoap --stdlib %{stdlib} %{test_liq} metadata.liq)))

(rule
 (alias citest)
 (package liquidsoap)
 (deps
  metrics.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:stdlib ../../src/libs/stdlib.liq)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} metrics.liq liquidsoap --stdlib %{stdlib} %{test_liq} metrics.liq)))

(rule
 (alias citest)
 (package liquidsoap)
 (deps
  null.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:stdlib ../../src/libs/stdlib.liq)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} null.liq liquidsoap --stdlib %{stdlib} %{test_liq} null.liq)))

(rule
 (alias citest)
 (package liquidsoap)
 (deps
  number.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:stdlib ../../src/libs/stdlib.liq)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} number.liq liquidsoap --stdlib %{stdlib} %{test_liq} number.liq)))

(rule
 (alias citest)
 (package liquidsoap)
 (deps
  osc.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:stdlib ../../src/libs/stdlib.liq)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} osc.liq liquidsoap --stdlib %{stdlib} %{test_liq} osc.liq)))

(rule
 (alias citest)
 (package liquidsoap)
 (deps
  pattern.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:stdlib ../../src/libs/stdlib.liq)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} pattern.liq liquidsoap --stdlib %{stdlib} %{test_liq} pattern.liq)))

(rule
 (alias citest)
 (package liquidsoap)
 (deps
  pp.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:stdlib ../../src/libs/stdlib.liq)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} pp.liq liquidsoap --stdlib %{stdlib} %{test_liq} pp.liq)))

(rule
 (alias citest)
 (package liquidsoap)
 (deps
  predicate.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:stdlib ../../src/libs/stdlib.liq)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} predicate.liq liquidsoap --stdlib %{stdlib} %{test_liq} predicate.liq)))

(rule
 (alias citest)
 (package liquidsoap)
 (deps
  process.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:stdlib ../../src/libs/stdlib.liq)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} process.liq liquidsoap --stdlib %{stdlib} %{test_liq} process.liq)))

(rule
 (alias citest)
 (package liquidsoap)
 (deps
  rec.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:stdlib ../../src/libs/stdlib.liq)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} rec.liq liquidsoap --stdlib %{stdlib} %{test_liq} rec.liq)))

(rule
 (alias citest)
 (package liquidsoap)
 (deps
  record.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:stdlib ../../src/libs/stdlib.liq)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} record.liq liquidsoap --stdlib %{stdlib} %{test_liq} record.liq)))

(rule
 (alias citest)
 (package liquidsoap)
 (deps
  ref.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:stdlib ../../src/libs/stdlib.liq)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} ref.liq liquidsoap --stdlib %{stdlib} %{test_liq} ref.liq)))

(rule
 (alias citest)
 (package liquidsoap)
 (deps
  regexp.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:stdlib ../../src/libs/stdlib.liq)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} regexp.liq liquidsoap --stdlib %{stdlib} %{test_liq} regexp.liq)))

(rule
 (alias citest)
 (package liquidsoap)
 (deps
  replaygain.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:stdlib ../../src/libs/stdlib.liq)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} replaygain.liq liquidsoap --stdlib %{stdlib} %{test_liq} replaygain.liq)))

(rule
 (alias citest)
 (package liquidsoap)
 (deps
  socket.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:stdlib ../../src/libs/stdlib.liq)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} socket.liq liquidsoap --stdlib %{stdlib} %{test_liq} socket.liq)))

(rule
 (alias citest)
 (package liquidsoap)
 (deps
  sqlite.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:stdlib ../../src/libs/stdlib.liq)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} sqlite.liq liquidsoap --stdlib %{stdlib} %{test_liq} sqlite.liq)))

(rule
 (alias citest)
 (package liquidsoap)
 (deps
  stdlib.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:stdlib ../../src/libs/stdlib.liq)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} stdlib.liq liquidsoap --stdlib %{stdlib} %{test_liq} stdlib.liq)))

(rule
 (alias citest)
 (package liquidsoap)
 (deps
  string.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:stdlib ../../src/libs/stdlib.liq)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} string.liq liquidsoap --stdlib %{stdlib} %{test_liq} string.liq)))

(rule
 (alias citest)
 (package liquidsoap)
 (deps
  thread.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:stdlib ../../src/libs/stdlib.liq)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} thread.liq liquidsoap --stdlib %{stdlib} %{test_liq} thread.liq)))

(rule
 (alias citest)
 (package liquidsoap)
 (deps
  time.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:stdlib ../../src/libs/stdlib.liq)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} time.liq liquidsoap --stdlib %{stdlib} %{test_liq} time.liq)))

(rule
 (alias citest)
 (package liquidsoap)
 (deps
  type_errors.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:stdlib ../../src/libs/stdlib.liq)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} type_errors.liq liquidsoap --stdlib %{stdlib} %{test_liq} type_errors.liq)))

(rule
 (alias citest)
 (package liquidsoap)
 (deps
  typing.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:stdlib ../../src/libs/stdlib.liq)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} typing.liq liquidsoap --stdlib %{stdlib} %{test_liq} typing.liq)))

(rule
 (alias citest)
 (package liquidsoap)
 (deps
  url.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:stdlib ../../src/libs/stdlib.liq)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} url.liq liquidsoap --stdlib %{stdlib} %{test_liq} url.liq)))

(rule
 (alias citest)
 (package liquidsoap)
 (deps
  yaml.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:stdlib ../../src/libs/stdlib.liq)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} yaml.liq liquidsoap --stdlib %{stdlib} %{test_liq} yaml.liq)))
