
(rule
 (alias citest)
 (package liquidsoap)
 (deps
  argsof.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} argsof.liq liquidsoap %{test_liq} argsof.liq)))
  
(rule
 (alias citest)
 (package liquidsoap)
 (deps
  bool.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} bool.liq liquidsoap %{test_liq} bool.liq)))
  
(rule
 (alias citest)
 (package liquidsoap)
 (deps
  comments.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} comments.liq liquidsoap %{test_liq} comments.liq)))
  
(rule
 (alias citest)
 (package liquidsoap)
 (deps
  conversions.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} conversions.liq liquidsoap %{test_liq} conversions.liq)))
  
(rule
 (alias citest)
 (package liquidsoap)
 (deps
  doc.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} doc.liq liquidsoap %{test_liq} doc.liq)))
  
(rule
 (alias citest)
 (package liquidsoap)
 (deps
  encoders.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} encoders.liq liquidsoap %{test_liq} encoders.liq)))
  
(rule
 (alias citest)
 (package liquidsoap)
 (deps
  error.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} error.liq liquidsoap %{test_liq} error.liq)))
  
(rule
 (alias citest)
 (package liquidsoap)
 (deps
  eval.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} eval.liq liquidsoap %{test_liq} eval.liq)))
  
(rule
 (alias citest)
 (package liquidsoap)
 (deps
  file.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} file.liq liquidsoap %{test_liq} file.liq)))
  
(rule
 (alias citest)
 (package liquidsoap)
 (deps
  file.watch.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} file.watch.liq liquidsoap %{test_liq} file.watch.liq)))
  
(rule
 (alias citest)
 (package liquidsoap)
 (deps
  file.watch2.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} file.watch2.liq liquidsoap %{test_liq} file.watch2.liq)))
  
(rule
 (alias citest)
 (package liquidsoap)
 (deps
  file_protocol.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} file_protocol.liq liquidsoap %{test_liq} file_protocol.liq)))
  
(rule
 (alias citest)
 (package liquidsoap)
 (deps
  functions.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} functions.liq liquidsoap %{test_liq} functions.liq)))
  
(rule
 (alias citest)
 (package liquidsoap)
 (deps
  getter.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} getter.liq liquidsoap %{test_liq} getter.liq)))
  
(rule
 (alias citest)
 (package liquidsoap)
 (deps
  interactive.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} interactive.liq liquidsoap %{test_liq} interactive.liq)))
  
(rule
 (alias citest)
 (package liquidsoap)
 (deps
  json.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} json.liq liquidsoap %{test_liq} json.liq)))
  
(rule
 (alias citest)
 (package liquidsoap)
 (deps
  list.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} list.liq liquidsoap %{test_liq} list.liq)))
  
(rule
 (alias citest)
 (package liquidsoap)
 (deps
  loop.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} loop.liq liquidsoap %{test_liq} loop.liq)))
  
(rule
 (alias citest)
 (package liquidsoap)
 (deps
  math.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} math.liq liquidsoap %{test_liq} math.liq)))
  
(rule
 (alias citest)
 (package liquidsoap)
 (deps
  mem_usage.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} mem_usage.liq liquidsoap %{test_liq} mem_usage.liq)))
  
(rule
 (alias citest)
 (package liquidsoap)
 (deps
  metadata.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} metadata.liq liquidsoap %{test_liq} metadata.liq)))
  
(rule
 (alias citest)
 (package liquidsoap)
 (deps
  metrics.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} metrics.liq liquidsoap %{test_liq} metrics.liq)))
  
(rule
 (alias citest)
 (package liquidsoap)
 (deps
  null.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} null.liq liquidsoap %{test_liq} null.liq)))
  
(rule
 (alias citest)
 (package liquidsoap)
 (deps
  number.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} number.liq liquidsoap %{test_liq} number.liq)))
  
(rule
 (alias citest)
 (package liquidsoap)
 (deps
  osc.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} osc.liq liquidsoap %{test_liq} osc.liq)))
  
(rule
 (alias citest)
 (package liquidsoap)
 (deps
  pattern.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} pattern.liq liquidsoap %{test_liq} pattern.liq)))
  
(rule
 (alias citest)
 (package liquidsoap)
 (deps
  pp.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} pp.liq liquidsoap %{test_liq} pp.liq)))
  
(rule
 (alias citest)
 (package liquidsoap)
 (deps
  predicate.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} predicate.liq liquidsoap %{test_liq} predicate.liq)))
  
(rule
 (alias citest)
 (package liquidsoap)
 (deps
  process.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} process.liq liquidsoap %{test_liq} process.liq)))
  
(rule
 (alias citest)
 (package liquidsoap)
 (deps
  rec.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} rec.liq liquidsoap %{test_liq} rec.liq)))
  
(rule
 (alias citest)
 (package liquidsoap)
 (deps
  record.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} record.liq liquidsoap %{test_liq} record.liq)))
  
(rule
 (alias citest)
 (package liquidsoap)
 (deps
  ref.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} ref.liq liquidsoap %{test_liq} ref.liq)))
  
(rule
 (alias citest)
 (package liquidsoap)
 (deps
  regexp.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} regexp.liq liquidsoap %{test_liq} regexp.liq)))
  
(rule
 (alias citest)
 (package liquidsoap)
 (deps
  replaygain.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} replaygain.liq liquidsoap %{test_liq} replaygain.liq)))
  
(rule
 (alias citest)
 (package liquidsoap)
 (deps
  socket.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} socket.liq liquidsoap %{test_liq} socket.liq)))
  
(rule
 (alias citest)
 (package liquidsoap)
 (deps
  sqlite.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} sqlite.liq liquidsoap %{test_liq} sqlite.liq)))
  
(rule
 (alias citest)
 (package liquidsoap)
 (deps
  stdlib.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} stdlib.liq liquidsoap %{test_liq} stdlib.liq)))
  
(rule
 (alias citest)
 (package liquidsoap)
 (deps
  string.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} string.liq liquidsoap %{test_liq} string.liq)))
  
(rule
 (alias citest)
 (package liquidsoap)
 (deps
  thread.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} thread.liq liquidsoap %{test_liq} thread.liq)))
  
(rule
 (alias citest)
 (package liquidsoap)
 (deps
  time.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} time.liq liquidsoap %{test_liq} time.liq)))
  
(rule
 (alias citest)
 (package liquidsoap)
 (deps
  type_errors.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} type_errors.liq liquidsoap %{test_liq} type_errors.liq)))
  
(rule
 (alias citest)
 (package liquidsoap)
 (deps
  typing.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} typing.liq liquidsoap %{test_liq} typing.liq)))
  
(rule
 (alias citest)
 (package liquidsoap)
 (deps
  url.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} url.liq liquidsoap %{test_liq} url.liq)))
  
(rule
 (alias citest)
 (package liquidsoap)
 (deps
  various.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} various.liq liquidsoap %{test_liq} various.liq)))
  
(rule
 (alias citest)
 (package liquidsoap)
 (deps
  xml_test.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} xml_test.liq liquidsoap %{test_liq} xml_test.liq)))
  
(rule
 (alias citest)
 (package liquidsoap)
 (deps
  yaml.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (source_tree ../../src/libs)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} yaml.liq liquidsoap %{test_liq} yaml.liq)))
  