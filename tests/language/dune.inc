
(rule
 (alias citest)
 (package liquidsoap)
 (deps
  rec.liq
  ../media/all_media_files
  ../../src/bin/liquidsoap.exe
  (source_tree ../../src/libs)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} rec.liq liquidsoap %{test_liq} rec.liq)))

(rule
 (alias citest)
 (package liquidsoap)
 (deps
  loop.liq
  ../media/all_media_files
  ../../src/bin/liquidsoap.exe
  (source_tree ../../src/libs)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} loop.liq liquidsoap %{test_liq} loop.liq)))

(rule
 (alias citest)
 (package liquidsoap)
 (deps
  type_errors.liq
  ../media/all_media_files
  ../../src/bin/liquidsoap.exe
  (source_tree ../../src/libs)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} type_errors.liq liquidsoap %{test_liq} type_errors.liq)))

(rule
 (alias citest)
 (package liquidsoap)
 (deps
  process.liq
  ../media/all_media_files
  ../../src/bin/liquidsoap.exe
  (source_tree ../../src/libs)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} process.liq liquidsoap %{test_liq} process.liq)))

(rule
 (alias citest)
 (package liquidsoap)
 (deps
  ref.liq
  ../media/all_media_files
  ../../src/bin/liquidsoap.exe
  (source_tree ../../src/libs)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} ref.liq liquidsoap %{test_liq} ref.liq)))

(rule
 (alias citest)
 (package liquidsoap)
 (deps
  conversions.liq
  ../media/all_media_files
  ../../src/bin/liquidsoap.exe
  (source_tree ../../src/libs)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} conversions.liq liquidsoap %{test_liq} conversions.liq)))

(rule
 (alias citest)
 (package liquidsoap)
 (deps
  regexp.liq
  ../media/all_media_files
  ../../src/bin/liquidsoap.exe
  (source_tree ../../src/libs)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} regexp.liq liquidsoap %{test_liq} regexp.liq)))

(rule
 (alias citest)
 (package liquidsoap)
 (deps
  null.liq
  ../media/all_media_files
  ../../src/bin/liquidsoap.exe
  (source_tree ../../src/libs)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} null.liq liquidsoap %{test_liq} null.liq)))

(rule
 (alias citest)
 (package liquidsoap)
 (deps
  math.liq
  ../media/all_media_files
  ../../src/bin/liquidsoap.exe
  (source_tree ../../src/libs)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} math.liq liquidsoap %{test_liq} math.liq)))

(rule
 (alias citest)
 (package liquidsoap)
 (deps
  socket.liq
  ../media/all_media_files
  ../../src/bin/liquidsoap.exe
  (source_tree ../../src/libs)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} socket.liq liquidsoap %{test_liq} socket.liq)))

(rule
 (alias citest)
 (package liquidsoap)
 (deps
  pp.liq
  ../media/all_media_files
  ../../src/bin/liquidsoap.exe
  (source_tree ../../src/libs)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} pp.liq liquidsoap %{test_liq} pp.liq)))

(rule
 (alias citest)
 (package liquidsoap)
 (deps
  error.liq
  ../media/all_media_files
  ../../src/bin/liquidsoap.exe
  (source_tree ../../src/libs)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} error.liq liquidsoap %{test_liq} error.liq)))

(rule
 (alias citest)
 (package liquidsoap)
 (deps
  string.liq
  ../media/all_media_files
  ../../src/bin/liquidsoap.exe
  (source_tree ../../src/libs)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} string.liq liquidsoap %{test_liq} string.liq)))

(rule
 (alias citest)
 (package liquidsoap)
 (deps
  stdlib.liq
  ../media/all_media_files
  ../../src/bin/liquidsoap.exe
  (source_tree ../../src/libs)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} stdlib.liq liquidsoap %{test_liq} stdlib.liq)))

(rule
 (alias citest)
 (package liquidsoap)
 (deps
  yaml.liq
  ../media/all_media_files
  ../../src/bin/liquidsoap.exe
  (source_tree ../../src/libs)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} yaml.liq liquidsoap %{test_liq} yaml.liq)))

(rule
 (alias citest)
 (package liquidsoap)
 (deps
  argsof.liq
  ../media/all_media_files
  ../../src/bin/liquidsoap.exe
  (source_tree ../../src/libs)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} argsof.liq liquidsoap %{test_liq} argsof.liq)))

(rule
 (alias citest)
 (package liquidsoap)
 (deps
  file.watch.liq
  ../media/all_media_files
  ../../src/bin/liquidsoap.exe
  (source_tree ../../src/libs)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} file.watch.liq liquidsoap %{test_liq} file.watch.liq)))

(rule
 (alias citest)
 (package liquidsoap)
 (deps
  interactive.liq
  ../media/all_media_files
  ../../src/bin/liquidsoap.exe
  (source_tree ../../src/libs)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} interactive.liq liquidsoap %{test_liq} interactive.liq)))

(rule
 (alias citest)
 (package liquidsoap)
 (deps
  predicate.liq
  ../media/all_media_files
  ../../src/bin/liquidsoap.exe
  (source_tree ../../src/libs)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} predicate.liq liquidsoap %{test_liq} predicate.liq)))

(rule
 (alias citest)
 (package liquidsoap)
 (deps
  file.liq
  ../media/all_media_files
  ../../src/bin/liquidsoap.exe
  (source_tree ../../src/libs)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} file.liq liquidsoap %{test_liq} file.liq)))

(rule
 (alias citest)
 (package liquidsoap)
 (deps
  getter.liq
  ../media/all_media_files
  ../../src/bin/liquidsoap.exe
  (source_tree ../../src/libs)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} getter.liq liquidsoap %{test_liq} getter.liq)))

(rule
 (alias citest)
 (package liquidsoap)
 (deps
  pattern.liq
  ../media/all_media_files
  ../../src/bin/liquidsoap.exe
  (source_tree ../../src/libs)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} pattern.liq liquidsoap %{test_liq} pattern.liq)))

(rule
 (alias citest)
 (package liquidsoap)
 (deps
  list.liq
  ../media/all_media_files
  ../../src/bin/liquidsoap.exe
  (source_tree ../../src/libs)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} list.liq liquidsoap %{test_liq} list.liq)))

(rule
 (alias citest)
 (package liquidsoap)
 (deps
  mem_usage.liq
  ../media/all_media_files
  ../../src/bin/liquidsoap.exe
  (source_tree ../../src/libs)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} mem_usage.liq liquidsoap %{test_liq} mem_usage.liq)))

(rule
 (alias citest)
 (package liquidsoap)
 (deps
  metadata.liq
  ../media/all_media_files
  ../../src/bin/liquidsoap.exe
  (source_tree ../../src/libs)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} metadata.liq liquidsoap %{test_liq} metadata.liq)))

(rule
 (alias citest)
 (package liquidsoap)
 (deps
  file.watch2.liq
  ../media/all_media_files
  ../../src/bin/liquidsoap.exe
  (source_tree ../../src/libs)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} file.watch2.liq liquidsoap %{test_liq} file.watch2.liq)))

(rule
 (alias citest)
 (package liquidsoap)
 (deps
  functions.liq
  ../media/all_media_files
  ../../src/bin/liquidsoap.exe
  (source_tree ../../src/libs)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} functions.liq liquidsoap %{test_liq} functions.liq)))

(rule
 (alias citest)
 (package liquidsoap)
 (deps
  record.liq
  ../media/all_media_files
  ../../src/bin/liquidsoap.exe
  (source_tree ../../src/libs)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} record.liq liquidsoap %{test_liq} record.liq)))

(rule
 (alias citest)
 (package liquidsoap)
 (deps
  osc.liq
  ../media/all_media_files
  ../../src/bin/liquidsoap.exe
  (source_tree ../../src/libs)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} osc.liq liquidsoap %{test_liq} osc.liq)))

(rule
 (alias citest)
 (package liquidsoap)
 (deps
  file_protocol.liq
  ../media/all_media_files
  ../../src/bin/liquidsoap.exe
  (source_tree ../../src/libs)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} file_protocol.liq liquidsoap %{test_liq} file_protocol.liq)))

(rule
 (alias citest)
 (package liquidsoap)
 (deps
  eval.liq
  ../media/all_media_files
  ../../src/bin/liquidsoap.exe
  (source_tree ../../src/libs)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} eval.liq liquidsoap %{test_liq} eval.liq)))

(rule
 (alias citest)
 (package liquidsoap)
 (deps
  bool.liq
  ../media/all_media_files
  ../../src/bin/liquidsoap.exe
  (source_tree ../../src/libs)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} bool.liq liquidsoap %{test_liq} bool.liq)))

(rule
 (alias citest)
 (package liquidsoap)
 (deps
  json.liq
  ../media/all_media_files
  ../../src/bin/liquidsoap.exe
  (source_tree ../../src/libs)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} json.liq liquidsoap %{test_liq} json.liq)))

(rule
 (alias citest)
 (package liquidsoap)
 (deps
  encoders.liq
  ../media/all_media_files
  ../../src/bin/liquidsoap.exe
  (source_tree ../../src/libs)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} encoders.liq liquidsoap %{test_liq} encoders.liq)))

(rule
 (alias citest)
 (package liquidsoap)
 (deps
  doc.liq
  ../media/all_media_files
  ../../src/bin/liquidsoap.exe
  (source_tree ../../src/libs)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} doc.liq liquidsoap %{test_liq} doc.liq)))

(rule
 (alias citest)
 (package liquidsoap)
 (deps
  typing.liq
  ../media/all_media_files
  ../../src/bin/liquidsoap.exe
  (source_tree ../../src/libs)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} typing.liq liquidsoap %{test_liq} typing.liq)))

(rule
 (alias citest)
 (package liquidsoap)
 (deps
  time.liq
  ../media/all_media_files
  ../../src/bin/liquidsoap.exe
  (source_tree ../../src/libs)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} time.liq liquidsoap %{test_liq} time.liq)))
