
(rule
 (alias runtest)
 (package liquidsoap)
 (deps
  rec.liq
  ../media/all_media_files
  (:liquidsoap ../../src/bin/liquidsoap.exe)
  (:test_liq ../test.liq)
  (:run_test ../run_test.sh))
 (action (run %{run_test} "%{liquidsoap} %{test_liq} -" rec.liq)))
  
(rule
 (alias runtest)
 (package liquidsoap)
 (deps
  loop.liq
  ../media/all_media_files
  (:liquidsoap ../../src/bin/liquidsoap.exe)
  (:test_liq ../test.liq)
  (:run_test ../run_test.sh))
 (action (run %{run_test} "%{liquidsoap} %{test_liq} -" loop.liq)))
  
(rule
 (alias runtest)
 (package liquidsoap)
 (deps
  process.liq
  ../media/all_media_files
  (:liquidsoap ../../src/bin/liquidsoap.exe)
  (:test_liq ../test.liq)
  (:run_test ../run_test.sh))
 (action (run %{run_test} "%{liquidsoap} %{test_liq} -" process.liq)))
  
(rule
 (alias runtest)
 (package liquidsoap)
 (deps
  ref.liq
  ../media/all_media_files
  (:liquidsoap ../../src/bin/liquidsoap.exe)
  (:test_liq ../test.liq)
  (:run_test ../run_test.sh))
 (action (run %{run_test} "%{liquidsoap} %{test_liq} -" ref.liq)))
  
(rule
 (alias runtest)
 (package liquidsoap)
 (deps
  regexp.liq
  ../media/all_media_files
  (:liquidsoap ../../src/bin/liquidsoap.exe)
  (:test_liq ../test.liq)
  (:run_test ../run_test.sh))
 (action (run %{run_test} "%{liquidsoap} %{test_liq} -" regexp.liq)))
  
(rule
 (alias runtest)
 (package liquidsoap)
 (deps
  null.liq
  ../media/all_media_files
  (:liquidsoap ../../src/bin/liquidsoap.exe)
  (:test_liq ../test.liq)
  (:run_test ../run_test.sh))
 (action (run %{run_test} "%{liquidsoap} %{test_liq} -" null.liq)))
  
(rule
 (alias runtest)
 (package liquidsoap)
 (deps
  math.liq
  ../media/all_media_files
  (:liquidsoap ../../src/bin/liquidsoap.exe)
  (:test_liq ../test.liq)
  (:run_test ../run_test.sh))
 (action (run %{run_test} "%{liquidsoap} %{test_liq} -" math.liq)))
  
(rule
 (alias runtest)
 (package liquidsoap)
 (deps
  pp.liq
  ../media/all_media_files
  (:liquidsoap ../../src/bin/liquidsoap.exe)
  (:test_liq ../test.liq)
  (:run_test ../run_test.sh))
 (action (run %{run_test} "%{liquidsoap} %{test_liq} -" pp.liq)))
  
(rule
 (alias runtest)
 (package liquidsoap)
 (deps
  error.liq
  ../media/all_media_files
  (:liquidsoap ../../src/bin/liquidsoap.exe)
  (:test_liq ../test.liq)
  (:run_test ../run_test.sh))
 (action (run %{run_test} "%{liquidsoap} %{test_liq} -" error.liq)))
  
(rule
 (alias runtest)
 (package liquidsoap)
 (deps
  string.liq
  ../media/all_media_files
  (:liquidsoap ../../src/bin/liquidsoap.exe)
  (:test_liq ../test.liq)
  (:run_test ../run_test.sh))
 (action (run %{run_test} "%{liquidsoap} %{test_liq} -" string.liq)))
  
(rule
 (alias runtest)
 (package liquidsoap)
 (deps
  stdlib.liq
  ../media/all_media_files
  (:liquidsoap ../../src/bin/liquidsoap.exe)
  (:test_liq ../test.liq)
  (:run_test ../run_test.sh))
 (action (run %{run_test} "%{liquidsoap} %{test_liq} -" stdlib.liq)))
  
(rule
 (alias runtest)
 (package liquidsoap)
 (deps
  argsof.liq
  ../media/all_media_files
  (:liquidsoap ../../src/bin/liquidsoap.exe)
  (:test_liq ../test.liq)
  (:run_test ../run_test.sh))
 (action (run %{run_test} "%{liquidsoap} %{test_liq} -" argsof.liq)))
  
(rule
 (alias runtest)
 (package liquidsoap)
 (deps
  file.watch.liq
  ../media/all_media_files
  (:liquidsoap ../../src/bin/liquidsoap.exe)
  (:test_liq ../test.liq)
  (:run_test ../run_test.sh))
 (action (run %{run_test} "%{liquidsoap} %{test_liq} -" file.watch.liq)))
  
(rule
 (alias runtest)
 (package liquidsoap)
 (deps
  interactive.liq
  ../media/all_media_files
  (:liquidsoap ../../src/bin/liquidsoap.exe)
  (:test_liq ../test.liq)
  (:run_test ../run_test.sh))
 (action (run %{run_test} "%{liquidsoap} %{test_liq} -" interactive.liq)))
  
(rule
 (alias runtest)
 (package liquidsoap)
 (deps
  predicate.liq
  ../media/all_media_files
  (:liquidsoap ../../src/bin/liquidsoap.exe)
  (:test_liq ../test.liq)
  (:run_test ../run_test.sh))
 (action (run %{run_test} "%{liquidsoap} %{test_liq} -" predicate.liq)))
  
(rule
 (alias runtest)
 (package liquidsoap)
 (deps
  file.liq
  ../media/all_media_files
  (:liquidsoap ../../src/bin/liquidsoap.exe)
  (:test_liq ../test.liq)
  (:run_test ../run_test.sh))
 (action (run %{run_test} "%{liquidsoap} %{test_liq} -" file.liq)))
  
(rule
 (alias runtest)
 (package liquidsoap)
 (deps
  getter.liq
  ../media/all_media_files
  (:liquidsoap ../../src/bin/liquidsoap.exe)
  (:test_liq ../test.liq)
  (:run_test ../run_test.sh))
 (action (run %{run_test} "%{liquidsoap} %{test_liq} -" getter.liq)))
  
(rule
 (alias runtest)
 (package liquidsoap)
 (deps
  pattern.liq
  ../media/all_media_files
  (:liquidsoap ../../src/bin/liquidsoap.exe)
  (:test_liq ../test.liq)
  (:run_test ../run_test.sh))
 (action (run %{run_test} "%{liquidsoap} %{test_liq} -" pattern.liq)))
  
(rule
 (alias runtest)
 (package liquidsoap)
 (deps
  list.liq
  ../media/all_media_files
  (:liquidsoap ../../src/bin/liquidsoap.exe)
  (:test_liq ../test.liq)
  (:run_test ../run_test.sh))
 (action (run %{run_test} "%{liquidsoap} %{test_liq} -" list.liq)))
  
(rule
 (alias runtest)
 (package liquidsoap)
 (deps
  mem_usage.liq
  ../media/all_media_files
  (:liquidsoap ../../src/bin/liquidsoap.exe)
  (:test_liq ../test.liq)
  (:run_test ../run_test.sh))
 (action (run %{run_test} "%{liquidsoap} %{test_liq} -" mem_usage.liq)))
  
(rule
 (alias runtest)
 (package liquidsoap)
 (deps
  metadata.liq
  ../media/all_media_files
  (:liquidsoap ../../src/bin/liquidsoap.exe)
  (:test_liq ../test.liq)
  (:run_test ../run_test.sh))
 (action (run %{run_test} "%{liquidsoap} %{test_liq} -" metadata.liq)))
  
(rule
 (alias runtest)
 (package liquidsoap)
 (deps
  file.watch2.liq
  ../media/all_media_files
  (:liquidsoap ../../src/bin/liquidsoap.exe)
  (:test_liq ../test.liq)
  (:run_test ../run_test.sh))
 (action (run %{run_test} "%{liquidsoap} %{test_liq} -" file.watch2.liq)))
  
(rule
 (alias runtest)
 (package liquidsoap)
 (deps
  functions.liq
  ../media/all_media_files
  (:liquidsoap ../../src/bin/liquidsoap.exe)
  (:test_liq ../test.liq)
  (:run_test ../run_test.sh))
 (action (run %{run_test} "%{liquidsoap} %{test_liq} -" functions.liq)))
  
(rule
 (alias runtest)
 (package liquidsoap)
 (deps
  record.liq
  ../media/all_media_files
  (:liquidsoap ../../src/bin/liquidsoap.exe)
  (:test_liq ../test.liq)
  (:run_test ../run_test.sh))
 (action (run %{run_test} "%{liquidsoap} %{test_liq} -" record.liq)))
  
(rule
 (alias runtest)
 (package liquidsoap)
 (deps
  file_protocol.liq
  ../media/all_media_files
  (:liquidsoap ../../src/bin/liquidsoap.exe)
  (:test_liq ../test.liq)
  (:run_test ../run_test.sh))
 (action (run %{run_test} "%{liquidsoap} %{test_liq} -" file_protocol.liq)))
  
(rule
 (alias runtest)
 (package liquidsoap)
 (deps
  eval.liq
  ../media/all_media_files
  (:liquidsoap ../../src/bin/liquidsoap.exe)
  (:test_liq ../test.liq)
  (:run_test ../run_test.sh))
 (action (run %{run_test} "%{liquidsoap} %{test_liq} -" eval.liq)))
  
(rule
 (alias runtest)
 (package liquidsoap)
 (deps
  bool.liq
  ../media/all_media_files
  (:liquidsoap ../../src/bin/liquidsoap.exe)
  (:test_liq ../test.liq)
  (:run_test ../run_test.sh))
 (action (run %{run_test} "%{liquidsoap} %{test_liq} -" bool.liq)))
  
(rule
 (alias runtest)
 (package liquidsoap)
 (deps
  json.liq
  ../media/all_media_files
  (:liquidsoap ../../src/bin/liquidsoap.exe)
  (:test_liq ../test.liq)
  (:run_test ../run_test.sh))
 (action (run %{run_test} "%{liquidsoap} %{test_liq} -" json.liq)))
  
(rule
 (alias runtest)
 (package liquidsoap)
 (deps
  encoders.liq
  ../media/all_media_files
  (:liquidsoap ../../src/bin/liquidsoap.exe)
  (:test_liq ../test.liq)
  (:run_test ../run_test.sh))
 (action (run %{run_test} "%{liquidsoap} %{test_liq} -" encoders.liq)))
  
(rule
 (alias runtest)
 (package liquidsoap)
 (deps
  doc.liq
  ../media/all_media_files
  (:liquidsoap ../../src/bin/liquidsoap.exe)
  (:test_liq ../test.liq)
  (:run_test ../run_test.sh))
 (action (run %{run_test} "%{liquidsoap} %{test_liq} -" doc.liq)))
  
(rule
 (alias runtest)
 (package liquidsoap)
 (deps
  typing.liq
  ../media/all_media_files
  (:liquidsoap ../../src/bin/liquidsoap.exe)
  (:test_liq ../test.liq)
  (:run_test ../run_test.sh))
 (action (run %{run_test} "%{liquidsoap} %{test_liq} -" typing.liq)))
  
(rule
 (alias runtest)
 (package liquidsoap)
 (deps
  time.liq
  ../media/all_media_files
  (:liquidsoap ../../src/bin/liquidsoap.exe)
  (:test_liq ../test.liq)
  (:run_test ../run_test.sh))
 (action (run %{run_test} "%{liquidsoap} %{test_liq} -" time.liq)))
  