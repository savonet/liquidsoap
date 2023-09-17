
(rule
  (alias fmt)
  (deps argsof.liq (source_tree ../../src/tooling/prettier) ../../src/tooling/json_dump.exe ../../src/tooling/prettier/node_modules)
  (action
    (progn
      (with-stdout-to argsof.liq.prettier
       (chdir ../../src/tooling/prettier
         (run pnpm prettier --config ./config.json ../../../tests/language/argsof.liq)))
      (diff argsof.liq argsof.liq.prettier))))

(rule
 (alias citest)
 (package liquidsoap)
 (deps
  argsof.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} argsof.liq liquidsoap %{test_liq} argsof.liq)))

(rule
  (alias fmt)
  (deps bool.liq (source_tree ../../src/tooling/prettier) ../../src/tooling/json_dump.exe ../../src/tooling/prettier/node_modules)
  (action
    (progn
      (with-stdout-to bool.liq.prettier
       (chdir ../../src/tooling/prettier
         (run pnpm prettier --config ./config.json ../../../tests/language/bool.liq)))
      (diff bool.liq bool.liq.prettier))))

(rule
 (alias citest)
 (package liquidsoap)
 (deps
  bool.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} bool.liq liquidsoap %{test_liq} bool.liq)))

(rule
  (alias fmt)
  (deps comments.liq (source_tree ../../src/tooling/prettier) ../../src/tooling/json_dump.exe ../../src/tooling/prettier/node_modules)
  (action
    (progn
      (with-stdout-to comments.liq.prettier
       (chdir ../../src/tooling/prettier
         (run pnpm prettier --config ./config.json ../../../tests/language/comments.liq)))
      (diff comments.liq comments.liq.prettier))))

(rule
 (alias citest)
 (package liquidsoap)
 (deps
  comments.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} comments.liq liquidsoap %{test_liq} comments.liq)))

(rule
  (alias fmt)
  (deps conversions.liq (source_tree ../../src/tooling/prettier) ../../src/tooling/json_dump.exe ../../src/tooling/prettier/node_modules)
  (action
    (progn
      (with-stdout-to conversions.liq.prettier
       (chdir ../../src/tooling/prettier
         (run pnpm prettier --config ./config.json ../../../tests/language/conversions.liq)))
      (diff conversions.liq conversions.liq.prettier))))

(rule
 (alias citest)
 (package liquidsoap)
 (deps
  conversions.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} conversions.liq liquidsoap %{test_liq} conversions.liq)))

(rule
  (alias fmt)
  (deps doc.liq (source_tree ../../src/tooling/prettier) ../../src/tooling/json_dump.exe ../../src/tooling/prettier/node_modules)
  (action
    (progn
      (with-stdout-to doc.liq.prettier
       (chdir ../../src/tooling/prettier
         (run pnpm prettier --config ./config.json ../../../tests/language/doc.liq)))
      (diff doc.liq doc.liq.prettier))))

(rule
 (alias citest)
 (package liquidsoap)
 (deps
  doc.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} doc.liq liquidsoap %{test_liq} doc.liq)))

(rule
  (alias fmt)
  (deps encoders.liq (source_tree ../../src/tooling/prettier) ../../src/tooling/json_dump.exe ../../src/tooling/prettier/node_modules)
  (action
    (progn
      (with-stdout-to encoders.liq.prettier
       (chdir ../../src/tooling/prettier
         (run pnpm prettier --config ./config.json ../../../tests/language/encoders.liq)))
      (diff encoders.liq encoders.liq.prettier))))

(rule
 (alias citest)
 (package liquidsoap)
 (deps
  encoders.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} encoders.liq liquidsoap %{test_liq} encoders.liq)))

(rule
  (alias fmt)
  (deps error.liq (source_tree ../../src/tooling/prettier) ../../src/tooling/json_dump.exe ../../src/tooling/prettier/node_modules)
  (action
    (progn
      (with-stdout-to error.liq.prettier
       (chdir ../../src/tooling/prettier
         (run pnpm prettier --config ./config.json ../../../tests/language/error.liq)))
      (diff error.liq error.liq.prettier))))

(rule
 (alias citest)
 (package liquidsoap)
 (deps
  error.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} error.liq liquidsoap %{test_liq} error.liq)))

(rule
  (alias fmt)
  (deps eval.liq (source_tree ../../src/tooling/prettier) ../../src/tooling/json_dump.exe ../../src/tooling/prettier/node_modules)
  (action
    (progn
      (with-stdout-to eval.liq.prettier
       (chdir ../../src/tooling/prettier
         (run pnpm prettier --config ./config.json ../../../tests/language/eval.liq)))
      (diff eval.liq eval.liq.prettier))))

(rule
 (alias citest)
 (package liquidsoap)
 (deps
  eval.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} eval.liq liquidsoap %{test_liq} eval.liq)))

(rule
  (alias fmt)
  (deps file.liq (source_tree ../../src/tooling/prettier) ../../src/tooling/json_dump.exe ../../src/tooling/prettier/node_modules)
  (action
    (progn
      (with-stdout-to file.liq.prettier
       (chdir ../../src/tooling/prettier
         (run pnpm prettier --config ./config.json ../../../tests/language/file.liq)))
      (diff file.liq file.liq.prettier))))

(rule
 (alias citest)
 (package liquidsoap)
 (deps
  file.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} file.liq liquidsoap %{test_liq} file.liq)))

(rule
  (alias fmt)
  (deps file.watch.liq (source_tree ../../src/tooling/prettier) ../../src/tooling/json_dump.exe ../../src/tooling/prettier/node_modules)
  (action
    (progn
      (with-stdout-to file.watch.liq.prettier
       (chdir ../../src/tooling/prettier
         (run pnpm prettier --config ./config.json ../../../tests/language/file.watch.liq)))
      (diff file.watch.liq file.watch.liq.prettier))))

(rule
 (alias citest)
 (package liquidsoap)
 (deps
  file.watch.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} file.watch.liq liquidsoap %{test_liq} file.watch.liq)))

(rule
  (alias fmt)
  (deps file.watch2.liq (source_tree ../../src/tooling/prettier) ../../src/tooling/json_dump.exe ../../src/tooling/prettier/node_modules)
  (action
    (progn
      (with-stdout-to file.watch2.liq.prettier
       (chdir ../../src/tooling/prettier
         (run pnpm prettier --config ./config.json ../../../tests/language/file.watch2.liq)))
      (diff file.watch2.liq file.watch2.liq.prettier))))

(rule
 (alias citest)
 (package liquidsoap)
 (deps
  file.watch2.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} file.watch2.liq liquidsoap %{test_liq} file.watch2.liq)))

(rule
  (alias fmt)
  (deps file_protocol.liq (source_tree ../../src/tooling/prettier) ../../src/tooling/json_dump.exe ../../src/tooling/prettier/node_modules)
  (action
    (progn
      (with-stdout-to file_protocol.liq.prettier
       (chdir ../../src/tooling/prettier
         (run pnpm prettier --config ./config.json ../../../tests/language/file_protocol.liq)))
      (diff file_protocol.liq file_protocol.liq.prettier))))

(rule
 (alias citest)
 (package liquidsoap)
 (deps
  file_protocol.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} file_protocol.liq liquidsoap %{test_liq} file_protocol.liq)))

(rule
  (alias fmt)
  (deps functions.liq (source_tree ../../src/tooling/prettier) ../../src/tooling/json_dump.exe ../../src/tooling/prettier/node_modules)
  (action
    (progn
      (with-stdout-to functions.liq.prettier
       (chdir ../../src/tooling/prettier
         (run pnpm prettier --config ./config.json ../../../tests/language/functions.liq)))
      (diff functions.liq functions.liq.prettier))))

(rule
 (alias citest)
 (package liquidsoap)
 (deps
  functions.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} functions.liq liquidsoap %{test_liq} functions.liq)))

(rule
  (alias fmt)
  (deps getter.liq (source_tree ../../src/tooling/prettier) ../../src/tooling/json_dump.exe ../../src/tooling/prettier/node_modules)
  (action
    (progn
      (with-stdout-to getter.liq.prettier
       (chdir ../../src/tooling/prettier
         (run pnpm prettier --config ./config.json ../../../tests/language/getter.liq)))
      (diff getter.liq getter.liq.prettier))))

(rule
 (alias citest)
 (package liquidsoap)
 (deps
  getter.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} getter.liq liquidsoap %{test_liq} getter.liq)))

(rule
  (alias fmt)
  (deps interactive.liq (source_tree ../../src/tooling/prettier) ../../src/tooling/json_dump.exe ../../src/tooling/prettier/node_modules)
  (action
    (progn
      (with-stdout-to interactive.liq.prettier
       (chdir ../../src/tooling/prettier
         (run pnpm prettier --config ./config.json ../../../tests/language/interactive.liq)))
      (diff interactive.liq interactive.liq.prettier))))

(rule
 (alias citest)
 (package liquidsoap)
 (deps
  interactive.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} interactive.liq liquidsoap %{test_liq} interactive.liq)))

(rule
  (alias fmt)
  (deps json.liq (source_tree ../../src/tooling/prettier) ../../src/tooling/json_dump.exe ../../src/tooling/prettier/node_modules)
  (action
    (progn
      (with-stdout-to json.liq.prettier
       (chdir ../../src/tooling/prettier
         (run pnpm prettier --config ./config.json ../../../tests/language/json.liq)))
      (diff json.liq json.liq.prettier))))

(rule
 (alias citest)
 (package liquidsoap)
 (deps
  json.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} json.liq liquidsoap %{test_liq} json.liq)))

(rule
  (alias fmt)
  (deps list.liq (source_tree ../../src/tooling/prettier) ../../src/tooling/json_dump.exe ../../src/tooling/prettier/node_modules)
  (action
    (progn
      (with-stdout-to list.liq.prettier
       (chdir ../../src/tooling/prettier
         (run pnpm prettier --config ./config.json ../../../tests/language/list.liq)))
      (diff list.liq list.liq.prettier))))

(rule
 (alias citest)
 (package liquidsoap)
 (deps
  list.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} list.liq liquidsoap %{test_liq} list.liq)))

(rule
  (alias fmt)
  (deps loop.liq (source_tree ../../src/tooling/prettier) ../../src/tooling/json_dump.exe ../../src/tooling/prettier/node_modules)
  (action
    (progn
      (with-stdout-to loop.liq.prettier
       (chdir ../../src/tooling/prettier
         (run pnpm prettier --config ./config.json ../../../tests/language/loop.liq)))
      (diff loop.liq loop.liq.prettier))))

(rule
 (alias citest)
 (package liquidsoap)
 (deps
  loop.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} loop.liq liquidsoap %{test_liq} loop.liq)))

(rule
  (alias fmt)
  (deps math.liq (source_tree ../../src/tooling/prettier) ../../src/tooling/json_dump.exe ../../src/tooling/prettier/node_modules)
  (action
    (progn
      (with-stdout-to math.liq.prettier
       (chdir ../../src/tooling/prettier
         (run pnpm prettier --config ./config.json ../../../tests/language/math.liq)))
      (diff math.liq math.liq.prettier))))

(rule
 (alias citest)
 (package liquidsoap)
 (deps
  math.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} math.liq liquidsoap %{test_liq} math.liq)))

(rule
  (alias fmt)
  (deps mem_usage.liq (source_tree ../../src/tooling/prettier) ../../src/tooling/json_dump.exe ../../src/tooling/prettier/node_modules)
  (action
    (progn
      (with-stdout-to mem_usage.liq.prettier
       (chdir ../../src/tooling/prettier
         (run pnpm prettier --config ./config.json ../../../tests/language/mem_usage.liq)))
      (diff mem_usage.liq mem_usage.liq.prettier))))

(rule
 (alias citest)
 (package liquidsoap)
 (deps
  mem_usage.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} mem_usage.liq liquidsoap %{test_liq} mem_usage.liq)))

(rule
  (alias fmt)
  (deps metadata.liq (source_tree ../../src/tooling/prettier) ../../src/tooling/json_dump.exe ../../src/tooling/prettier/node_modules)
  (action
    (progn
      (with-stdout-to metadata.liq.prettier
       (chdir ../../src/tooling/prettier
         (run pnpm prettier --config ./config.json ../../../tests/language/metadata.liq)))
      (diff metadata.liq metadata.liq.prettier))))

(rule
 (alias citest)
 (package liquidsoap)
 (deps
  metadata.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} metadata.liq liquidsoap %{test_liq} metadata.liq)))

(rule
  (alias fmt)
  (deps metrics.liq (source_tree ../../src/tooling/prettier) ../../src/tooling/json_dump.exe ../../src/tooling/prettier/node_modules)
  (action
    (progn
      (with-stdout-to metrics.liq.prettier
       (chdir ../../src/tooling/prettier
         (run pnpm prettier --config ./config.json ../../../tests/language/metrics.liq)))
      (diff metrics.liq metrics.liq.prettier))))

(rule
 (alias citest)
 (package liquidsoap)
 (deps
  metrics.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} metrics.liq liquidsoap %{test_liq} metrics.liq)))

(rule
  (alias fmt)
  (deps null.liq (source_tree ../../src/tooling/prettier) ../../src/tooling/json_dump.exe ../../src/tooling/prettier/node_modules)
  (action
    (progn
      (with-stdout-to null.liq.prettier
       (chdir ../../src/tooling/prettier
         (run pnpm prettier --config ./config.json ../../../tests/language/null.liq)))
      (diff null.liq null.liq.prettier))))

(rule
 (alias citest)
 (package liquidsoap)
 (deps
  null.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} null.liq liquidsoap %{test_liq} null.liq)))

(rule
  (alias fmt)
  (deps number.liq (source_tree ../../src/tooling/prettier) ../../src/tooling/json_dump.exe ../../src/tooling/prettier/node_modules)
  (action
    (progn
      (with-stdout-to number.liq.prettier
       (chdir ../../src/tooling/prettier
         (run pnpm prettier --config ./config.json ../../../tests/language/number.liq)))
      (diff number.liq number.liq.prettier))))

(rule
 (alias citest)
 (package liquidsoap)
 (deps
  number.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} number.liq liquidsoap %{test_liq} number.liq)))

(rule
  (alias fmt)
  (deps osc.liq (source_tree ../../src/tooling/prettier) ../../src/tooling/json_dump.exe ../../src/tooling/prettier/node_modules)
  (action
    (progn
      (with-stdout-to osc.liq.prettier
       (chdir ../../src/tooling/prettier
         (run pnpm prettier --config ./config.json ../../../tests/language/osc.liq)))
      (diff osc.liq osc.liq.prettier))))

(rule
 (alias citest)
 (package liquidsoap)
 (deps
  osc.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} osc.liq liquidsoap %{test_liq} osc.liq)))

(rule
  (alias fmt)
  (deps pattern.liq (source_tree ../../src/tooling/prettier) ../../src/tooling/json_dump.exe ../../src/tooling/prettier/node_modules)
  (action
    (progn
      (with-stdout-to pattern.liq.prettier
       (chdir ../../src/tooling/prettier
         (run pnpm prettier --config ./config.json ../../../tests/language/pattern.liq)))
      (diff pattern.liq pattern.liq.prettier))))

(rule
 (alias citest)
 (package liquidsoap)
 (deps
  pattern.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} pattern.liq liquidsoap %{test_liq} pattern.liq)))

(rule
  (alias fmt)
  (deps pp.liq (source_tree ../../src/tooling/prettier) ../../src/tooling/json_dump.exe ../../src/tooling/prettier/node_modules)
  (action
    (progn
      (with-stdout-to pp.liq.prettier
       (chdir ../../src/tooling/prettier
         (run pnpm prettier --config ./config.json ../../../tests/language/pp.liq)))
      (diff pp.liq pp.liq.prettier))))

(rule
 (alias citest)
 (package liquidsoap)
 (deps
  pp.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} pp.liq liquidsoap %{test_liq} pp.liq)))

(rule
  (alias fmt)
  (deps predicate.liq (source_tree ../../src/tooling/prettier) ../../src/tooling/json_dump.exe ../../src/tooling/prettier/node_modules)
  (action
    (progn
      (with-stdout-to predicate.liq.prettier
       (chdir ../../src/tooling/prettier
         (run pnpm prettier --config ./config.json ../../../tests/language/predicate.liq)))
      (diff predicate.liq predicate.liq.prettier))))

(rule
 (alias citest)
 (package liquidsoap)
 (deps
  predicate.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} predicate.liq liquidsoap %{test_liq} predicate.liq)))

(rule
  (alias fmt)
  (deps process.liq (source_tree ../../src/tooling/prettier) ../../src/tooling/json_dump.exe ../../src/tooling/prettier/node_modules)
  (action
    (progn
      (with-stdout-to process.liq.prettier
       (chdir ../../src/tooling/prettier
         (run pnpm prettier --config ./config.json ../../../tests/language/process.liq)))
      (diff process.liq process.liq.prettier))))

(rule
 (alias citest)
 (package liquidsoap)
 (deps
  process.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} process.liq liquidsoap %{test_liq} process.liq)))

(rule
  (alias fmt)
  (deps rec.liq (source_tree ../../src/tooling/prettier) ../../src/tooling/json_dump.exe ../../src/tooling/prettier/node_modules)
  (action
    (progn
      (with-stdout-to rec.liq.prettier
       (chdir ../../src/tooling/prettier
         (run pnpm prettier --config ./config.json ../../../tests/language/rec.liq)))
      (diff rec.liq rec.liq.prettier))))

(rule
 (alias citest)
 (package liquidsoap)
 (deps
  rec.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} rec.liq liquidsoap %{test_liq} rec.liq)))

(rule
  (alias fmt)
  (deps record.liq (source_tree ../../src/tooling/prettier) ../../src/tooling/json_dump.exe ../../src/tooling/prettier/node_modules)
  (action
    (progn
      (with-stdout-to record.liq.prettier
       (chdir ../../src/tooling/prettier
         (run pnpm prettier --config ./config.json ../../../tests/language/record.liq)))
      (diff record.liq record.liq.prettier))))

(rule
 (alias citest)
 (package liquidsoap)
 (deps
  record.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} record.liq liquidsoap %{test_liq} record.liq)))

(rule
  (alias fmt)
  (deps ref.liq (source_tree ../../src/tooling/prettier) ../../src/tooling/json_dump.exe ../../src/tooling/prettier/node_modules)
  (action
    (progn
      (with-stdout-to ref.liq.prettier
       (chdir ../../src/tooling/prettier
         (run pnpm prettier --config ./config.json ../../../tests/language/ref.liq)))
      (diff ref.liq ref.liq.prettier))))

(rule
 (alias citest)
 (package liquidsoap)
 (deps
  ref.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} ref.liq liquidsoap %{test_liq} ref.liq)))

(rule
  (alias fmt)
  (deps regexp.liq (source_tree ../../src/tooling/prettier) ../../src/tooling/json_dump.exe ../../src/tooling/prettier/node_modules)
  (action
    (progn
      (with-stdout-to regexp.liq.prettier
       (chdir ../../src/tooling/prettier
         (run pnpm prettier --config ./config.json ../../../tests/language/regexp.liq)))
      (diff regexp.liq regexp.liq.prettier))))

(rule
 (alias citest)
 (package liquidsoap)
 (deps
  regexp.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} regexp.liq liquidsoap %{test_liq} regexp.liq)))

(rule
  (alias fmt)
  (deps socket.liq (source_tree ../../src/tooling/prettier) ../../src/tooling/json_dump.exe ../../src/tooling/prettier/node_modules)
  (action
    (progn
      (with-stdout-to socket.liq.prettier
       (chdir ../../src/tooling/prettier
         (run pnpm prettier --config ./config.json ../../../tests/language/socket.liq)))
      (diff socket.liq socket.liq.prettier))))

(rule
 (alias citest)
 (package liquidsoap)
 (deps
  socket.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} socket.liq liquidsoap %{test_liq} socket.liq)))

(rule
  (alias fmt)
  (deps stdlib.liq (source_tree ../../src/tooling/prettier) ../../src/tooling/json_dump.exe ../../src/tooling/prettier/node_modules)
  (action
    (progn
      (with-stdout-to stdlib.liq.prettier
       (chdir ../../src/tooling/prettier
         (run pnpm prettier --config ./config.json ../../../tests/language/stdlib.liq)))
      (diff stdlib.liq stdlib.liq.prettier))))

(rule
 (alias citest)
 (package liquidsoap)
 (deps
  stdlib.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} stdlib.liq liquidsoap %{test_liq} stdlib.liq)))

(rule
  (alias fmt)
  (deps string.liq (source_tree ../../src/tooling/prettier) ../../src/tooling/json_dump.exe ../../src/tooling/prettier/node_modules)
  (action
    (progn
      (with-stdout-to string.liq.prettier
       (chdir ../../src/tooling/prettier
         (run pnpm prettier --config ./config.json ../../../tests/language/string.liq)))
      (diff string.liq string.liq.prettier))))

(rule
 (alias citest)
 (package liquidsoap)
 (deps
  string.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} string.liq liquidsoap %{test_liq} string.liq)))

(rule
  (alias fmt)
  (deps thread.liq (source_tree ../../src/tooling/prettier) ../../src/tooling/json_dump.exe ../../src/tooling/prettier/node_modules)
  (action
    (progn
      (with-stdout-to thread.liq.prettier
       (chdir ../../src/tooling/prettier
         (run pnpm prettier --config ./config.json ../../../tests/language/thread.liq)))
      (diff thread.liq thread.liq.prettier))))

(rule
 (alias citest)
 (package liquidsoap)
 (deps
  thread.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} thread.liq liquidsoap %{test_liq} thread.liq)))

(rule
  (alias fmt)
  (deps time.liq (source_tree ../../src/tooling/prettier) ../../src/tooling/json_dump.exe ../../src/tooling/prettier/node_modules)
  (action
    (progn
      (with-stdout-to time.liq.prettier
       (chdir ../../src/tooling/prettier
         (run pnpm prettier --config ./config.json ../../../tests/language/time.liq)))
      (diff time.liq time.liq.prettier))))

(rule
 (alias citest)
 (package liquidsoap)
 (deps
  time.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} time.liq liquidsoap %{test_liq} time.liq)))

(rule
  (alias fmt)
  (deps type_errors.liq (source_tree ../../src/tooling/prettier) ../../src/tooling/json_dump.exe ../../src/tooling/prettier/node_modules)
  (action
    (progn
      (with-stdout-to type_errors.liq.prettier
       (chdir ../../src/tooling/prettier
         (run pnpm prettier --config ./config.json ../../../tests/language/type_errors.liq)))
      (diff type_errors.liq type_errors.liq.prettier))))

(rule
 (alias citest)
 (package liquidsoap)
 (deps
  type_errors.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} type_errors.liq liquidsoap %{test_liq} type_errors.liq)))

(rule
  (alias fmt)
  (deps typing.liq (source_tree ../../src/tooling/prettier) ../../src/tooling/json_dump.exe ../../src/tooling/prettier/node_modules)
  (action
    (progn
      (with-stdout-to typing.liq.prettier
       (chdir ../../src/tooling/prettier
         (run pnpm prettier --config ./config.json ../../../tests/language/typing.liq)))
      (diff typing.liq typing.liq.prettier))))

(rule
 (alias citest)
 (package liquidsoap)
 (deps
  typing.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} typing.liq liquidsoap %{test_liq} typing.liq)))

(rule
  (alias fmt)
  (deps url.liq (source_tree ../../src/tooling/prettier) ../../src/tooling/json_dump.exe ../../src/tooling/prettier/node_modules)
  (action
    (progn
      (with-stdout-to url.liq.prettier
       (chdir ../../src/tooling/prettier
         (run pnpm prettier --config ./config.json ../../../tests/language/url.liq)))
      (diff url.liq url.liq.prettier))))

(rule
 (alias citest)
 (package liquidsoap)
 (deps
  url.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} url.liq liquidsoap %{test_liq} url.liq)))

(rule
  (alias fmt)
  (deps yaml.liq (source_tree ../../src/tooling/prettier) ../../src/tooling/json_dump.exe ../../src/tooling/prettier/node_modules)
  (action
    (progn
      (with-stdout-to yaml.liq.prettier
       (chdir ../../src/tooling/prettier
         (run pnpm prettier --config ./config.json ../../../tests/language/yaml.liq)))
      (diff yaml.liq yaml.liq.prettier))))

(rule
 (alias citest)
 (package liquidsoap)
 (deps
  yaml.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (:stdlib ../../src/libs/stdlib.liq)
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} yaml.liq liquidsoap %{test_liq} yaml.liq)))
