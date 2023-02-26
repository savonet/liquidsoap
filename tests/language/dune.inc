(rule
 (alias argsof)
 (package liquidsoap)
 (deps
  argsof.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (source_tree ../../src/libs)
  crontab_test_cases.json
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} argsof.liq liquidsoap %{test_liq} argsof.liq)))

(rule
 (alias bool)
 (package liquidsoap)
 (deps
  bool.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (source_tree ../../src/libs)
  crontab_test_cases.json
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} bool.liq liquidsoap %{test_liq} bool.liq)))

(rule
 (alias comments)
 (package liquidsoap)
 (deps
  casting.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (source_tree ../../src/libs)
  crontab_test_cases.json
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action (run %{run_test} casting.liq liquidsoap %{test_liq} casting.liq)))
  
(rule
 (alias citest)
 (package liquidsoap)
 (deps
  comments.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (source_tree ../../src/libs)
  crontab_test_cases.json
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} comments.liq liquidsoap %{test_liq} comments.liq)))

(rule
 (alias conversions)
 (package liquidsoap)
 (deps
  conversions.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (source_tree ../../src/libs)
  crontab_test_cases.json
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} conversions.liq liquidsoap %{test_liq} conversions.liq)))

(rule
 (alias cron)
 (package liquidsoap)
 (deps
  cron.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (source_tree ../../src/libs)
  crontab_test_cases.json
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} cron.liq liquidsoap %{test_liq} cron.liq)))

(rule
 (alias cue_test)
 (package liquidsoap)
 (deps
  cue_test.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (source_tree ../../src/libs)
  crontab_test_cases.json
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} cue_test.liq liquidsoap %{test_liq} cue_test.liq)))

(rule
 (alias doc)
 (package liquidsoap)
 (deps
  doc.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (source_tree ../../src/libs)
  crontab_test_cases.json
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} doc.liq liquidsoap %{test_liq} doc.liq)))

(rule
 (alias encoders)
 (package liquidsoap)
 (deps
  encoders.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (source_tree ../../src/libs)
  crontab_test_cases.json
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} encoders.liq liquidsoap %{test_liq} encoders.liq)))

(rule
 (alias error)
 (package liquidsoap)
 (deps
  error.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (source_tree ../../src/libs)
  crontab_test_cases.json
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} error.liq liquidsoap %{test_liq} error.liq)))

(rule
 (alias eval)
 (package liquidsoap)
 (deps
  eval.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (source_tree ../../src/libs)
  crontab_test_cases.json
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} eval.liq liquidsoap %{test_liq} eval.liq)))

(rule
 (alias file)
 (package liquidsoap)
 (deps
  file.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (source_tree ../../src/libs)
  crontab_test_cases.json
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} file.liq liquidsoap %{test_liq} file.liq)))

(rule
 (alias file.watch)
 (package liquidsoap)
 (deps
  file.watch.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (source_tree ../../src/libs)
  crontab_test_cases.json
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} file.watch.liq liquidsoap %{test_liq} file.watch.liq)))

(rule
 (alias file.watch2)
 (package liquidsoap)
 (deps
  file.watch2.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (source_tree ../../src/libs)
  crontab_test_cases.json
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} file.watch2.liq liquidsoap %{test_liq} file.watch2.liq)))

(rule
 (alias file_protocol)
 (package liquidsoap)
 (deps
  file_protocol.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (source_tree ../../src/libs)
  crontab_test_cases.json
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run
   %{run_test}
   file_protocol.liq
   liquidsoap
   %{test_liq}
   file_protocol.liq)))

(rule
 (alias functions)
 (package liquidsoap)
 (deps
  functions.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (source_tree ../../src/libs)
  crontab_test_cases.json
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} functions.liq liquidsoap %{test_liq} functions.liq)))

(rule
 (alias getter)
 (package liquidsoap)
 (deps
  getter.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (source_tree ../../src/libs)
  crontab_test_cases.json
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} getter.liq liquidsoap %{test_liq} getter.liq)))

(rule
 (alias interactive)
 (package liquidsoap)
 (deps
  interactive.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (source_tree ../../src/libs)
  crontab_test_cases.json
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} interactive.liq liquidsoap %{test_liq} interactive.liq)))

(rule
 (alias json)
 (package liquidsoap)
 (deps
  json.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (source_tree ../../src/libs)
  crontab_test_cases.json
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} json.liq liquidsoap %{test_liq} json.liq)))

(rule
 (alias list)
 (package liquidsoap)
 (deps
  list.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (source_tree ../../src/libs)
  crontab_test_cases.json
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} list.liq liquidsoap %{test_liq} list.liq)))

(rule
 (alias loop)
 (package liquidsoap)
 (deps
  loop.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (source_tree ../../src/libs)
  crontab_test_cases.json
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} loop.liq liquidsoap %{test_liq} loop.liq)))

(rule
 (alias math)
 (package liquidsoap)
 (deps
  math.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (source_tree ../../src/libs)
  crontab_test_cases.json
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} math.liq liquidsoap %{test_liq} math.liq)))

(rule
 (alias mem_usage)
 (package liquidsoap)
 (deps
  mem_usage.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (source_tree ../../src/libs)
  crontab_test_cases.json
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} mem_usage.liq liquidsoap %{test_liq} mem_usage.liq)))

(rule
 (alias metadata)
 (package liquidsoap)
 (deps
  metadata.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (source_tree ../../src/libs)
  crontab_test_cases.json
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} metadata.liq liquidsoap %{test_liq} metadata.liq)))

(rule
 (alias metrics)
 (package liquidsoap)
 (deps
  metrics.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (source_tree ../../src/libs)
  crontab_test_cases.json
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} metrics.liq liquidsoap %{test_liq} metrics.liq)))

(rule
 (alias null)
 (package liquidsoap)
 (deps
  null.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (source_tree ../../src/libs)
  crontab_test_cases.json
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} null.liq liquidsoap %{test_liq} null.liq)))

(rule
 (alias number)
 (package liquidsoap)
 (deps
  number.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (source_tree ../../src/libs)
  crontab_test_cases.json
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} number.liq liquidsoap %{test_liq} number.liq)))

(rule
 (alias osc)
 (package liquidsoap)
 (deps
  osc.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (source_tree ../../src/libs)
  crontab_test_cases.json
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} osc.liq liquidsoap %{test_liq} osc.liq)))

(rule
 (alias pattern)
 (package liquidsoap)
 (deps
  pattern.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (source_tree ../../src/libs)
  crontab_test_cases.json
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} pattern.liq liquidsoap %{test_liq} pattern.liq)))

(rule
 (alias pp)
 (package liquidsoap)
 (deps
  pp.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (source_tree ../../src/libs)
  crontab_test_cases.json
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} pp.liq liquidsoap %{test_liq} pp.liq)))

(rule
 (alias predicate)
 (package liquidsoap)
 (deps
  predicate.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (source_tree ../../src/libs)
  crontab_test_cases.json
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} predicate.liq liquidsoap %{test_liq} predicate.liq)))

(rule
 (alias process)
 (package liquidsoap)
 (deps
  process.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (source_tree ../../src/libs)
  crontab_test_cases.json
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} process.liq liquidsoap %{test_liq} process.liq)))

(rule
 (alias rec)
 (package liquidsoap)
 (deps
  rec.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (source_tree ../../src/libs)
  crontab_test_cases.json
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} rec.liq liquidsoap %{test_liq} rec.liq)))

(rule
 (alias record)
 (package liquidsoap)
 (deps
  record.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (source_tree ../../src/libs)
  crontab_test_cases.json
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} record.liq liquidsoap %{test_liq} record.liq)))

(rule
 (alias ref)
 (package liquidsoap)
 (deps
  ref.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (source_tree ../../src/libs)
  crontab_test_cases.json
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} ref.liq liquidsoap %{test_liq} ref.liq)))

(rule
 (alias regexp)
 (package liquidsoap)
 (deps
  regexp.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (source_tree ../../src/libs)
  crontab_test_cases.json
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} regexp.liq liquidsoap %{test_liq} regexp.liq)))

(rule
 (alias replaygain)
 (package liquidsoap)
 (deps
  replaygain.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (source_tree ../../src/libs)
  crontab_test_cases.json
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} replaygain.liq liquidsoap %{test_liq} replaygain.liq)))

(rule
 (alias socket)
 (package liquidsoap)
 (deps
  socket.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (source_tree ../../src/libs)
  crontab_test_cases.json
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} socket.liq liquidsoap %{test_liq} socket.liq)))

(rule
 (alias sqlite)
 (package liquidsoap)
 (deps
  sqlite.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (source_tree ../../src/libs)
  crontab_test_cases.json
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} sqlite.liq liquidsoap %{test_liq} sqlite.liq)))

(rule
 (alias stdlib)
 (package liquidsoap)
 (deps
  stdlib.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (source_tree ../../src/libs)
  crontab_test_cases.json
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} stdlib.liq liquidsoap %{test_liq} stdlib.liq)))

(rule
 (alias string)
 (package liquidsoap)
 (deps
  string.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (source_tree ../../src/libs)
  crontab_test_cases.json
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} string.liq liquidsoap %{test_liq} string.liq)))

(rule
 (alias thread)
 (package liquidsoap)
 (deps
  thread.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (source_tree ../../src/libs)
  crontab_test_cases.json
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} thread.liq liquidsoap %{test_liq} thread.liq)))

(rule
 (alias time)
 (package liquidsoap)
 (deps
  time.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (source_tree ../../src/libs)
  crontab_test_cases.json
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} time.liq liquidsoap %{test_liq} time.liq)))

(rule
 (alias type_errors)
 (package liquidsoap)
 (deps
  type_errors.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (source_tree ../../src/libs)
  crontab_test_cases.json
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} type_errors.liq liquidsoap %{test_liq} type_errors.liq)))

(rule
 (alias typing)
 (package liquidsoap)
 (deps
  typing.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (source_tree ../../src/libs)
  crontab_test_cases.json
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} typing.liq liquidsoap %{test_liq} typing.liq)))

(rule
 (alias url)
 (package liquidsoap)
 (deps
  url.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (source_tree ../../src/libs)
  crontab_test_cases.json
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} url.liq liquidsoap %{test_liq} url.liq)))

(rule
 (alias various)
 (package liquidsoap)
 (deps
  various.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (source_tree ../../src/libs)
  crontab_test_cases.json
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} various.liq liquidsoap %{test_liq} various.liq)))

(rule
 (alias xml_test)
 (package liquidsoap)
 (deps
  xml_test.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (source_tree ../../src/libs)
  crontab_test_cases.json
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} xml_test.liq liquidsoap %{test_liq} xml_test.liq)))

(rule
 (alias yaml)
 (package liquidsoap)
 (deps
  yaml.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (source_tree ../../src/libs)
  crontab_test_cases.json
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} yaml.liq liquidsoap %{test_liq} yaml.liq)))

(alias
 (name citest)
 (deps
  (alias argsof)
  (alias bool)
  (alias comments)
  (alias conversions)
  (alias cron)
  (alias cue_test)
  (alias doc)
  (alias encoders)
  (alias error)
  (alias eval)
  (alias file)
  (alias file.watch)
  (alias file.watch2)
  (alias file_protocol)
  (alias functions)
  (alias getter)
  (alias interactive)
  (alias json)
  (alias list)
  (alias loop)
  (alias math)
  (alias mem_usage)
  (alias metadata)
  (alias metrics)
  (alias null)
  (alias number)
  (alias osc)
  (alias pattern)
  (alias pp)
  (alias predicate)
  (alias process)
  (alias rec)
  (alias record)
  (alias ref)
  (alias regexp)
  (alias replaygain)
  (alias socket)
  (alias sqlite)
  (alias stdlib)
  (alias string)
  (alias thread)
  (alias time)
  (alias type_errors)
  (alias typing)
  (alias url)
  (alias various)
  (alias xml_test)
  (alias yaml)))
