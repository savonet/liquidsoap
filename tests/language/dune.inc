(rule
 (alias test_argsof)
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
 (alias test_bool)
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
 (alias test_comments)
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
 (alias test_conversions)
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
 (alias test_cron)
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
 (alias test_cue_test)
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
 (alias test_doc)
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
 (alias test_encoders)
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
 (alias test_error)
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
 (alias test_eval)
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
 (alias test_file)
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
 (alias test_file.watch)
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
 (alias test_file.watch2)
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
 (alias test_file_protocol)
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
 (alias test_functions)
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
 (alias test_getter)
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
 (alias test_implicit_float_casting)
 (package liquidsoap)
 (deps
  implicit_float_casting.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (source_tree ../../src/libs)
  crontab_test_cases.json
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run
   %{run_test}
   implicit_float_casting.liq
   liquidsoap
   %{test_liq}
   implicit_float_casting.liq)))

(rule
 (alias test_interactive)
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
 (alias test_json)
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
 (alias test_list)
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
 (alias test_loop)
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
 (alias test_math)
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
 (alias test_mem_usage)
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
 (alias test_metadata)
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
 (alias test_metrics)
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
 (alias test_null)
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
 (alias test_number)
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
 (alias test_osc)
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
 (alias test_pattern)
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
 (alias test_pp)
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
 (alias test_predicate)
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
 (alias test_process)
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
 (alias test_rec)
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
 (alias test_record)
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
 (alias test_ref)
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
 (alias test_regexp)
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
 (alias test_replaygain)
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
 (alias test_socket)
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
 (alias test_source_content)
 (package liquidsoap)
 (deps
  source_content.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (source_tree ../../src/libs)
  crontab_test_cases.json
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run
   %{run_test}
   source_content.liq
   liquidsoap
   %{test_liq}
   source_content.liq)))

(rule
 (alias test_sqlite)
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
 (alias test_stdlib)
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
 (alias test_string)
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
 (alias test_thread)
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
 (alias test_time)
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
 (alias test_type_errors)
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
 (alias test_typing)
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
 (alias test_url)
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
 (alias test_various)
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
 (alias test_xml_test)
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
 (alias test_yaml)
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
  (alias test_argsof)
  (alias test_bool)
  (alias test_comments)
  (alias test_conversions)
  (alias test_cron)
  (alias test_cue_test)
  (alias test_doc)
  (alias test_encoders)
  (alias test_error)
  (alias test_eval)
  (alias test_file)
  (alias test_file.watch)
  (alias test_file.watch2)
  (alias test_file_protocol)
  (alias test_functions)
  (alias test_getter)
  (alias test_implicit_float_casting)
  (alias test_interactive)
  (alias test_json)
  (alias test_list)
  (alias test_loop)
  (alias test_math)
  (alias test_mem_usage)
  (alias test_metadata)
  (alias test_metrics)
  (alias test_null)
  (alias test_number)
  (alias test_osc)
  (alias test_pattern)
  (alias test_pp)
  (alias test_predicate)
  (alias test_process)
  (alias test_rec)
  (alias test_record)
  (alias test_ref)
  (alias test_regexp)
  (alias test_replaygain)
  (alias test_socket)
  (alias test_source_content)
  (alias test_sqlite)
  (alias test_stdlib)
  (alias test_string)
  (alias test_thread)
  (alias test_time)
  (alias test_type_errors)
  (alias test_typing)
  (alias test_url)
  (alias test_various)
  (alias test_xml_test)
  (alias test_yaml)))
