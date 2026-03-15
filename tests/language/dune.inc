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
 (alias test_argsof)
 (package liquidsoap)
 (deps
  test_argsof.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (source_tree ../../src/libs)
  crontab_test_cases.json
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} test_argsof.liq liquidsoap %{test_liq} test_argsof.liq)))

(rule
 (alias test_bool)
 (package liquidsoap)
 (deps
  test_bool.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (source_tree ../../src/libs)
  crontab_test_cases.json
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} test_bool.liq liquidsoap %{test_liq} test_bool.liq)))

(rule
 (alias test_comments)
 (package liquidsoap)
 (deps
  test_comments.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (source_tree ../../src/libs)
  crontab_test_cases.json
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run
   %{run_test}
   test_comments.liq
   liquidsoap
   %{test_liq}
   test_comments.liq)))

(rule
 (alias test_conversions)
 (package liquidsoap)
 (deps
  test_conversions.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (source_tree ../../src/libs)
  crontab_test_cases.json
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run
   %{run_test}
   test_conversions.liq
   liquidsoap
   %{test_liq}
   test_conversions.liq)))

(rule
 (alias test_cron)
 (package liquidsoap)
 (deps
  test_cron.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (source_tree ../../src/libs)
  crontab_test_cases.json
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} test_cron.liq liquidsoap %{test_liq} test_cron.liq)))

(rule
 (alias test_doc)
 (package liquidsoap)
 (deps
  test_doc.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (source_tree ../../src/libs)
  crontab_test_cases.json
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} test_doc.liq liquidsoap %{test_liq} test_doc.liq)))

(rule
 (alias test_encoders)
 (package liquidsoap)
 (deps
  test_encoders.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (source_tree ../../src/libs)
  crontab_test_cases.json
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run
   %{run_test}
   test_encoders.liq
   liquidsoap
   %{test_liq}
   test_encoders.liq)))

(rule
 (alias test_error)
 (package liquidsoap)
 (deps
  test_error.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (source_tree ../../src/libs)
  crontab_test_cases.json
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} test_error.liq liquidsoap %{test_liq} test_error.liq)))

(rule
 (alias test_eval)
 (package liquidsoap)
 (deps
  test_eval.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (source_tree ../../src/libs)
  crontab_test_cases.json
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} test_eval.liq liquidsoap %{test_liq} test_eval.liq)))

(rule
 (alias test_file)
 (package liquidsoap)
 (deps
  test_file.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (source_tree ../../src/libs)
  crontab_test_cases.json
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} test_file.liq liquidsoap %{test_liq} test_file.liq)))

(rule
 (alias test_file.watch)
 (package liquidsoap)
 (deps
  test_file.watch.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (source_tree ../../src/libs)
  crontab_test_cases.json
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run
   %{run_test}
   test_file.watch.liq
   liquidsoap
   %{test_liq}
   test_file.watch.liq)))

(rule
 (alias test_file.watch2)
 (package liquidsoap)
 (deps
  test_file.watch2.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (source_tree ../../src/libs)
  crontab_test_cases.json
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run
   %{run_test}
   test_file.watch2.liq
   liquidsoap
   %{test_liq}
   test_file.watch2.liq)))

(rule
 (alias test_file_protocol)
 (package liquidsoap)
 (deps
  test_file_protocol.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (source_tree ../../src/libs)
  crontab_test_cases.json
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run
   %{run_test}
   test_file_protocol.liq
   liquidsoap
   %{test_liq}
   test_file_protocol.liq)))

(rule
 (alias test_functions)
 (package liquidsoap)
 (deps
  test_functions.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (source_tree ../../src/libs)
  crontab_test_cases.json
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run
   %{run_test}
   test_functions.liq
   liquidsoap
   %{test_liq}
   test_functions.liq)))

(rule
 (alias test_getter)
 (package liquidsoap)
 (deps
  test_getter.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (source_tree ../../src/libs)
  crontab_test_cases.json
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} test_getter.liq liquidsoap %{test_liq} test_getter.liq)))

(rule
 (alias test_implicit_float_casting)
 (package liquidsoap)
 (deps
  test_implicit_float_casting.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (source_tree ../../src/libs)
  crontab_test_cases.json
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run
   %{run_test}
   test_implicit_float_casting.liq
   liquidsoap
   %{test_liq}
   test_implicit_float_casting.liq)))

(rule
 (alias test_interactive)
 (package liquidsoap)
 (deps
  test_interactive.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (source_tree ../../src/libs)
  crontab_test_cases.json
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run
   %{run_test}
   test_interactive.liq
   liquidsoap
   %{test_liq}
   test_interactive.liq)))

(rule
 (alias test_json)
 (package liquidsoap)
 (deps
  test_json.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (source_tree ../../src/libs)
  crontab_test_cases.json
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} test_json.liq liquidsoap %{test_liq} test_json.liq)))

(rule
 (alias test_list)
 (package liquidsoap)
 (deps
  test_list.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (source_tree ../../src/libs)
  crontab_test_cases.json
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} test_list.liq liquidsoap %{test_liq} test_list.liq)))

(rule
 (alias test_loop)
 (package liquidsoap)
 (deps
  test_loop.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (source_tree ../../src/libs)
  crontab_test_cases.json
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} test_loop.liq liquidsoap %{test_liq} test_loop.liq)))

(rule
 (alias test_math)
 (package liquidsoap)
 (deps
  test_math.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (source_tree ../../src/libs)
  crontab_test_cases.json
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} test_math.liq liquidsoap %{test_liq} test_math.liq)))

(rule
 (alias test_mem_usage)
 (package liquidsoap)
 (deps
  test_mem_usage.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (source_tree ../../src/libs)
  crontab_test_cases.json
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run
   %{run_test}
   test_mem_usage.liq
   liquidsoap
   %{test_liq}
   test_mem_usage.liq)))

(rule
 (alias test_metadata)
 (package liquidsoap)
 (deps
  test_metadata.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (source_tree ../../src/libs)
  crontab_test_cases.json
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run
   %{run_test}
   test_metadata.liq
   liquidsoap
   %{test_liq}
   test_metadata.liq)))

(rule
 (alias test_metrics)
 (package liquidsoap)
 (deps
  test_metrics.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (source_tree ../../src/libs)
  crontab_test_cases.json
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} test_metrics.liq liquidsoap %{test_liq} test_metrics.liq)))

(rule
 (alias test_null)
 (package liquidsoap)
 (deps
  test_null.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (source_tree ../../src/libs)
  crontab_test_cases.json
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} test_null.liq liquidsoap %{test_liq} test_null.liq)))

(rule
 (alias test_number)
 (package liquidsoap)
 (deps
  test_number.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (source_tree ../../src/libs)
  crontab_test_cases.json
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} test_number.liq liquidsoap %{test_liq} test_number.liq)))

(rule
 (alias test_osc)
 (package liquidsoap)
 (deps
  test_osc.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (source_tree ../../src/libs)
  crontab_test_cases.json
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} test_osc.liq liquidsoap %{test_liq} test_osc.liq)))

(rule
 (alias test_pattern)
 (package liquidsoap)
 (deps
  test_pattern.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (source_tree ../../src/libs)
  crontab_test_cases.json
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} test_pattern.liq liquidsoap %{test_liq} test_pattern.liq)))

(rule
 (alias test_pp)
 (package liquidsoap)
 (deps
  test_pp.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (source_tree ../../src/libs)
  crontab_test_cases.json
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} test_pp.liq liquidsoap %{test_liq} test_pp.liq)))

(rule
 (alias test_predicate)
 (package liquidsoap)
 (deps
  test_predicate.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (source_tree ../../src/libs)
  crontab_test_cases.json
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run
   %{run_test}
   test_predicate.liq
   liquidsoap
   %{test_liq}
   test_predicate.liq)))

(rule
 (alias test_process)
 (package liquidsoap)
 (deps
  test_process.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (source_tree ../../src/libs)
  crontab_test_cases.json
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} test_process.liq liquidsoap %{test_liq} test_process.liq)))

(rule
 (alias test_rec)
 (package liquidsoap)
 (deps
  test_rec.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (source_tree ../../src/libs)
  crontab_test_cases.json
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} test_rec.liq liquidsoap %{test_liq} test_rec.liq)))

(rule
 (alias test_record)
 (package liquidsoap)
 (deps
  test_record.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (source_tree ../../src/libs)
  crontab_test_cases.json
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} test_record.liq liquidsoap %{test_liq} test_record.liq)))

(rule
 (alias test_ref)
 (package liquidsoap)
 (deps
  test_ref.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (source_tree ../../src/libs)
  crontab_test_cases.json
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} test_ref.liq liquidsoap %{test_liq} test_ref.liq)))

(rule
 (alias test_regexp)
 (package liquidsoap)
 (deps
  test_regexp.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (source_tree ../../src/libs)
  crontab_test_cases.json
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} test_regexp.liq liquidsoap %{test_liq} test_regexp.liq)))

(rule
 (alias test_replaygain)
 (package liquidsoap)
 (deps
  test_replaygain.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (source_tree ../../src/libs)
  crontab_test_cases.json
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run
   %{run_test}
   test_replaygain.liq
   liquidsoap
   %{test_liq}
   test_replaygain.liq)))

(rule
 (alias test_socket)
 (package liquidsoap)
 (deps
  test_socket.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (source_tree ../../src/libs)
  crontab_test_cases.json
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} test_socket.liq liquidsoap %{test_liq} test_socket.liq)))

(rule
 (alias test_sqlite)
 (package liquidsoap)
 (deps
  test_sqlite.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (source_tree ../../src/libs)
  crontab_test_cases.json
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} test_sqlite.liq liquidsoap %{test_liq} test_sqlite.liq)))

(rule
 (alias test_stdlib)
 (package liquidsoap)
 (deps
  test_stdlib.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (source_tree ../../src/libs)
  crontab_test_cases.json
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} test_stdlib.liq liquidsoap %{test_liq} test_stdlib.liq)))

(rule
 (alias test_string)
 (package liquidsoap)
 (deps
  test_string.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (source_tree ../../src/libs)
  crontab_test_cases.json
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} test_string.liq liquidsoap %{test_liq} test_string.liq)))

(rule
 (alias test_string_binary_flags)
 (package liquidsoap)
 (deps
  test_string_binary_flags.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (source_tree ../../src/libs)
  crontab_test_cases.json
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run
   %{run_test}
   test_string_binary_flags.liq
   liquidsoap
   %{test_liq}
   test_string_binary_flags.liq)))

(rule
 (alias test_thread)
 (package liquidsoap)
 (deps
  test_thread.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (source_tree ../../src/libs)
  crontab_test_cases.json
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} test_thread.liq liquidsoap %{test_liq} test_thread.liq)))

(rule
 (alias test_time)
 (package liquidsoap)
 (deps
  test_time.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (source_tree ../../src/libs)
  crontab_test_cases.json
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} test_time.liq liquidsoap %{test_liq} test_time.liq)))

(rule
 (alias test_type_errors)
 (package liquidsoap)
 (deps
  test_type_errors.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (source_tree ../../src/libs)
  crontab_test_cases.json
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run
   %{run_test}
   test_type_errors.liq
   liquidsoap
   %{test_liq}
   test_type_errors.liq)))

(rule
 (alias test_typing)
 (package liquidsoap)
 (deps
  test_typing.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (source_tree ../../src/libs)
  crontab_test_cases.json
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} test_typing.liq liquidsoap %{test_liq} test_typing.liq)))

(rule
 (alias test_url)
 (package liquidsoap)
 (deps
  test_url.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (source_tree ../../src/libs)
  crontab_test_cases.json
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} test_url.liq liquidsoap %{test_liq} test_url.liq)))

(rule
 (alias test_various)
 (package liquidsoap)
 (deps
  test_various.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (source_tree ../../src/libs)
  crontab_test_cases.json
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} test_various.liq liquidsoap %{test_liq} test_various.liq)))

(rule
 (alias test_yaml)
 (package liquidsoap)
 (deps
  test_yaml.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (source_tree ../../src/libs)
  crontab_test_cases.json
  (:test_liq ../test.liq)
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} test_yaml.liq liquidsoap %{test_liq} test_yaml.liq)))

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

(alias
 (name citest)
 (deps
  (alias cue_test)
  (alias test_argsof)
  (alias test_bool)
  (alias test_comments)
  (alias test_conversions)
  (alias test_cron)
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
  (alias test_sqlite)
  (alias test_stdlib)
  (alias test_string)
  (alias test_string_binary_flags)
  (alias test_thread)
  (alias test_time)
  (alias test_type_errors)
  (alias test_typing)
  (alias test_url)
  (alias test_various)
  (alias test_yaml)
  (alias xml_test)))
