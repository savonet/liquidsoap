(rule
 (alias test_abstract_source)
 (package liquidsoap)
 (deps
  abstract_source.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (source_tree ../../src/libs)
  crontab_test_cases.json
  ../test.liq
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} abstract_source.liq liquidsoap abstract_source.liq)))

(rule
 (alias test_argsof)
 (package liquidsoap)
 (deps
  argsof.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (source_tree ../../src/libs)
  crontab_test_cases.json
  ../test.liq
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} argsof.liq liquidsoap argsof.liq)))

(rule
 (alias test_atomic)
 (package liquidsoap)
 (deps
  atomic.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (source_tree ../../src/libs)
  crontab_test_cases.json
  ../test.liq
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} atomic.liq liquidsoap atomic.liq)))

(rule
 (alias test_bool)
 (package liquidsoap)
 (deps
  bool.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (source_tree ../../src/libs)
  crontab_test_cases.json
  ../test.liq
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} bool.liq liquidsoap bool.liq)))

(rule
 (alias test_comments)
 (package liquidsoap)
 (deps
  comments.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (source_tree ../../src/libs)
  crontab_test_cases.json
  ../test.liq
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} comments.liq liquidsoap comments.liq)))

(rule
 (alias test_conversions)
 (package liquidsoap)
 (deps
  conversions.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (source_tree ../../src/libs)
  crontab_test_cases.json
  ../test.liq
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} conversions.liq liquidsoap conversions.liq)))

(rule
 (alias test_cron)
 (package liquidsoap)
 (deps
  cron.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (source_tree ../../src/libs)
  crontab_test_cases.json
  ../test.liq
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} cron.liq liquidsoap cron.liq)))

(rule
 (alias test_cue_test)
 (package liquidsoap)
 (deps
  cue_test.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (source_tree ../../src/libs)
  crontab_test_cases.json
  ../test.liq
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} cue_test.liq liquidsoap cue_test.liq)))

(rule
 (alias test_doc)
 (package liquidsoap)
 (deps
  doc.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (source_tree ../../src/libs)
  crontab_test_cases.json
  ../test.liq
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} doc.liq liquidsoap doc.liq)))

(rule
 (alias test_encoders)
 (package liquidsoap)
 (deps
  encoders.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (source_tree ../../src/libs)
  crontab_test_cases.json
  ../test.liq
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} encoders.liq liquidsoap encoders.liq)))

(rule
 (alias test_error)
 (package liquidsoap)
 (deps
  error.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (source_tree ../../src/libs)
  crontab_test_cases.json
  ../test.liq
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} error.liq liquidsoap error.liq)))

(rule
 (alias test_eval)
 (package liquidsoap)
 (deps
  eval.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (source_tree ../../src/libs)
  crontab_test_cases.json
  ../test.liq
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} eval.liq liquidsoap eval.liq)))

(rule
 (alias test_file)
 (package liquidsoap)
 (deps
  file.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (source_tree ../../src/libs)
  crontab_test_cases.json
  ../test.liq
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} file.liq liquidsoap file.liq)))

(rule
 (alias test_file.watch)
 (package liquidsoap)
 (deps
  file.watch.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (source_tree ../../src/libs)
  crontab_test_cases.json
  ../test.liq
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} file.watch.liq liquidsoap file.watch.liq)))

(rule
 (alias test_file.watch2)
 (package liquidsoap)
 (deps
  file.watch2.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (source_tree ../../src/libs)
  crontab_test_cases.json
  ../test.liq
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} file.watch2.liq liquidsoap file.watch2.liq)))

(rule
 (alias test_file.watch3)
 (package liquidsoap)
 (deps
  file.watch3.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (source_tree ../../src/libs)
  crontab_test_cases.json
  ../test.liq
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} file.watch3.liq liquidsoap file.watch3.liq)))

(rule
 (alias test_file_protocol)
 (package liquidsoap)
 (deps
  file_protocol.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (source_tree ../../src/libs)
  crontab_test_cases.json
  ../test.liq
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} file_protocol.liq liquidsoap file_protocol.liq)))

(rule
 (alias test_functions)
 (package liquidsoap)
 (deps
  functions.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (source_tree ../../src/libs)
  crontab_test_cases.json
  ../test.liq
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} functions.liq liquidsoap functions.liq)))

(rule
 (alias test_getter)
 (package liquidsoap)
 (deps
  getter.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (source_tree ../../src/libs)
  crontab_test_cases.json
  ../test.liq
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} getter.liq liquidsoap getter.liq)))

(rule
 (alias test_icecast_server_config)
 (package liquidsoap)
 (deps
  icecast_server_config.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (source_tree ../../src/libs)
  crontab_test_cases.json
  ../test.liq
  (:run_test ../run_test.exe))
 (action
  (run
   %{run_test}
   icecast_server_config.liq
   liquidsoap
   icecast_server_config.liq)))

(rule
 (alias test_icecast_server_tables)
 (package liquidsoap)
 (deps
  icecast_server_tables.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (source_tree ../../src/libs)
  crontab_test_cases.json
  ../test.liq
  (:run_test ../run_test.exe))
 (action
  (run
   %{run_test}
   icecast_server_tables.liq
   liquidsoap
   icecast_server_tables.liq)))

(rule
 (alias test_implicit_float_casting)
 (package liquidsoap)
 (deps
  implicit_float_casting.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (source_tree ../../src/libs)
  crontab_test_cases.json
  ../test.liq
  (:run_test ../run_test.exe))
 (action
  (run
   %{run_test}
   implicit_float_casting.liq
   liquidsoap
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
  ../test.liq
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} interactive.liq liquidsoap interactive.liq)))

(rule
 (alias test_json)
 (package liquidsoap)
 (deps
  json.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (source_tree ../../src/libs)
  crontab_test_cases.json
  ../test.liq
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} json.liq liquidsoap json.liq)))

(rule
 (alias test_list)
 (package liquidsoap)
 (deps
  list.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (source_tree ../../src/libs)
  crontab_test_cases.json
  ../test.liq
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} list.liq liquidsoap list.liq)))

(rule
 (alias test_loop)
 (package liquidsoap)
 (deps
  loop.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (source_tree ../../src/libs)
  crontab_test_cases.json
  ../test.liq
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} loop.liq liquidsoap loop.liq)))

(rule
 (alias test_math)
 (package liquidsoap)
 (deps
  math.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (source_tree ../../src/libs)
  crontab_test_cases.json
  ../test.liq
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} math.liq liquidsoap math.liq)))

(rule
 (alias test_mem_usage)
 (package liquidsoap)
 (deps
  mem_usage.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (source_tree ../../src/libs)
  crontab_test_cases.json
  ../test.liq
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} mem_usage.liq liquidsoap mem_usage.liq)))

(rule
 (alias test_metadata)
 (package liquidsoap)
 (deps
  metadata.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (source_tree ../../src/libs)
  crontab_test_cases.json
  ../test.liq
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} metadata.liq liquidsoap metadata.liq)))

(rule
 (alias test_metrics)
 (package liquidsoap)
 (deps
  metrics.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (source_tree ../../src/libs)
  crontab_test_cases.json
  ../test.liq
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} metrics.liq liquidsoap metrics.liq)))

(rule
 (alias test_mime_types)
 (package liquidsoap)
 (deps
  mime_types.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (source_tree ../../src/libs)
  crontab_test_cases.json
  ../test.liq
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} mime_types.liq liquidsoap mime_types.liq)))

(rule
 (alias test_null)
 (package liquidsoap)
 (deps
  null.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (source_tree ../../src/libs)
  crontab_test_cases.json
  ../test.liq
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} null.liq liquidsoap null.liq)))

(rule
 (alias test_number)
 (package liquidsoap)
 (deps
  number.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (source_tree ../../src/libs)
  crontab_test_cases.json
  ../test.liq
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} number.liq liquidsoap number.liq)))

(rule
 (alias test_osc)
 (package liquidsoap)
 (deps
  osc.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (source_tree ../../src/libs)
  crontab_test_cases.json
  ../test.liq
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} osc.liq liquidsoap osc.liq)))

(rule
 (alias test_pattern)
 (package liquidsoap)
 (deps
  pattern.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (source_tree ../../src/libs)
  crontab_test_cases.json
  ../test.liq
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} pattern.liq liquidsoap pattern.liq)))

(rule
 (alias test_pp)
 (package liquidsoap)
 (deps
  pp.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (source_tree ../../src/libs)
  crontab_test_cases.json
  ../test.liq
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} pp.liq liquidsoap pp.liq)))

(rule
 (alias test_predicate)
 (package liquidsoap)
 (deps
  predicate.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (source_tree ../../src/libs)
  crontab_test_cases.json
  ../test.liq
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} predicate.liq liquidsoap predicate.liq)))

(rule
 (alias test_process)
 (package liquidsoap)
 (deps
  process.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (source_tree ../../src/libs)
  crontab_test_cases.json
  ../test.liq
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} process.liq liquidsoap process.liq)))

(rule
 (alias test_rec)
 (package liquidsoap)
 (deps
  rec.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (source_tree ../../src/libs)
  crontab_test_cases.json
  ../test.liq
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} rec.liq liquidsoap rec.liq)))

(rule
 (alias test_record)
 (package liquidsoap)
 (deps
  record.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (source_tree ../../src/libs)
  crontab_test_cases.json
  ../test.liq
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} record.liq liquidsoap record.liq)))

(rule
 (alias test_record_methods_eager)
 (package liquidsoap)
 (deps
  record_methods_eager.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (source_tree ../../src/libs)
  crontab_test_cases.json
  ../test.liq
  (:run_test ../run_test.exe))
 (action
  (run
   %{run_test}
   record_methods_eager.liq
   liquidsoap
   record_methods_eager.liq)))

(rule
 (alias test_ref)
 (package liquidsoap)
 (deps
  ref.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (source_tree ../../src/libs)
  crontab_test_cases.json
  ../test.liq
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} ref.liq liquidsoap ref.liq)))

(rule
 (alias test_regexp)
 (package liquidsoap)
 (deps
  regexp.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (source_tree ../../src/libs)
  crontab_test_cases.json
  ../test.liq
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} regexp.liq liquidsoap regexp.liq)))

(rule
 (alias test_replaygain)
 (package liquidsoap)
 (deps
  replaygain.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (source_tree ../../src/libs)
  crontab_test_cases.json
  ../test.liq
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} replaygain.liq liquidsoap replaygain.liq)))

(rule
 (alias test_settings_default)
 (package liquidsoap)
 (deps
  settings_default.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (source_tree ../../src/libs)
  crontab_test_cases.json
  ../test.liq
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} settings_default.liq liquidsoap settings_default.liq)))

(rule
 (alias test_socket)
 (package liquidsoap)
 (deps
  socket.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (source_tree ../../src/libs)
  crontab_test_cases.json
  ../test.liq
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} socket.liq liquidsoap socket.liq)))

(rule
 (alias test_source_content)
 (package liquidsoap)
 (deps
  source_content.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (source_tree ../../src/libs)
  crontab_test_cases.json
  ../test.liq
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} source_content.liq liquidsoap source_content.liq)))

(rule
 (alias test_sqlite)
 (package liquidsoap)
 (deps
  sqlite.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (source_tree ../../src/libs)
  crontab_test_cases.json
  ../test.liq
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} sqlite.liq liquidsoap sqlite.liq)))

(rule
 (alias test_stdlib)
 (package liquidsoap)
 (deps
  stdlib.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (source_tree ../../src/libs)
  crontab_test_cases.json
  ../test.liq
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} stdlib.liq liquidsoap stdlib.liq)))

(rule
 (alias test_string)
 (package liquidsoap)
 (deps
  string.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (source_tree ../../src/libs)
  crontab_test_cases.json
  ../test.liq
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} string.liq liquidsoap string.liq)))

(rule
 (alias test_thread)
 (package liquidsoap)
 (deps
  thread.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (source_tree ../../src/libs)
  crontab_test_cases.json
  ../test.liq
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} thread.liq liquidsoap thread.liq)))

(rule
 (alias test_time)
 (package liquidsoap)
 (deps
  time.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (source_tree ../../src/libs)
  crontab_test_cases.json
  ../test.liq
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} time.liq liquidsoap time.liq)))

(rule
 (alias test_type_errors)
 (package liquidsoap)
 (deps
  type_errors.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (source_tree ../../src/libs)
  crontab_test_cases.json
  ../test.liq
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} type_errors.liq liquidsoap type_errors.liq)))

(rule
 (alias test_typing)
 (package liquidsoap)
 (deps
  typing.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (source_tree ../../src/libs)
  crontab_test_cases.json
  ../test.liq
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} typing.liq liquidsoap typing.liq)))

(rule
 (alias test_url)
 (package liquidsoap)
 (deps
  url.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (source_tree ../../src/libs)
  crontab_test_cases.json
  ../test.liq
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} url.liq liquidsoap url.liq)))

(rule
 (alias test_various)
 (package liquidsoap)
 (deps
  various.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (source_tree ../../src/libs)
  crontab_test_cases.json
  ../test.liq
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} various.liq liquidsoap various.liq)))

(rule
 (alias test_xml_test)
 (package liquidsoap)
 (deps
  xml_test.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (source_tree ../../src/libs)
  crontab_test_cases.json
  ../test.liq
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} xml_test.liq liquidsoap xml_test.liq)))

(rule
 (alias test_yaml)
 (package liquidsoap)
 (deps
  yaml.liq
  ../../src/bin/liquidsoap.exe
  (package liquidsoap)
  (source_tree ../../src/libs)
  crontab_test_cases.json
  ../test.liq
  (:run_test ../run_test.exe))
 (action
  (run %{run_test} yaml.liq liquidsoap yaml.liq)))

(alias
 (name citest)
 (deps
  (alias test_abstract_source)
  (alias test_argsof)
  (alias test_atomic)
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
  (alias test_file.watch3)
  (alias test_file_protocol)
  (alias test_functions)
  (alias test_getter)
  (alias test_icecast_server_config)
  (alias test_icecast_server_tables)
  (alias test_implicit_float_casting)
  (alias test_interactive)
  (alias test_json)
  (alias test_list)
  (alias test_loop)
  (alias test_math)
  (alias test_mem_usage)
  (alias test_metadata)
  (alias test_metrics)
  (alias test_mime_types)
  (alias test_null)
  (alias test_number)
  (alias test_osc)
  (alias test_pattern)
  (alias test_pp)
  (alias test_predicate)
  (alias test_process)
  (alias test_rec)
  (alias test_record)
  (alias test_record_methods_eager)
  (alias test_ref)
  (alias test_regexp)
  (alias test_replaygain)
  (alias test_settings_default)
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
