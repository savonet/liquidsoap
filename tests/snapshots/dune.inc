(rule
 (target append_list.actual)
 (deps
  (:case cases/append_list.liq)
  (glob_files cases/*.liq-inc)
  (:snapshot snapshot.exe))
 (action
  (with-stdout-to
   %{target}
   (run %{snapshot} %{case}))))

(rule
 (aliases lang_snapshot runtest)
 (package liquidsoap)
 (action
  (diff expected/append_list.expected append_list.actual)))

(rule
 (target argsof.actual)
 (deps
  (:case cases/argsof.liq)
  (glob_files cases/*.liq-inc)
  (:snapshot snapshot.exe))
 (action
  (with-stdout-to
   %{target}
   (run %{snapshot} %{case}))))

(rule
 (aliases lang_snapshot runtest)
 (package liquidsoap)
 (action
  (diff expected/argsof.expected argsof.actual)))

(rule
 (target assoc.actual)
 (deps
  (:case cases/assoc.liq)
  (glob_files cases/*.liq-inc)
  (:snapshot snapshot.exe))
 (action
  (with-stdout-to
   %{target}
   (run %{snapshot} %{case}))))

(rule
 (aliases lang_snapshot runtest)
 (package liquidsoap)
 (action
  (diff expected/assoc.expected assoc.actual)))

(rule
 (target binding_bad_target.actual)
 (deps
  (:case cases/binding_bad_target.invalid-liq)
  (glob_files cases/*.liq-inc)
  (:snapshot snapshot.exe))
 (action
  (with-stdout-to
   %{target}
   (run %{snapshot} %{case}))))

(rule
 (aliases lang_snapshot runtest)
 (package liquidsoap)
 (action
  (diff expected/binding_bad_target.expected binding_bad_target.actual)))

(rule
 (target binding_bare_after_first_in_shorthand.actual)
 (deps
  (:case cases/binding_bare_after_first_in_shorthand.liq)
  (glob_files cases/*.liq-inc)
  (:snapshot snapshot.exe))
 (action
  (with-stdout-to
   %{target}
   (run %{snapshot} %{case}))))

(rule
 (aliases lang_snapshot runtest)
 (package liquidsoap)
 (action
  (diff
   expected/binding_bare_after_first_in_shorthand.expected
   binding_bare_after_first_in_shorthand.actual)))

(rule
 (target binding_bare_in_shorthand.actual)
 (deps
  (:case cases/binding_bare_in_shorthand.invalid-liq)
  (glob_files cases/*.liq-inc)
  (:snapshot snapshot.exe))
 (action
  (with-stdout-to
   %{target}
   (run %{snapshot} %{case}))))

(rule
 (aliases lang_snapshot runtest)
 (package liquidsoap)
 (action
  (diff
   expected/binding_bare_in_shorthand.expected
   binding_bare_in_shorthand.actual)))

(rule
 (target binding_forms.actual)
 (deps
  (:case cases/binding_forms.liq)
  (glob_files cases/*.liq-inc)
  (:snapshot snapshot.exe))
 (action
  (with-stdout-to
   %{target}
   (run %{snapshot} %{case}))))

(rule
 (aliases lang_snapshot runtest)
 (package liquidsoap)
 (action
  (diff expected/binding_forms.expected binding_forms.actual)))

(rule
 (target binding_let_in_shorthand.actual)
 (deps
  (:case cases/binding_let_in_shorthand.liq)
  (glob_files cases/*.liq-inc)
  (:snapshot snapshot.exe))
 (action
  (with-stdout-to
   %{target}
   (run %{snapshot} %{case}))))

(rule
 (aliases lang_snapshot runtest)
 (package liquidsoap)
 (action
  (diff
   expected/binding_let_in_shorthand.expected
   binding_let_in_shorthand.actual)))

(rule
 (target binding_positions.actual)
 (deps
  (:case cases/binding_positions.liq)
  (glob_files cases/*.liq-inc)
  (:snapshot snapshot.exe))
 (action
  (with-stdout-to
   %{target}
   (run %{snapshot} %{case}))))

(rule
 (aliases lang_snapshot runtest)
 (package liquidsoap)
 (action
  (diff expected/binding_positions.expected binding_positions.actual)))

(rule
 (target binding_targets.actual)
 (deps
  (:case cases/binding_targets.liq)
  (glob_files cases/*.liq-inc)
  (:snapshot snapshot.exe))
 (action
  (with-stdout-to
   %{target}
   (run %{snapshot} %{case}))))

(rule
 (aliases lang_snapshot runtest)
 (package liquidsoap)
 (action
  (diff expected/binding_targets.expected binding_targets.actual)))

(rule
 (target bool_ops.actual)
 (deps
  (:case cases/bool_ops.liq)
  (glob_files cases/*.liq-inc)
  (:snapshot snapshot.exe))
 (action
  (with-stdout-to
   %{target}
   (run %{snapshot} %{case}))))

(rule
 (aliases lang_snapshot runtest)
 (package liquidsoap)
 (action
  (diff expected/bool_ops.expected bool_ops.actual)))

(rule
 (target coalesce.actual)
 (deps
  (:case cases/coalesce.liq)
  (glob_files cases/*.liq-inc)
  (:snapshot snapshot.exe))
 (action
  (with-stdout-to
   %{target}
   (run %{snapshot} %{case}))))

(rule
 (aliases lang_snapshot runtest)
 (package liquidsoap)
 (action
  (diff expected/coalesce.expected coalesce.actual)))

(rule
 (target control_flow.actual)
 (deps
  (:case cases/control_flow.liq)
  (glob_files cases/*.liq-inc)
  (:snapshot snapshot.exe))
 (action
  (with-stdout-to
   %{target}
   (run %{snapshot} %{case}))))

(rule
 (aliases lang_snapshot runtest)
 (package liquidsoap)
 (action
  (diff expected/control_flow.expected control_flow.actual)))

(rule
 (target doc_comments.actual)
 (deps
  (:case cases/doc_comments.liq)
  (glob_files cases/*.liq-inc)
  (:snapshot snapshot.exe))
 (action
  (with-stdout-to
   %{target}
   (run %{snapshot} %{case}))))

(rule
 (aliases lang_snapshot runtest)
 (package liquidsoap)
 (action
  (diff expected/doc_comments.expected doc_comments.actual)))

(rule
 (target encoder.actual)
 (deps
  (:case cases/encoder.liq)
  (glob_files cases/*.liq-inc)
  (:snapshot snapshot.exe))
 (action
  (with-stdout-to
   %{target}
   (run %{snapshot} %{case}))))

(rule
 (aliases lang_snapshot runtest)
 (package liquidsoap)
 (action
  (diff expected/encoder.expected encoder.actual)))

(rule
 (target error_parse.actual)
 (deps
  (:case cases/error_parse.invalid-liq)
  (glob_files cases/*.liq-inc)
  (:snapshot snapshot.exe))
 (action
  (with-stdout-to
   %{target}
   (run %{snapshot} %{case}))))

(rule
 (aliases lang_snapshot runtest)
 (package liquidsoap)
 (action
  (diff expected/error_parse.expected error_parse.actual)))

(rule
 (target error_type.actual)
 (deps
  (:case cases/error_type.liq)
  (glob_files cases/*.liq-inc)
  (:snapshot snapshot.exe))
 (action
  (with-stdout-to
   %{target}
   (run %{snapshot} %{case}))))

(rule
 (aliases lang_snapshot runtest)
 (package liquidsoap)
 (action
  (diff expected/error_type.expected error_type.actual)))

(rule
 (target error_unbound.actual)
 (deps
  (:case cases/error_unbound.liq)
  (glob_files cases/*.liq-inc)
  (:snapshot snapshot.exe))
 (action
  (with-stdout-to
   %{target}
   (run %{snapshot} %{case}))))

(rule
 (aliases lang_snapshot runtest)
 (package liquidsoap)
 (action
  (diff expected/error_unbound.expected error_unbound.actual)))

(rule
 (target for_loop.actual)
 (deps
  (:case cases/for_loop.liq)
  (glob_files cases/*.liq-inc)
  (:snapshot snapshot.exe))
 (action
  (with-stdout-to
   %{target}
   (run %{snapshot} %{case}))))

(rule
 (aliases lang_snapshot runtest)
 (package liquidsoap)
 (action
  (diff expected/for_loop.expected for_loop.actual)))

(rule
 (target fun_shorthand.actual)
 (deps
  (:case cases/fun_shorthand.liq)
  (glob_files cases/*.liq-inc)
  (:snapshot snapshot.exe))
 (action
  (with-stdout-to
   %{target}
   (run %{snapshot} %{case}))))

(rule
 (aliases lang_snapshot runtest)
 (package liquidsoap)
 (action
  (diff expected/fun_shorthand.expected fun_shorthand.actual)))

(rule
 (target functions.actual)
 (deps
  (:case cases/functions.liq)
  (glob_files cases/*.liq-inc)
  (:snapshot snapshot.exe))
 (action
  (with-stdout-to
   %{target}
   (run %{snapshot} %{case}))))

(rule
 (aliases lang_snapshot runtest)
 (package liquidsoap)
 (action
  (diff expected/functions.expected functions.actual)))

(rule
 (target getter_coercion.actual)
 (deps
  (:case cases/getter_coercion.liq)
  (glob_files cases/*.liq-inc)
  (:snapshot snapshot.exe))
 (action
  (with-stdout-to
   %{target}
   (run %{snapshot} %{case}))))

(rule
 (aliases lang_snapshot runtest)
 (package liquidsoap)
 (action
  (diff expected/getter_coercion.expected getter_coercion.actual)))

(rule
 (target getters.actual)
 (deps
  (:case cases/getters.liq)
  (glob_files cases/*.liq-inc)
  (:snapshot snapshot.exe))
 (action
  (with-stdout-to
   %{target}
   (run %{snapshot} %{case}))))

(rule
 (aliases lang_snapshot runtest)
 (package liquidsoap)
 (action
  (diff expected/getters.expected getters.actual)))

(rule
 (target infix.actual)
 (deps
  (:case cases/infix.liq)
  (glob_files cases/*.liq-inc)
  (:snapshot snapshot.exe))
 (action
  (with-stdout-to
   %{target}
   (run %{snapshot} %{case}))))

(rule
 (aliases lang_snapshot runtest)
 (package liquidsoap)
 (action
  (diff expected/infix.expected infix.actual)))

(rule
 (target infix_precedence.actual)
 (deps
  (:case cases/infix_precedence.liq)
  (glob_files cases/*.liq-inc)
  (:snapshot snapshot.exe))
 (action
  (with-stdout-to
   %{target}
   (run %{snapshot} %{case}))))

(rule
 (aliases lang_snapshot runtest)
 (package liquidsoap)
 (action
  (diff expected/infix_precedence.expected infix_precedence.actual)))

(rule
 (target invoke_default.actual)
 (deps
  (:case cases/invoke_default.liq)
  (glob_files cases/*.liq-inc)
  (:snapshot snapshot.exe))
 (action
  (with-stdout-to
   %{target}
   (run %{snapshot} %{case}))))

(rule
 (aliases lang_snapshot runtest)
 (package liquidsoap)
 (action
  (diff expected/invoke_default.expected invoke_default.actual)))

(rule
 (target iterable_for.actual)
 (deps
  (:case cases/iterable_for.liq)
  (glob_files cases/*.liq-inc)
  (:snapshot snapshot.exe))
 (action
  (with-stdout-to
   %{target}
   (run %{snapshot} %{case}))))

(rule
 (aliases lang_snapshot runtest)
 (package liquidsoap)
 (action
  (diff expected/iterable_for.expected iterable_for.actual)))

(rule
 (target json_as_type.actual)
 (deps
  (:case cases/json_as_type.liq)
  (glob_files cases/*.liq-inc)
  (:snapshot snapshot.exe))
 (action
  (with-stdout-to
   %{target}
   (run %{snapshot} %{case}))))

(rule
 (aliases lang_snapshot runtest)
 (package liquidsoap)
 (action
  (diff expected/json_as_type.expected json_as_type.actual)))

(rule
 (target json_as_type_bad.actual)
 (deps
  (:case cases/json_as_type_bad.invalid-liq)
  (glob_files cases/*.liq-inc)
  (:snapshot snapshot.exe))
 (action
  (with-stdout-to
   %{target}
   (run %{snapshot} %{case}))))

(rule
 (aliases lang_snapshot runtest)
 (package liquidsoap)
 (action
  (diff expected/json_as_type_bad.expected json_as_type_bad.actual)))

(rule
 (target json_parse.actual)
 (deps
  (:case cases/json_parse.liq)
  (glob_files cases/*.liq-inc)
  (:snapshot snapshot.exe))
 (action
  (with-stdout-to
   %{target}
   (run %{snapshot} %{case}))))

(rule
 (aliases lang_snapshot runtest)
 (package liquidsoap)
 (action
  (diff expected/json_parse.expected json_parse.actual)))

(rule
 (target let_patterns.actual)
 (deps
  (:case cases/let_patterns.liq)
  (glob_files cases/*.liq-inc)
  (:snapshot snapshot.exe))
 (action
  (with-stdout-to
   %{target}
   (run %{snapshot} %{case}))))

(rule
 (aliases lang_snapshot runtest)
 (package liquidsoap)
 (action
  (diff expected/let_patterns.expected let_patterns.actual)))

(rule
 (target let_rec.actual)
 (deps
  (:case cases/let_rec.liq)
  (glob_files cases/*.liq-inc)
  (:snapshot snapshot.exe))
 (action
  (with-stdout-to
   %{target}
   (run %{snapshot} %{case}))))

(rule
 (aliases lang_snapshot runtest)
 (package liquidsoap)
 (action
  (diff expected/let_rec.expected let_rec.actual)))

(rule
 (target list_ops.actual)
 (deps
  (:case cases/list_ops.liq)
  (glob_files cases/*.liq-inc)
  (:snapshot snapshot.exe))
 (action
  (with-stdout-to
   %{target}
   (run %{snapshot} %{case}))))

(rule
 (aliases lang_snapshot runtest)
 (package liquidsoap)
 (action
  (diff expected/list_ops.expected list_ops.actual)))

(rule
 (target methods.actual)
 (deps
  (:case cases/methods.liq)
  (glob_files cases/*.liq-inc)
  (:snapshot snapshot.exe))
 (action
  (with-stdout-to
   %{target}
   (run %{snapshot} %{case}))))

(rule
 (aliases lang_snapshot runtest)
 (package liquidsoap)
 (action
  (diff expected/methods.expected methods.actual)))

(rule
 (target negative.actual)
 (deps
  (:case cases/negative.liq)
  (glob_files cases/*.liq-inc)
  (:snapshot snapshot.exe))
 (action
  (with-stdout-to
   %{target}
   (run %{snapshot} %{case}))))

(rule
 (aliases lang_snapshot runtest)
 (package liquidsoap)
 (action
  (diff expected/negative.expected negative.actual)))

(rule
 (target nested_methods.actual)
 (deps
  (:case cases/nested_methods.liq)
  (glob_files cases/*.liq-inc)
  (:snapshot snapshot.exe))
 (action
  (with-stdout-to
   %{target}
   (run %{snapshot} %{case}))))

(rule
 (aliases lang_snapshot runtest)
 (package liquidsoap)
 (action
  (diff expected/nested_methods.expected nested_methods.actual)))

(rule
 (target null_ops.actual)
 (deps
  (:case cases/null_ops.liq)
  (glob_files cases/*.liq-inc)
  (:snapshot snapshot.exe))
 (action
  (with-stdout-to
   %{target}
   (run %{snapshot} %{case}))))

(rule
 (aliases lang_snapshot runtest)
 (package liquidsoap)
 (action
  (diff expected/null_ops.expected null_ops.actual)))

(rule
 (target numeric_literals.actual)
 (deps
  (:case cases/numeric_literals.liq)
  (glob_files cases/*.liq-inc)
  (:snapshot snapshot.exe))
 (action
  (with-stdout-to
   %{target}
   (run %{snapshot} %{case}))))

(rule
 (aliases lang_snapshot runtest)
 (package liquidsoap)
 (action
  (diff expected/numeric_literals.expected numeric_literals.actual)))

(rule
 (target open_module.actual)
 (deps
  (:case cases/open_module.liq)
  (glob_files cases/*.liq-inc)
  (:snapshot snapshot.exe))
 (action
  (with-stdout-to
   %{target}
   (run %{snapshot} %{case}))))

(rule
 (aliases lang_snapshot runtest)
 (package liquidsoap)
 (action
  (diff expected/open_module.expected open_module.actual)))

(rule
 (target optional_args.actual)
 (deps
  (:case cases/optional_args.liq)
  (glob_files cases/*.liq-inc)
  (:snapshot snapshot.exe))
 (action
  (with-stdout-to
   %{target}
   (run %{snapshot} %{case}))))

(rule
 (aliases lang_snapshot runtest)
 (package liquidsoap)
 (action
  (diff expected/optional_args.expected optional_args.actual)))

(rule
 (target pattern_nested.actual)
 (deps
  (:case cases/pattern_nested.liq)
  (glob_files cases/*.liq-inc)
  (:snapshot snapshot.exe))
 (action
  (with-stdout-to
   %{target}
   (run %{snapshot} %{case}))))

(rule
 (aliases lang_snapshot runtest)
 (package liquidsoap)
 (action
  (diff expected/pattern_nested.expected pattern_nested.actual)))

(rule
 (target pattern_optional_meth.actual)
 (deps
  (:case cases/pattern_optional_meth.liq)
  (glob_files cases/*.liq-inc)
  (:snapshot snapshot.exe))
 (action
  (with-stdout-to
   %{target}
   (run %{snapshot} %{case}))))

(rule
 (aliases lang_snapshot runtest)
 (package liquidsoap)
 (action
  (diff expected/pattern_optional_meth.expected pattern_optional_meth.actual)))

(rule
 (target pp_comments.actual)
 (deps
  (:case cases/pp_comments.liq)
  (glob_files cases/*.liq-inc)
  (:snapshot snapshot.exe))
 (action
  (with-stdout-to
   %{target}
   (run %{snapshot} %{case}))))

(rule
 (aliases lang_snapshot runtest)
 (package liquidsoap)
 (action
  (diff expected/pp_comments.expected pp_comments.actual)))

(rule
 (target pp_ifdef.actual)
 (deps
  (:case cases/pp_ifdef.liq)
  (glob_files cases/*.liq-inc)
  (:snapshot snapshot.exe))
 (action
  (with-stdout-to
   %{target}
   (run %{snapshot} %{case}))))

(rule
 (aliases lang_snapshot runtest)
 (package liquidsoap)
 (action
  (diff expected/pp_ifdef.expected pp_ifdef.actual)))

(rule
 (target pp_ifdef_value.actual)
 (deps
  (:case cases/pp_ifdef_value.liq)
  (glob_files cases/*.liq-inc)
  (:snapshot snapshot.exe))
 (action
  (with-stdout-to
   %{target}
   (run %{snapshot} %{case}))))

(rule
 (aliases lang_snapshot runtest)
 (package liquidsoap)
 (action
  (diff expected/pp_ifdef_value.expected pp_ifdef_value.actual)))

(rule
 (target pp_ifversion.actual)
 (deps
  (:case cases/pp_ifversion.liq)
  (glob_files cases/*.liq-inc)
  (:snapshot snapshot.exe))
 (action
  (with-stdout-to
   %{target}
   (run %{snapshot} %{case}))))

(rule
 (aliases lang_snapshot runtest)
 (package liquidsoap)
 (action
  (diff expected/pp_ifversion.expected pp_ifversion.actual)))

(rule
 (target pp_include.actual)
 (deps
  (:case cases/pp_include.liq)
  (glob_files cases/*.liq-inc)
  (:snapshot snapshot.exe))
 (action
  (with-stdout-to
   %{target}
   (run %{snapshot} %{case}))))

(rule
 (aliases lang_snapshot runtest)
 (package liquidsoap)
 (action
  (diff expected/pp_include.expected pp_include.actual)))

(rule
 (target record_update.actual)
 (deps
  (:case cases/record_update.liq)
  (glob_files cases/*.liq-inc)
  (:snapshot snapshot.exe))
 (action
  (with-stdout-to
   %{target}
   (run %{snapshot} %{case}))))

(rule
 (aliases lang_snapshot runtest)
 (package liquidsoap)
 (action
  (diff expected/record_update.expected record_update.actual)))

(rule
 (target regexp_literal.actual)
 (deps
  (:case cases/regexp_literal.liq)
  (glob_files cases/*.liq-inc)
  (:snapshot snapshot.exe))
 (action
  (with-stdout-to
   %{target}
   (run %{snapshot} %{case}))))

(rule
 (aliases lang_snapshot runtest)
 (package liquidsoap)
 (action
  (diff expected/regexp_literal.expected regexp_literal.actual)))

(rule
 (target seq_and_ignore.actual)
 (deps
  (:case cases/seq_and_ignore.liq)
  (glob_files cases/*.liq-inc)
  (:snapshot snapshot.exe))
 (action
  (with-stdout-to
   %{target}
   (run %{snapshot} %{case}))))

(rule
 (aliases lang_snapshot runtest)
 (package liquidsoap)
 (action
  (diff expected/seq_and_ignore.expected seq_and_ignore.actual)))

(rule
 (target string_escapes.actual)
 (deps
  (:case cases/string_escapes.liq)
  (glob_files cases/*.liq-inc)
  (:snapshot snapshot.exe))
 (action
  (with-stdout-to
   %{target}
   (run %{snapshot} %{case}))))

(rule
 (aliases lang_snapshot runtest)
 (package liquidsoap)
 (action
  (diff expected/string_escapes.expected string_escapes.actual)))

(rule
 (target string_interpolation.actual)
 (deps
  (:case cases/string_interpolation.liq)
  (glob_files cases/*.liq-inc)
  (:snapshot snapshot.exe))
 (action
  (with-stdout-to
   %{target}
   (run %{snapshot} %{case}))))

(rule
 (aliases lang_snapshot runtest)
 (package liquidsoap)
 (action
  (diff expected/string_interpolation.expected string_interpolation.actual)))

(rule
 (target string_interpolation_nested.actual)
 (deps
  (:case cases/string_interpolation_nested.liq)
  (glob_files cases/*.liq-inc)
  (:snapshot snapshot.exe))
 (action
  (with-stdout-to
   %{target}
   (run %{snapshot} %{case}))))

(rule
 (aliases lang_snapshot runtest)
 (package liquidsoap)
 (action
  (diff
   expected/string_interpolation_nested.expected
   string_interpolation_nested.actual)))

(rule
 (target time_predicate.actual)
 (deps
  (:case cases/time_predicate.liq)
  (glob_files cases/*.liq-inc)
  (:snapshot snapshot.exe))
 (action
  (with-stdout-to
   %{target}
   (run %{snapshot} %{case}))))

(rule
 (aliases lang_snapshot runtest)
 (package liquidsoap)
 (action
  (diff expected/time_predicate.expected time_predicate.actual)))

(rule
 (target try_catch.actual)
 (deps
  (:case cases/try_catch.liq)
  (glob_files cases/*.liq-inc)
  (:snapshot snapshot.exe))
 (action
  (with-stdout-to
   %{target}
   (run %{snapshot} %{case}))))

(rule
 (aliases lang_snapshot runtest)
 (package liquidsoap)
 (action
  (diff expected/try_catch.expected try_catch.actual)))

(rule
 (target try_finally.actual)
 (deps
  (:case cases/try_finally.liq)
  (glob_files cases/*.liq-inc)
  (:snapshot snapshot.exe))
 (action
  (with-stdout-to
   %{target}
   (run %{snapshot} %{case}))))

(rule
 (aliases lang_snapshot runtest)
 (package liquidsoap)
 (action
  (diff expected/try_finally.expected try_finally.actual)))

(rule
 (target type_annotation.actual)
 (deps
  (:case cases/type_annotation.liq)
  (glob_files cases/*.liq-inc)
  (:snapshot snapshot.exe))
 (action
  (with-stdout-to
   %{target}
   (run %{snapshot} %{case}))))

(rule
 (aliases lang_snapshot runtest)
 (package liquidsoap)
 (action
  (diff expected/type_annotation.expected type_annotation.actual)))

(rule
 (target while_loop.actual)
 (deps
  (:case cases/while_loop.liq)
  (glob_files cases/*.liq-inc)
  (:snapshot snapshot.exe))
 (action
  (with-stdout-to
   %{target}
   (run %{snapshot} %{case}))))

(rule
 (aliases lang_snapshot runtest)
 (package liquidsoap)
 (action
  (diff expected/while_loop.expected while_loop.actual)))

(rule
 (target canonical_blocks.actual)
 (deps
  (:case cases_canonical/blocks.liq)
  (glob_files cases/*.liq-inc)
  (:snapshot snapshot.exe))
 (action
  (with-stdout-to
   %{target}
   (run %{snapshot} --canonical %{case}))))

(rule
 (aliases lang_snapshot runtest)
 (package liquidsoap)
 (action
  (diff expected_canonical/blocks.expected canonical_blocks.actual)))

(rule
 (target canonical_comments.actual)
 (deps
  (:case cases_canonical/comments.liq)
  (glob_files cases/*.liq-inc)
  (:snapshot snapshot.exe))
 (action
  (with-stdout-to
   %{target}
   (run %{snapshot} --canonical %{case}))))

(rule
 (aliases lang_snapshot runtest)
 (package liquidsoap)
 (action
  (diff expected_canonical/comments.expected canonical_comments.actual)))

(rule
 (target canonical_try_blocks.actual)
 (deps
  (:case cases_canonical/try_blocks.liq)
  (glob_files cases/*.liq-inc)
  (:snapshot snapshot.exe))
 (action
  (with-stdout-to
   %{target}
   (run %{snapshot} --canonical %{case}))))

(rule
 (aliases lang_snapshot runtest)
 (package liquidsoap)
 (action
  (diff expected_canonical/try_blocks.expected canonical_try_blocks.actual)))
