(rule
 (target append_list.actual)
 (deps
  (:case cases/append_list.liq)
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
 (target bool_ops.actual)
 (deps
  (:case cases/bool_ops.liq)
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
 (target encoder.actual)
 (deps
  (:case cases/encoder.liq)
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
 (target json_parse.actual)
 (deps
  (:case cases/json_parse.liq)
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
 (target pp_ifversion.actual)
 (deps
  (:case cases/pp_ifversion.liq)
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
 (target record_update.actual)
 (deps
  (:case cases/record_update.liq)
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
 (target time_predicate.actual)
 (deps
  (:case cases/time_predicate.liq)
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
