open Liquidsoap_lang.Builtings_string

(* Valid short UTF-8 strings are not binary. *)
let () =
  assert (not (is_binary "hello"));
  assert (not (is_binary ""));
  assert (not (is_binary "résumé"))

(* Invalid UTF-8 is binary. *)
let () =
  let invalid_utf8 = "\xFF\xFE" in
  assert (is_binary invalid_utf8)

(* Strings longer than max_printable_length are binary. *)
let () =
  let long_string = String.make (!max_printable_length + 1) 'a' in
  assert (is_binary long_string);
  let exact_length = String.make !max_printable_length 'a' in
  assert (not (is_binary exact_length))
