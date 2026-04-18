open Liquidsoap_lang.Lang_string

(* Valid short UTF-8 strings are not binary. *)
let () =
  assert (not (is_binary "hello"));
  assert (not (is_binary ""));
  assert (not (is_binary "résumé"))

(* Invalid UTF-8 is binary. *)
let () = assert (is_binary "\xff\xfe")

(* Strings longer than max_printable_length are binary. *)
let () =
  let long_string = String.make (!max_printable_length + 1) 'a' in
  assert (is_binary long_string);
  let exact_length = String.make !max_printable_length 'a' in
  assert (not (is_binary exact_length))

(* utf8:false — size check only; any encoding passes through. *)
let () =
  assert (not (is_binary ~utf8:false "hello"));
  assert (not (is_binary ~utf8:false ""));
  assert (not (is_binary ~utf8:false "\xe9\xe0\xfc"));
  assert (not (is_binary ~utf8:false "\xff\xfeR\x00"));
  assert (not (is_binary ~utf8:false "\x00\x01\x02"));
  let long_string = String.make (!max_printable_length + 1) 'a' in
  assert (is_binary ~utf8:false long_string)
