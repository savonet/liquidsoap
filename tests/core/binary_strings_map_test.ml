open Liquidsoap_lang.Binary_strings_map

(* Basic register/is_binary/remove cycle. *)
let () =
  let s = "hello" in
  assert (not (is_binary s));
  register s;
  assert (is_binary s);
  remove s;
  assert (not (is_binary s))

(* Two physically distinct strings with the same content share binary status.
   register one, is_binary on the other should return true and anchor it. *)
let () =
  let s1 = Bytes.to_string (Bytes.of_string "world") in
  let s2 = Bytes.to_string (Bytes.of_string "world") in
  assert (s1 != s2);
  assert (not (is_binary s1));
  assert (not (is_binary s2));
  register s1;
  assert (is_binary s1);
  assert (is_binary s2);
  remove s1;
  assert (not (is_binary s1));
  assert (not (is_binary s2))

(* Registering the same physical string twice is a no-op. *)
let () =
  let s = "foo" in
  register s;
  register s;
  assert (is_binary s);
  remove s;
  assert (not (is_binary s))
