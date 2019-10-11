let () =
  let buf = Strings.of_list ["a"; "bc"; ""; "d"] in
  assert (Strings.length buf = 4);
  assert (Strings.to_string buf = "abcd")
