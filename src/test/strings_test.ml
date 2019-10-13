let () =
  let buf = Strings.of_list ["a"; "bc"; ""; "de"] in
  assert (Strings.length buf = 5);
  assert (Strings.to_string buf = "abcde");
  let drop n =
    let buf = Strings.copy buf in
    Strings.drop buf n;
    Strings.to_string buf
  in
  assert (drop 1 = "bcde");
  assert (drop 2 = "cde");
  assert (drop 5 = "");
  let keep n =
    let buf = Strings.copy buf in
    Strings.keep buf n;
    Strings.to_string buf
  in
  assert (keep 2 = "de");
  assert (keep 3 = "cde");
  assert (Strings.to_string (Strings.sub buf 1 2) = "bc");
  assert (Strings.to_string (Strings.sub buf 2 2) = "cd");
  let b = Bytes.create 2 in
  Strings.blit buf 1 b 0 2;
  assert (Bytes.unsafe_to_string b = "bc");
  Strings.blit buf 2 b 0 2;
  assert (Bytes.unsafe_to_string b = "cd");
  let buf2 = Strings.of_list ["f"; "gh"] in
  Strings.append buf buf2;
  assert ((Strings.to_string buf) = "abcdefgh")
