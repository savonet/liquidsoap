let () =
  let buf = Strings.of_list ["a"; "bc"; ""; "de"] in
  assert (Strings.length buf = 5);
  assert (Strings.to_string buf = "abcde");
  let b = Bytes.create 2 in
  assert (Strings.to_string (Strings.drop buf 1) = "bcde");
  assert (Strings.to_string (Strings.drop buf 2) = "cde");
  assert (Strings.to_string (Strings.drop buf 5) = "");
  Strings.blit (Strings.sub buf 1 2) b 0;
  assert (Bytes.unsafe_to_string b = "bc");
  Strings.blit (Strings.sub buf 2 2) b 0;
  assert (Bytes.unsafe_to_string b = "cd");
  assert (Strings.to_string (Strings.sub buf 1 2) = "bc");
  assert (Strings.to_string (Strings.sub buf 2 2) = "cd");
  assert (Strings.to_string (Strings.keep buf 3) = "cde")

module M = Strings.Mutable

let () =
  let m = M.create () in
  assert (M.is_empty m);
  assert (M.length m = 0);
  assert (M.to_string m = "");
  assert (M.pos m = 0)

let () =
  let m = M.of_string "hello" in
  assert (not (M.is_empty m));
  assert (M.length m = 5);
  assert (M.to_string m = "hello");
  assert (M.pos m = 5)

let () =
  let m = M.of_bytes (Bytes.of_string "world") in
  assert (M.to_string m = "world");
  assert (M.length m = 5)

let () =
  let m = M.of_strings (Strings.of_list ["a"; "bc"; "de"]) in
  assert (M.to_string m = "abcde");
  assert (M.length m = 5)

let () =
  let m = M.of_list ["foo"; "bar"] in
  assert (M.to_string m = "foobar");
  assert (M.length m = 6)

let () =
  let m = M.create () in
  M.add m "hello";
  assert (M.to_string m = "hello");
  assert (M.pos m = 5);
  M.add m " world";
  assert (M.to_string m = "hello world");
  assert (M.pos m = 11)

let () =
  let m = M.create () in
  M.add_bytes m (Bytes.of_string "test");
  assert (M.to_string m = "test")

let () =
  let m = M.create () in
  M.add_substring m "hello world" 6 5;
  assert (M.to_string m = "world")

let () =
  let m = M.create () in
  M.add_subbytes m (Bytes.of_string "hello world") 0 5;
  assert (M.to_string m = "hello")

let () =
  let m = M.of_string "world" in
  M.dda "hello " m;
  assert (M.to_string m = "hello world");
  assert (M.length m = 11)

let () =
  let m = M.of_string "test" in
  assert (M.pos m = 4);
  let p = M.seek m 0 Unix.SEEK_SET in
  assert (p = 0);
  assert (M.pos m = 0);
  let p = M.seek m 2 Unix.SEEK_SET in
  assert (p = 2);
  assert (M.pos m = 2);
  let p = M.seek m 1 Unix.SEEK_CUR in
  assert (p = 3);
  assert (M.pos m = 3);
  let p = M.seek m (-1) Unix.SEEK_END in
  assert (p = 3);
  assert (M.pos m = 3);
  let p = M.seek m 0 Unix.SEEK_END in
  assert (p = 4);
  assert (M.pos m = 4)

let () =
  let m = M.of_string "test" in
  let raised =
    try
      ignore (M.seek m (-1) Unix.SEEK_SET);
      false
    with Invalid_argument _ -> true
  in
  assert raised

let () =
  let m = M.of_string "hello" in
  ignore (M.seek m 0 Unix.SEEK_SET);
  M.add m "HELLO";
  assert (M.to_string m = "HELLO");
  assert (M.length m = 5)

let () =
  let m = M.of_string "hello" in
  ignore (M.seek m 2 Unix.SEEK_SET);
  M.add m "XY";
  assert (M.to_string m = "heXYo");
  assert (M.length m = 5);
  assert (M.pos m = 4)

let () =
  let m = M.of_string "hi" in
  ignore (M.seek m 0 Unix.SEEK_END);
  M.add m "!!!";
  assert (M.to_string m = "hi!!!");
  assert (M.length m = 5)

let () =
  let m = M.of_string "abcde" in
  M.drop m 2;
  assert (M.to_string m = "cde");
  assert (M.length m = 3)

let () =
  let m = M.of_string "abcde" in
  M.drop m 10;
  assert (M.to_string m = "");
  assert (M.length m = 0);
  assert (M.is_empty m)

let () =
  let m = M.of_string "abcde" in
  M.keep m 3;
  assert (M.to_string m = "cde");
  assert (M.length m = 3)

let () =
  let m = M.of_string "abcde" in
  M.keep m 10;
  assert (M.to_string m = "abcde");
  assert (M.length m = 5)

let () =
  let m = M.of_string "hello" in
  let s = M.substring m 1 3 in
  assert (s = "ell")

let () =
  let m = M.of_string "hello" in
  let raised =
    try
      ignore (M.substring m 3 5);
      false
    with Invalid_argument _ -> true
  in
  assert raised

let () =
  let m = M.of_string "hello" in
  let m2 = M.sub m 1 3 in
  assert (M.to_string m2 = "ell");
  assert (M.length m2 = 3)

let () =
  let m = M.of_string "hello" in
  let b = Bytes.create 3 in
  M.blit m 1 b 0 3;
  assert (Bytes.to_string b = "ell")

let () =
  let m = M.of_string "hello" in
  let raised =
    try
      let b = Bytes.create 3 in
      M.blit m 3 b 0 3;
      false
    with Invalid_argument _ -> true
  in
  assert raised

let () =
  let m = M.of_string "abc" in
  let result = ref "" in
  M.iter (fun s o l -> result := String.sub s o l) m;
  assert (!result = "abc")

let () =
  let m = M.create () in
  let called = ref false in
  M.iter (fun _ _ _ -> called := true) m;
  assert (not !called)

let () =
  let m = M.of_string "test" in
  let len = M.fold (fun acc _ _ l -> acc + l) 0 m in
  assert (len = 4)

let () =
  let m = M.of_string "abc" in
  let m2 =
    M.map (fun s o l -> (String.uppercase_ascii (String.sub s o l), 0, l)) m
  in
  assert (M.to_string m2 = "ABC")

let () =
  let m = M.create () in
  let m2 = M.map (fun s o l -> (s, o, l)) m in
  assert (M.is_empty m2)

let () =
  let m = M.of_string "hello" in
  let content = M.flush m in
  assert (M.is_empty m);
  assert (M.length m = 0);
  assert (M.pos m = 0);
  assert (Strings.to_string content = "hello")

let () =
  let m1 = M.of_string "hello" in
  let m2 = M.of_string " world" in
  M.append m1 m2;
  assert (M.to_string m1 = "hello world");
  assert (M.to_string m2 = " world")

let () =
  let m = M.of_string "start: " in
  M.append_strings m (Strings.of_list ["a"; "b"; "c"]);
  assert (M.to_string m = "start: abc")

let () =
  let m = M.of_string "hello" in
  let b = M.to_bytes m in
  assert (Bytes.to_string b = "hello");
  Bytes.set b 0 'X';
  assert (M.to_string m = "hello")

let () =
  let m = M.of_string "test" in
  let s = M.to_strings m in
  assert (Strings.to_string s = "test")

let () =
  let m = M.create ~size:4 () in
  M.add m "hello world this is a long string";
  assert (M.to_string m = "hello world this is a long string")

let () =
  let m = M.of_string "abc" in
  let raised =
    try
      ignore (M.seek m 5 Unix.SEEK_SET);
      false
    with Invalid_argument _ -> true
  in
  assert raised

let () =
  let m = M.of_string "abcde" in
  ignore (M.seek m 2 Unix.SEEK_SET);
  M.drop m 1;
  assert (M.to_string m = "bcde");
  assert (M.pos m = 1)

let () =
  let m = M.of_string "abcde" in
  ignore (M.seek m 4 Unix.SEEK_SET);
  M.keep m 2;
  assert (M.to_string m = "de");
  assert (M.pos m = 2)

let () =
  let m = M.of_string "test" in
  M.iter_view
    (fun v ->
      let s, o, l = StringView.to_substring v in
      assert (String.sub s o l = "test"))
    m

let () =
  let m = M.of_string "data" in
  let result =
    M.fold_view
      (fun acc v ->
        let s, o, l = StringView.to_substring v in
        acc ^ String.sub s o l)
      "" m
  in
  assert (result = "data")

let () =
  let m = M.of_string "abc" in
  let m2 =
    M.map_view
      (fun v ->
        let s, o, l = StringView.to_substring v in
        StringView.of_string (String.uppercase_ascii (String.sub s o l)))
      m
  in
  assert (M.to_string m2 = "ABC")

let () =
  let m = M.of_string "initial" in
  let reader () =
    for _ = 1 to 1000 do
      ignore (M.length m);
      ignore (M.to_string m);
      ignore (M.is_empty m)
    done
  in
  let writer () =
    for i = 1 to 100 do
      M.add m (string_of_int i)
    done
  in
  let t1 = Thread.create reader () in
  let t2 = Thread.create writer () in
  Thread.join t1;
  Thread.join t2;
  assert (M.length m > 7)
