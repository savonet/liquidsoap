open Yaml

let failures = ref 0

let rec show = function
  | Null -> "Null"
  | Bool b -> Printf.sprintf "Bool %b" b
  | Float f -> Printf.sprintf "Float %h" f
  | String s -> Printf.sprintf "String %S" s
  | List l -> "List [" ^ String.concat "; " (List.map show l) ^ "]"
  | Assoc l ->
      "Assoc ["
      ^ String.concat "; "
          (List.map (fun (k, v) -> Printf.sprintf "%S, %s" k (show v)) l)
      ^ "]"

let fail fmt =
  Printf.ksprintf
    (fun s ->
      incr failures;
      print_string ("FAIL: " ^ s ^ "\n"))
    fmt

(* [s] parses to [expected]. *)
let ok s expected =
  match of_string s with
    | Ok v when v = expected -> ()
    | Ok v -> fail "%S\n  expected %s\n  got      %s" s (show expected) (show v)
    | Error e -> fail "%S\n  expected %s\n  got error: %s" s (show expected) e

(* [s] is rejected. *)
let err s =
  match of_string s with
    | Error _ -> ()
    | Ok v -> fail "%S should not parse (got %s)" s (show v)

(* [v] survives a printing and parsing round trip. *)
let roundtrip v =
  let s = to_string v in
  match of_string s with
    | Ok v' when v' = v -> ()
    | Ok v' ->
        fail "round trip of %s\n  printed %S\n  got %s" (show v) s (show v')
    | Error e -> fail "round trip of %s\n  printed %S\n  error: %s" (show v) s e

let () =
  (* Scalars. *)
  ok "" Null;
  ok "null" Null;
  ok "~" Null;
  ok "NULL" Null;
  ok "true" (Bool true);
  ok "False" (Bool false);
  ok "42" (Float 42.);
  ok "-1.5" (Float (-1.5));
  ok "2e3" (Float 2000.);
  ok ".inf" (Float infinity);
  ok "-.inf" (Float neg_infinity);
  ok "no" (String "no");
  ok "0x10" (String "0x10");
  ok "1_0" (String "1_0");
  ok "hello world" (String "hello world");
  (match of_string ".nan" with
    | Ok (Float f) when Float.is_nan f -> ()
    | r ->
        fail ".nan: got %s"
          (match r with Ok v -> show v | Error e -> "error: " ^ e));

  (* Block mappings and sequences. *)
  ok "a: 1\nb: 2\n" (Assoc [("a", Float 1.); ("b", Float 2.)]);
  ok "a:\n  b: 1\n  c: 2\n"
    (Assoc [("a", Assoc [("b", Float 1.); ("c", Float 2.)])]);
  ok "a:\n" (Assoc [("a", Null)]);
  ok "- 1\n- 2\n" (List [Float 1.; Float 2.]);
  ok "a:\n  - 1\n  - 2\n" (Assoc [("a", List [Float 1.; Float 2.])]);
  (* A sequence may sit at the indentation of its key. *)
  ok "a:\n- 1\n- 2\nb: 3\n"
    (Assoc [("a", List [Float 1.; Float 2.]); ("b", Float 3.)]);
  (* Compact entries on the dash line. *)
  ok "- name: alice\n  admin: ~\n- name: bob\n"
    (List
       [
         Assoc [("name", String "alice"); ("admin", Null)];
         Assoc [("name", String "bob")];
       ]);
  ok "- - 1\n  - 2\n- 3\n" (List [List [Float 1.; Float 2.]; Float 3.]);
  ok "-\n- 1\n" (List [Null; Float 1.]);
  ok "- a:\n    b: 1\n" (List [Assoc [("a", Assoc [("b", Float 1.)])]]);

  (* Flow collections. *)
  ok "[1, 2]" (List [Float 1.; Float 2.]);
  ok "[]" (List []);
  ok "{}" (Assoc []);
  ok "[1, ]" (List [Float 1.]);
  ok "{a: 1, b: [2, {c: 3}]}"
    (Assoc [("a", Float 1.); ("b", List [Float 2.; Assoc [("c", Float 3.)]])]);
  ok "{a}" (Assoc [("a", Null)]);
  ok "{a: }" (Assoc [("a", Null)]);
  ok "a: []\nb: {}\n" (Assoc [("a", List []); ("b", Assoc [])]);
  ok "a:\n  - [1, 2]\n" (Assoc [("a", List [List [Float 1.; Float 2.]])]);
  ok "[a b, \"c, d\"]" (List [String "a b"; String "c, d"]);

  (* Quoting. *)
  ok "a: \"x\\ny\"" (Assoc [("a", String "x\ny")]);
  ok "a: 'it''s'" (Assoc [("a", String "it's")]);
  ok "a: \"local#host\"" (Assoc [("a", String "local#host")]);
  ok "a: \"\\u00e9\\u20ac\"" (Assoc [("a", String "\xc3\xa9\xe2\x82\xac")]);
  ok "a: \"1\"" (Assoc [("a", String "1")]);
  ok "a: '~'" (Assoc [("a", String "~")]);
  ok "\"a b\": 1" (Assoc [("a b", Float 1.)]);
  ok "a: b:c" (Assoc [("a", String "b:c")]);

  (* Comments, blank lines and document markers. *)
  ok "# top\na: 1 # trailing\n\n\nb: 2\n"
    (Assoc [("a", Float 1.); ("b", Float 2.)]);
  ok "a: '# not a comment'" (Assoc [("a", String "# not a comment")]);
  ok "---\na: 1\n" (Assoc [("a", Float 1.)]);
  ok "---\na: 1\n...\n" (Assoc [("a", Float 1.)]);
  ok "# only a comment\n" Null;
  ok "a: 1\r\nb: 2\r\n" (Assoc [("a", Float 1.); ("b", Float 2.)]);

  (* Errors. *)
  err "\ta: 1";
  err "a:\n\t b: 1\n";
  err "a: \"unterminated";
  err "a: 'unterminated";
  err "a: [1, 2";
  err "a: {b: 1";
  err "a: [1] 2";
  err "a: |";
  err "a: >";
  err "a: &anchor";
  err "a: *alias";
  err "a: !!str x";
  err "---\na: 1\n---\nb: 2\n";
  err "a: 1\n  b: 2\n";
  err "- 1\nb: 2\n";
  err "a: 1\n- 2\n";
  err "a: \"\\q\"";

  (* Round trips. *)
  List.iter roundtrip
    [
      Null;
      Bool true;
      Bool false;
      Float 0.;
      Float 42.;
      Float (-1.5);
      Float 1e100;
      Float 0.1;
      Float infinity;
      Float neg_infinity;
      String "";
      String "null";
      String "true";
      String "1.5";
      String "a: b";
      String "a:b";
      String "#x";
      String "a #x";
      String " x ";
      String "[1]";
      String "{a}";
      String "- x";
      String "-x";
      String "a\nb";
      String "a\tb";
      String "---";
      String "...";
      String "héllo";
      List [];
      Assoc [];
      List [Null; Bool true; String "x"];
      Assoc [("", Null); ("a b", List []); ("null", Assoc [])];
      List [List [Float 1.; List []]; Assoc [("a", List [Assoc [("b", Null)]])]];
      Assoc
        [
          ( "server",
            Assoc
              [
                ("host", String "local#host");
                ("ports", List [Float 80.; Float 443.]);
                ("opts", Assoc [("tls", Bool true)]);
              ] );
          ( "users",
            List
              [
                Assoc [("name", String "alice"); ("admin", Null)];
                Assoc [("name", String "bob")];
              ] );
        ];
    ];

  (* Printing looks like idiomatic YAML. *)
  let printed =
    to_string
      (Assoc
         [
           ("a", Float 1.);
           ("b", List [Float 1.; Assoc [("c", Float 2.); ("d", Float 3.)]]);
           ("e", Assoc [("f", List [])]);
         ])
  in
  let expected = "a: 1\nb:\n  - 1\n  - c: 2\n    d: 3\ne:\n  f: []\n" in
  if printed <> expected then
    fail "printing:\n  expected %S\n  got      %S" expected printed;

  if !failures = 0 then print_string "All tests passed\n\n"
  else (
    Printf.printf "%d failure(s)\n" !failures;
    exit 1)

let () =
  List.iter
    (fun f ->
      Printf.printf "Parsing %s...\n\n%!" f;
      match Yaml.of_file f with
        | Ok v -> Printf.printf "%s\n%!" (Yaml.to_string v)
        | Error e ->
            Printf.printf "error: %s\n%!" e;
            exit 1)
    ["test.yaml"]
