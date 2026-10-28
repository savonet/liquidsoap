(* ICY blocks against the worked examples of the specification, UTF-8-safe
   truncation, negotiation and interleaving. *)

module Icy = Harbor_output.Icy

let check_block ~url title expected_text expected_length =
  let block = Icy.block ~url title in
  assert (String.length block = expected_length);
  assert (Char.code block.[0] = (expected_length - 1) / 16);
  assert (String.sub block 1 (String.length expected_text) = expected_text);
  String.iteri
    (fun i c -> if i > String.length expected_text then assert (c = '\000'))
    block

let worked_examples () =
  check_block ~url:None (Some "icy test") "StreamTitle='icy test';" 33;
  check_block ~url:None (Some "abcdefghijklmnopq")
    "StreamTitle='abcdefghijklmnopq';" 33;
  check_block ~url:(Some "http://x")
    (Icy.title [("artist", "A"); ("title", "T")])
    "StreamTitle='A - T';StreamUrl='http://x';" 49;
  assert (Icy.block ~url:None None = Icy.empty_block);
  let block =
    Icy.block ~url:(Some (String.make 300 'u')) (Some (String.make 4000 't'))
  in
  assert (Char.code block.[0] = 255);
  assert (String.length block = 1 + 4080);
  let text = String.sub block 1 4080 in
  assert (String.sub text 0 13 = "StreamTitle='");
  assert (String.sub text (13 + 3849) 3 = "...");
  assert (String.sub text (4080 - 13 - 200) 11 = "StreamUrl='");
  assert (String.sub text (4080 - 2 - 3) 3 = "...")

let utf8_truncation () =
  let title = "a" ^ String.concat "" (List.init 2100 (fun _ -> "é")) in
  let block = Icy.block ~url:None (Some title) in
  let text = String.sub block 1 (String.length block - 1) in
  let close = String.index_from text 13 '\'' in
  let value = String.sub text 13 (close - 13) in
  assert (String.is_valid_utf_8 value);
  assert (String.ends_with ~suffix:"..." value);
  assert (String.length value = 4061 + 3)

let titles () =
  assert (Icy.title [("artist", "A")] = Some "A");
  assert (Icy.title [("title", "T")] = Some "T");
  assert (Icy.title [("album", "X")] = None)

let negotiation () =
  assert (Icy.negotiated ~mime:"audio/mpeg" [("icy-metadata", " 1 ")]);
  assert (not (Icy.negotiated ~mime:"audio/mpeg" [("icy-metadata", "0")]));
  assert (not (Icy.negotiated ~mime:"audio/mpeg" []));
  assert (
    not (Icy.negotiated ~mime:"Audio/Ogg; codecs=opus" [("icy-metadata", "1")]));
  assert (Icy.is_ogg_mime "application/ogg")

let interleaving () =
  let event = Icy.event ~generation:3 ~url:None [("title", "x")] in
  let data = String.make 25 'a' in
  let output, position =
    Icy.interleave ~metaint:10 Icy.start (Some event) data
  in
  assert (String.sub output 0 10 = String.make 10 'a');
  assert (String.sub output 10 (String.length event.Icy.block) = event.Icy.block);
  let after = 10 + String.length event.Icy.block + 10 in
  assert (output.[after] = '\000');
  assert (String.length output = after + 1 + 5);
  assert (position.Icy.since_block = 5);
  assert (position.Icy.last_generation = 3);
  let output, _ =
    Icy.interleave ~metaint:10 Icy.start (Some event) (String.make 10 'a')
  in
  assert (String.length output = 10)

let () =
  worked_examples ();
  utf8_truncation ();
  titles ();
  negotiation ();
  interleaving ();
  print_endline "icy checks passed"
