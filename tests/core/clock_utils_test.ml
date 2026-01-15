let check name result expected =
  if result <> expected then begin
    Printf.printf "Test %s failed!\n" name;
    Printf.printf "Expected:\n%s\n" expected;
    Printf.printf "Got:\n%s\n" result;
    assert false
  end

let () =
  (* Test simple clock with no sub-clocks *)
  let entry =
    Clock_utils.
      {
        name = "main_clock";
        outputs = ["output1"; "output2"];
        active = ["src1"];
        passive = ["src2"; "src3"];
        sub_clocks = [];
      }
  in
  let result = Clock_utils.format_clock entry in
  let expected =
    {|main_clock:
│ outputs = [output1, output2]
│ active  = [src1]
│ passive = [src2, src3]|}
  in
  check "simple clock" result expected;

  (* Test clock with sub-clock *)
  let entry =
    Clock_utils.
      {
        name = "parent";
        outputs = ["out1"];
        active = [];
        passive = ["p1"];
        sub_clocks =
          [
            {
              name = "child";
              outputs = ["out2"];
              active = ["a1"];
              passive = [];
              sub_clocks = [];
            };
          ];
      }
  in
  let result = Clock_utils.format_clock entry in
  let expected =
    {|parent:
│ outputs = [out1]
│ passive = [p1]
└─ child:
   │ outputs = [out2]
   │ active  = [a1]|}
  in
  check "clock with sub-clock" result expected;

  (* Test line wrapping *)
  let entry =
    Clock_utils.
      {
        name = "audio.producer";
        outputs = ["audio.consumer"];
        active = ["foo"];
        passive =
          [
            "metadata_map.3";
            "mksafe";
            "track_metadata_deduplicate";
            "safe_blank";
            "safe_blank.1";
            "cross";
            "metadata_deduplicate";
            "metadata_map.2";
            "mksafe.1";
          ];
        sub_clocks = [];
      }
  in
  let result = Clock_utils.format_clock ~max_width:80 entry in
  let expected =
    {|audio.producer:
│ outputs = [audio.consumer]
│ active  = [foo]
│ passive = [metadata_map.3, mksafe, track_metadata_deduplicate, safe_blank,
│            safe_blank.1, cross, metadata_deduplicate, metadata_map.2,
│            mksafe.1]|}
  in
  check "line wrapping" result expected;

  (* Test empty lists are omitted *)
  let entry =
    Clock_utils.
      {
        name = "minimal";
        outputs = [];
        active = [];
        passive = ["only_passive"];
        sub_clocks = [];
      }
  in
  let result = Clock_utils.format_clock entry in
  let expected = {|minimal:
│ passive = [only_passive]|} in
  check "empty lists omitted" result expected;

  Printf.printf "All Clock_utils tests passed!\n%!"
