let check name result expected =
  if result <> expected then begin
    Printf.printf "Test %s failed!\n" name;
    Printf.printf "Expected:\n%s\n" expected;
    Printf.printf "Got:\n%s\n" result;
    assert false
  end

let src id activations = Clock_utils.{ id; activations }

let () =
  (* Test simple clock with no sub-clocks *)
  let entry =
    Clock_utils.
      {
        name = "main_clock";
        outputs = [src "output1" []; src "output2" []];
        active = [src "src1" []];
        passive = [src "src2" []; src "src3" []];
        sub_clocks = [];
      }
  in
  let result = Clock_utils.format_clock entry in
  let expected =
    {|main_clock:
├── outputs = [output1 [], output2 []]
├── active  = [src1 []]
└── passive = [src2 [], src3 []]|}
  in
  check "simple clock" result expected;

  (* Test clock with sub-clock *)
  let entry =
    Clock_utils.
      {
        name = "parent";
        outputs = [src "out1" []];
        active = [];
        passive = [src "p1" []];
        sub_clocks =
          [
            {
              name = "child";
              outputs = [src "out2" []];
              active = [src "a1" []];
              passive = [];
              sub_clocks = [];
            };
          ];
      }
  in
  let result = Clock_utils.format_clock entry in
  let expected =
    {|parent:
├── outputs = [out1 []]
├── passive = [p1 []]
└─ child:
   │ outputs = [out2 []]
   │ active  = [a1 []]|}
  in
  check "clock with sub-clock" result expected;

  (* Test line wrapping *)
  let entry =
    Clock_utils.
      {
        name = "audio.producer";
        outputs = [src "audio.consumer" []];
        active = [src "foo" []];
        passive =
          [
            src "metadata_map.3" [];
            src "mksafe" [];
            src "track_metadata_deduplicate" [];
            src "safe_blank" [];
            src "safe_blank.1" [];
            src "cross" [];
            src "metadata_deduplicate" [];
            src "metadata_map.2" [];
            src "mksafe.1" [];
          ];
        sub_clocks = [];
      }
  in
  let result = Clock_utils.format_clock ~max_width:80 entry in
  let expected =
    {|audio.producer:
├── outputs = [audio.consumer []]
├── active  = [foo []]
└── passive = [cross [], metadata_deduplicate [], metadata_map.2 [],
               metadata_map.3 [], mksafe [], mksafe.1 [], safe_blank [],
               safe_blank.1 [], track_metadata_deduplicate []]|}
  in
  check "line wrapping" result expected;

  (* Test empty lists are omitted *)
  let entry =
    Clock_utils.
      {
        name = "minimal";
        outputs = [];
        active = [];
        passive = [src "only_passive" []];
        sub_clocks = [];
      }
  in
  let result = Clock_utils.format_clock entry in
  let expected = {|minimal:
└── passive = [only_passive []]|} in
  check "empty lists omitted" result expected;

  (* Test with activations *)
  let entry =
    Clock_utils.
      {
        name = "with_activations";
        outputs = [src "out1" ["switch"; "fallback"]];
        active = [src "src1" ["main"]];
        passive = [src "src2" []];
        sub_clocks = [];
      }
  in
  let result = Clock_utils.format_clock entry in
  let expected =
    {|with_activations:
├── outputs = [out1 [switch, fallback]]
├── active  = [src1 [main]]
└── passive = [src2 []]|}
  in
  check "with activations" result expected;

  (* Test format_dump with single clock *)
  let entry =
    Clock_utils.
      {
        clock_name = "main";
        ticks = 42;
        self_sync = true;
        outputs = [src "out1" []; src "out2" []];
        active = [src "active1" []];
        passive = [src "passive1" []; src "passive2" []];
        sub_clocks = [];
      }
  in
  let result = Clock_utils.format_dump [entry] in
  let expected =
    {|└── main (ticks: 42, self_sync: true)
    ├── outputs: out1 [], out2 []
    ├── active sources: active1 []
    └── passive sources: passive1 [], passive2 []|}
  in
  check "dump single clock" result expected;

  (* Test format_dump with nested clocks *)
  let entry =
    Clock_utils.
      {
        clock_name = "parent";
        ticks = 100;
        self_sync = false;
        outputs = [src "output" []];
        active = [];
        passive = [src "src1" []];
        sub_clocks =
          [
            {
              clock_name = "child";
              ticks = 50;
              self_sync = true;
              outputs = [src "child_out" []];
              active = [src "child_active" []];
              passive = [];
              sub_clocks = [];
            };
          ];
      }
  in
  let result = Clock_utils.format_dump [entry] in
  let expected =
    {|└── parent (ticks: 100, self_sync: false)
    ├── outputs: output []
    ├── active sources:
    ├── passive sources: src1 []
    └── child (ticks: 50, self_sync: true)
        ├── outputs: child_out []
        ├── active sources: child_active []
        └── passive sources:|}
  in
  check "dump nested clocks" result expected;

  (* Test format_dump with multiple top-level clocks *)
  let entries =
    Clock_utils.
      [
        {
          clock_name = "clock1";
          ticks = 10;
          self_sync = false;
          outputs = [src "o1" []];
          active = [src "a1" []];
          passive = [];
          sub_clocks = [];
        };
        {
          clock_name = "clock2";
          ticks = 20;
          self_sync = true;
          outputs = [];
          active = [];
          passive = [src "p1" []];
          sub_clocks = [];
        };
      ]
  in
  let result = Clock_utils.format_dump entries in
  let expected =
    {|├── clock1 (ticks: 10, self_sync: false)
│   ├── outputs: o1 []
│   ├── active sources: a1 []
│   └── passive sources:
└── clock2 (ticks: 20, self_sync: true)
    ├── outputs:
    ├── active sources:
    └── passive sources: p1 []|}
  in
  check "dump multiple clocks" result expected;

  (* Test format_dump with line wrapping *)
  let entry =
    Clock_utils.
      {
        clock_name = "main";
        ticks = 100;
        self_sync = false;
        outputs = [src "output1" []; src "output2" []; src "output3" []];
        active =
          [
            src "active_source_1" [];
            src "active_source_2" [];
            src "active_source_3" [];
          ];
        passive =
          [
            src "passive1" [];
            src "passive2" [];
            src "passive3" [];
            src "passive4" [];
            src "passive5" [];
          ];
        sub_clocks = [];
      }
  in
  let result = Clock_utils.format_dump ~max_width:60 [entry] in
  let expected =
    {|└── main (ticks: 100, self_sync: false)
    ├── outputs: output1 [], output2 [], output3 []
    ├── active sources: active_source_1 [],
    │                   active_source_2 [],
    │                   active_source_3 []
    └── passive sources: passive1 [], passive2 [],
                         passive3 [], passive4 [],
                         passive5 []|}
  in
  check "dump line wrapping" result expected;

  (* Test format_dump with activations *)
  let entry =
    Clock_utils.
      {
        clock_name = "main";
        ticks = 10;
        self_sync = false;
        outputs = [src "out1" ["switch"; "fallback"]];
        active = [src "src1" ["main"]];
        passive = [src "src2" []];
        sub_clocks = [];
      }
  in
  let result = Clock_utils.format_dump [entry] in
  let expected =
    {|└── main (ticks: 10, self_sync: false)
    ├── outputs: out1 [switch, fallback]
    ├── active sources: src1 [main]
    └── passive sources: src2 []|}
  in
  check "dump with activations" result expected;

  Printf.printf "All Clock_utils tests passed!\n%!"
