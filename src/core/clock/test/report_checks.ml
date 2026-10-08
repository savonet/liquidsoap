open Harness

let lines text = String.split_on_char '\n' text

let contains line fragment =
  let length = String.length fragment in
  let rec at i =
    i + length <= String.length line
    && (String.sub line i length = fragment || at (i + 1))
  in
  at 0

let index_of text fragment =
  let rec find i = function
    | line :: rest ->
        if contains line fragment then Some i else find (i + 1) rest
    | [] -> None
  in
  find 0 (lines text)

let in_order text fragments =
  let positions = List.map (index_of text) fragments in
  List.for_all Option.is_some positions
  && List.sort compare positions = positions
  && List.sort_uniq compare positions = positions

let position line fragment =
  let length = String.length fragment in
  let rec at i =
    if i + length > String.length line then -1
    else if String.sub line i length = fragment then i
    else at (i + 1)
  in
  at 0

(* Where the fragment starts in its line: nesting, whatever the glyphs. *)
let indent_of text fragment =
  match List.find_opt (fun line -> contains line fragment) (lines text) with
    | Some line -> position line fragment
    | None -> -1

let status_report () =
  let main = passive ~id:"report.main" () in
  let child = sub_clock ~id:"report.child" main in
  Clock.register ~parent:main child;
  attach main (source ~id:"zebra" `Output);
  attach main (source ~id:"alpha" `Output);
  let mic = source ~id:"mic" `Active in
  attach main mic;
  attach child (source ~id:"inner" `Output);
  tick main 3;
  let archive = Clock.create ~id:"report.archive" () in
  attach archive (source ~id:"file" `Output);
  let report = Clock.Status.report [Clock.status main; Clock.status archive] in
  check "the status report lists clocks in order, sub-clocks under their parent"
    (in_order report ["report.main ["; "report.child ["; "report.archive ["]
    && indent_of report "report.child [" > indent_of report "report.main [");
  check "a header gives the state, the sync mode and the controller"
    (in_order report ["report.main [started, passive, controlled by test"]
    && in_order report ["report.archive [stopped: never started, auto]"]);
  check "sources are listed by type, sorted by id"
    (in_order report ["outputs: alpha"; "active: mic"; "passive: -"]
    && in_order report ["outputs: alpha [alpha], zebra [zebra]"]);
  check "a running clock shows its ticks and time"
    (in_order report ["ticks: 3"]);
  let archive_block = Clock.Status.report [Clock.status archive] in
  check "a stopped clock shows its reason and pending sources, no ticks or time"
    (lines archive_block
    = ["report.archive [stopped: never started, auto]"; "  pending: file []"]);
  check "the same record is available in structured form"
    ((Option.get (run main)).ticks = 3
    && List.length (Clock.status main).sub_clocks = 1)

let entry id source_type activations =
  { Clock.Status.id; source_type; activations }

let source_graph () =
  let status =
    {
      (Clock.status (Clock.create ~id:"graph" ())) with
      state =
        `Started
          {
            Clock.Status.animator = None;
            sync = None;
            ticks = 0;
            stream_time = 0.;
            lateness = None;
            outputs =
              [
                entry "output.icecast" `Output ["output.icecast"];
                entry "output.file" `Output ["output.file"];
                entry "cross.out" `Output ["cross"];
              ];
            active = [];
            passive =
              [
                entry "shared_encoder" `Passive ["output.icecast"; "output.file"];
                entry "audio" `Passive ["shared_encoder"];
                entry "spare" `Passive [];
              ];
            statistics =
              {
                life = Clock.Status.no_figures;
                recent = Clock.Status.no_figures;
              };
          };
    }
  in
  let graph = Clock.Status.source_graph [status] in
  check "the source graph prints each source under what wakes it"
    (in_order graph
       [
         "Clock graph:";
         "output.icecast [output]";
         "shared_encoder [passive]";
         "audio [passive]";
         "output.file [output]";
         "shared_encoder [passive] (see above)";
         "Woken from outside";
         "cross [external]";
         "cross.out [output]";
         "Not woken";
         "spare [passive]";
       ]);
  check "a source already printed has nothing beneath it"
    (List.length (List.filter (fun line -> contains line "audio") (lines graph))
    = 1);
  check "children are nested under their activator"
    (indent_of graph "audio [passive]"
     > indent_of graph "shared_encoder [passive]"
    && indent_of graph "shared_encoder [passive]"
       > indent_of graph "output.icecast [output]")

let has clock matching = List.exists matching (events_of clock)

let log_events () =
  let clock = Clock.create ~id:"events" ~sync:`Passive ~owner:(owner ()) () in
  attach clock (source ~id:"out" `Output);
  Clock.start ~force:true clock;
  tick clock 4;
  Clock.stop clock;
  check "the start is logged with its facts"
    (has clock (function
      | Clock.Event.Start
          {
            top_level = true;
            controller = Some _;
            sync_mode = `Passive;
            sources = [("out", `Output)];
            animator = None;
          } ->
          true
      | _ -> false));
  check "the stop is logged with its reason, ticks and stream time"
    (has clock (function
      | Clock.Event.Stop { reason = `Requested; ticks = 4; stream_time } ->
          stream_time = 4. *. frame_duration
      | _ -> false));
  Clock.Settings.conf_preferred#set "nowhere";
  let lost = passive ~id:"lost" () in
  check "an unknown time source is logged with the one used"
    (has lost (function
      | Clock.Event.Unknown_time_source { wanted = "nowhere"; used = "ocaml" }
        ->
          true
      | _ -> false))

let run () =
  status_report ();
  source_graph ();
  log_events ()
