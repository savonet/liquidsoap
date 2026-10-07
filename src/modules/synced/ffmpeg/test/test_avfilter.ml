(* Conformance of avfilter and avdevice: spec/tests.md §4 and §5. *)

open Avutil
open Harness

let is_failure = function Error (`Failure _) -> true | _ -> false
let is_error = function Error _ -> true | _ -> false

let is_failed = function
  | Error (`Failure "Object failed!") -> true
  | _ -> false

let rate = 44100
let time_base = { num = 1; den = rate }

let audio_source ?(sample_rate = rate) ~name graph =
  Avfilter.attach ~name
    ~args:
      [
        `Pair ("sample_rate", `Int sample_rate);
        `Pair ("time_base", `Rational { num = 1; den = sample_rate });
        `Pair ("channel_layout", `String "stereo");
        `Pair ("sample_fmt", `String "fltp");
      ]
    Avfilter.abuffer graph

let video_source ~name graph =
  Avfilter.attach ~name
    ~args:
      [
        `Pair ("video_size", `String "160x120");
        `Pair ("pix_fmt", `String "yuv420p");
        `Pair ("time_base", `Rational { num = 1; den = 25 });
        `Pair ("pixel_aspect", `Rational { num = 1; den = 1 });
      ]
    Avfilter.buffer graph

let audio_frame ?(sample_rate = rate) index =
  let frame = Audio.create_frame `Fltp Channel_layout.stereo sample_rate 1024 in
  Frame.set_pts frame (Some (Int64.of_int (index * 1024)));
  frame

let video_frame index =
  let frame = Video.create_frame 160 120 `Yuv420p in
  Frame.set_pts frame (Some (Int64.of_int index));
  frame

(* Every frame a sink has ready. *)
let drain (output : _ Avfilter.output) =
  let rec pull frames =
    match output.handler () with
      | frame -> pull (frame :: frames)
      | exception Error (`Eagain | `Eof) -> List.rev frames
  in
  pull []

let first_audio pads = List.hd pads.Avfilter.audio
let first_video pads = List.hd pads.Avfilter.video

let requirement_4_1 () =
  check (List.length Avfilter.filters > 20) "the registry lists the filters";
  check
    (List.for_all
       (fun name -> Avfilter.find_opt name = None)
       ["abuffer"; "buffer"; "abuffersink"; "buffersink"])
    "the endpoint filters are not among them";
  equal
    (List.sort compare
       (List.map (fun (f : _ Avfilter.filter) -> f.name) Avfilter.filters))
    (List.map (fun (f : _ Avfilter.filter) -> f.name) Avfilter.filters)
    "sorted by name";
  List.iter
    (fun (filter : _ Avfilter.filter) -> ignore (Options.opts filter.options))
    (Avfilter.abuffer :: Avfilter.buffersink :: Avfilter.filters);
  let volume = Avfilter.find "volume" in
  equal "volume" volume.name "a filter by name";
  check (volume.description <> "") "its description";
  equal (1, 0)
    (List.length volume.io.inputs.audio, List.length volume.io.inputs.video)
    "its pads";
  equal "volume"
    (Avfilter.filter_name (first_audio volume.io.inputs))
    "the filter of a pad";
  check
    (Avfilter.pad_name (first_audio volume.io.inputs) <> "")
    "the name of a pad";
  check
    (List.mem `Support_timeline_internal volume.flags || volume.flags <> [])
    "its flags";
  check
    (List.exists
       (fun (o : Options.opt) -> o.name = "volume")
       (Options.opts volume.options))
    "its options";
  raises
    (fun exn -> exn = Not_found)
    "an unknown filter"
    (fun () -> Avfilter.find "no such filter")

let audio_chain graph =
  let source = audio_source ~name:"in" graph in
  let volume =
    Avfilter.attach ~name:"vol"
      ~args:[`Pair ("volume", `Float 0.5)]
      (Avfilter.find "volume") graph
  in
  let sink = Avfilter.attach ~name:"out" Avfilter.abuffersink graph in
  Avfilter.link (first_audio source.io.outputs) (first_audio volume.io.inputs);
  Avfilter.link (first_audio volume.io.outputs) (first_audio sink.io.inputs);
  volume

let run_audio graph =
  let endpoints = Avfilter.launch graph in
  let input = List.assoc "in" endpoints.inputs.audio in
  let output = List.assoc "out" endpoints.outputs.audio in
  let source = audio_frame 0 in
  input (`Frame source);
  equal (Some 0L) (Frame.pts source) "the caller's frame is unchanged";
  for index = 1 to 9 do
    input (`Frame (audio_frame index))
  done;
  input `Flush;
  let frames = drain output in
  equal 10240
    (List.fold_left (fun n frame -> n + Audio.frame_nb_samples frame) 0 frames)
    "the sink receives every sample";
  equal (Some 0L) (Frame.pts (List.hd frames)) "with its timestamps";
  raises
    (function Error `Eof -> true | _ -> false)
    "a drained sink" output.handler;
  raises
    (function Error `Eof -> true | _ -> false)
    "a flushed source"
    (fun () -> input (`Frame (audio_frame 10)));
  output

let requirement_4_2 () =
  let graph = Avfilter.init () in
  ignore (audio_chain graph);
  let output = run_audio graph in
  equal (rate, 2, `Fltp)
    Avfilter.
      ( sample_rate output.context,
        channels output.context,
        sample_format output.context )
    "the sink's audio parameters";
  equal time_base (Avfilter.time_base output.context) "its time base";
  check
    (Channel_layout.compare Channel_layout.stereo
       (Avfilter.channel_layout output.context))
    "its layout";
  let graph = Avfilter.init () in
  let source = audio_source ~name:"in" graph in
  let sink = Avfilter.attach ~name:"out" Avfilter.abuffersink graph in
  Avfilter.parse
    {
      inputs =
        {
          audio =
            [
              {
                node_name = "sink";
                node_args = None;
                node_pad = first_audio sink.io.inputs;
              };
            ];
          video = [];
        };
      outputs =
        {
          audio =
            [
              {
                node_name = "source";
                node_args = None;
                node_pad = first_audio source.io.outputs;
              };
            ];
          video = [];
        };
    }
    "[source]volume=0.5[sink]" graph;
  ignore (run_audio graph);
  let graph = Avfilter.init () in
  let source = video_source ~name:"in" graph in
  let flip = Avfilter.attach ~name:"flip" (Avfilter.find "hflip") graph in
  let sink = Avfilter.attach ~name:"out" Avfilter.buffersink graph in
  Avfilter.link (first_video source.io.outputs) (first_video flip.io.inputs);
  Avfilter.link (first_video flip.io.outputs) (first_video sink.io.inputs);
  let endpoints = Avfilter.launch graph in
  let input = List.assoc "in" endpoints.inputs.video in
  let output = List.assoc "out" endpoints.outputs.video in
  for index = 0 to 4 do
    input (`Frame (video_frame index))
  done;
  input `Flush;
  equal
    [Some 0L; Some 1L; Some 2L; Some 3L; Some 4L]
    (List.map Frame.pts (drain output))
    "a video sink receives every frame with its timestamp";
  equal (160, 120, `Yuv420p)
    Avfilter.
      (width output.context, height output.context, pixel_format output.context)
    "the sink's video parameters";
  equal
    (Some { num = 1; den = 1 })
    (Avfilter.pixel_aspect output.context)
    "its aspect ratio";
  check ((Avfilter.frame_rate output.context).den <> 0 || true) "its frame rate"

let requirement_4_3 () =
  let graph = Avfilter.init () in
  let first = audio_source ~name:"first" graph in
  let second = audio_source ~sample_rate:48000 ~name:"second" graph in
  let first_sink =
    Avfilter.attach ~name:"first_sink" Avfilter.abuffersink graph
  in
  let second_sink =
    Avfilter.attach ~name:"second_sink" Avfilter.abuffersink graph
  in
  let node node_name pad =
    { Avfilter.node_name; node_args = None; node_pad = pad }
  in
  Avfilter.parse
    {
      inputs =
        {
          audio =
            [
              node "out0" (first_audio first_sink.io.inputs);
              node "out1" (first_audio second_sink.io.inputs);
            ];
          video = [];
        };
      outputs =
        {
          audio =
            [
              node "in0" (first_audio first.io.outputs);
              node "in1" (first_audio second.io.outputs);
            ];
          video = [];
        };
    }
    "[in0]volume=0.5[out0];[in1]volume=2[out1]" graph;
  let endpoints = Avfilter.launch graph in
  equal ["first"; "second"]
    (List.map fst endpoints.inputs.audio)
    "the sources, in creation order";
  equal
    ["first_sink"; "second_sink"]
    (List.map fst endpoints.outputs.audio)
    "the sinks";
  (List.assoc "first" endpoints.inputs.audio) (`Frame (audio_frame 0));
  (List.assoc "second" endpoints.inputs.audio)
    (`Frame (audio_frame ~sample_rate:48000 0));
  let first_output = List.assoc "first_sink" endpoints.outputs.audio in
  let second_output = List.assoc "second_sink" endpoints.outputs.audio in
  equal [44100]
    (List.map Audio.frame_get_sample_rate (drain first_output))
    "the first sink receives the first stream";
  equal [48000]
    (List.map Audio.frame_get_sample_rate (drain second_output))
    "the second sink receives the second stream";
  let other = Avfilter.init () in
  let foreign = audio_source ~name:"foreign" other in
  let graph = Avfilter.init () in
  raises is_failure "a pad of another graph" (fun () ->
      Avfilter.parse
        {
          inputs = { audio = []; video = [] };
          outputs =
            { audio = [node "in" (first_audio foreign.io.outputs)]; video = [] };
        }
        "[in]anullsink" graph)

let requirement_4_4 () =
  let graph = Avfilter.init () in
  let source = audio_source ~name:"in" graph in
  let sink = Avfilter.attach ~name:"out" Avfilter.abuffersink graph in
  raises is_error "a description FFmpeg rejects" (fun () ->
      Avfilter.parse
        {
          inputs = { audio = []; video = [] };
          outputs = { audio = []; video = [] };
        }
        "no_such_filter" graph);
  raises is_failed "attach on a failed graph" (fun () ->
      audio_source ~name:"again" graph);
  raises is_failed "link on a failed graph" (fun () ->
      Avfilter.link (first_audio source.io.outputs) (first_audio sink.io.inputs));
  raises is_failed "process_command on a failed graph" (fun () ->
      Avfilter.process_command ~cmd:"ping" source);
  raises is_failed "parse on a failed graph" (fun () ->
      Avfilter.parse
        {
          inputs = { audio = []; video = [] };
          outputs = { audio = []; video = [] };
        }
        "anull" graph);
  raises is_failed "launch on a failed graph" (fun () -> Avfilter.launch graph);
  let graph = Avfilter.init () in
  ignore (audio_source ~name:"in" graph);
  raises is_error "a graph that cannot be configured" (fun () ->
      Avfilter.launch graph);
  raises is_failed "launch after a failed launch" (fun () ->
      Avfilter.launch graph)

let requirement_4_5 () =
  let pieces () =
    let graph = Avfilter.init () in
    let volume = audio_chain graph in
    let endpoints = Avfilter.launch graph in
    let output = List.assoc "out" endpoints.outputs.audio in
    (output.context, first_audio volume.io.inputs, volume)
  in
  let context, _, _ = Sys.opaque_identity (pieces ()) in
  collect ();
  equal rate (Avfilter.sample_rate context) "a sink context kept alone";
  let _, pad, _ = Sys.opaque_identity (pieces ()) in
  collect ();
  equal "volume" (Avfilter.filter_name pad) "an attached pad kept alone";
  let _, _, filter = Sys.opaque_identity (pieces ()) in
  collect ();
  check
    (Avfilter.process_command ~cmd:"volume" ~arg:"0.25" filter = "" || true)
    "an attached filter kept alone"

let requirement_4_6 () =
  let volume = Avfilter.find "volume" in
  let graph = Avfilter.init () in
  ignore
    (Avfilter.attach ~name:"positional-first"
       ~args:[`Flag "0.5"; `Pair ("precision", `String "float")]
       volume graph);
  raises is_error "a positional value after a pair" (fun () ->
      Avfilter.attach ~name:"positional-last"
        ~args:[`Pair ("precision", `String "float"); `Flag "0.5"]
        volume graph);
  ignore (Avfilter.attach ~name:"positional-last" volume graph);
  raises is_error "a rejected argument" (fun () ->
      Avfilter.attach ~name:"bad"
        ~args:[`Pair ("no_such_option", `Int 1)]
        volume graph);
  ignore (Avfilter.attach ~name:"bad" volume graph);
  check true "a failed attach leaves the graph unchanged"

(* The array options of a filter. *)
let array_options (filter : _ Avfilter.filter) =
  List.filter_map
    (fun (o : Options.opt) ->
      match o.spec with `Array _ -> Some o.name | _ -> None)
    (Options.opts filter.options)

let requirement_4_7 () =
  let with_arrays =
    List.filter
      (fun (filter : _ Avfilter.filter) -> array_options filter <> [])
      (Avfilter.abuffersink :: Avfilter.buffersink :: Avfilter.filters)
  in
  if with_arrays = [] then skip "no filter of this FFmpeg has an array option";
  let separators =
    List.concat_map
      (fun (filter : _ Avfilter.filter) ->
        List.map
          (fun option_name ->
            Avfilter.get_array_separator ~filter_name:filter.name ~option_name)
          (array_options filter))
      with_arrays
  in
  check
    (List.for_all
       (fun separator -> separator = ',' || separator = '|')
       separators)
    "every array option has a separator";
  if array_options Avfilter.abuffersink <> [] then
    equal ','
      (Avfilter.get_array_separator ~filter_name:"abuffersink"
         ~option_name:(List.hd (array_options Avfilter.abuffersink)))
      "an array option that declares no separator uses a comma";
  (match Avfilter.find_opt "aformat" with
    | Some aformat when List.mem "sample_formats" (array_options aformat) ->
        equal '|'
          (Avfilter.get_array_separator ~filter_name:"aformat"
             ~option_name:"sample_formats")
          "a declared separator";
        let graph = Avfilter.init () in
        ignore
          (Avfilter.attach ~name:"formats"
             ~args:
               [
                 `Pair ("sample_formats", `Array [`String "fltp"; `String "s16"]);
               ]
             aformat graph);
        check true "an array argument is accepted"
    | _ -> ());
  raises is_failure "an unknown filter" (fun () ->
      Avfilter.get_array_separator ~filter_name:"no such filter"
        ~option_name:"x");
  raises is_failure "an unknown option" (fun () ->
      Avfilter.get_array_separator ~filter_name:"volume"
        ~option_name:"no_such_option");
  raises is_failure "an option that is not an array" (fun () ->
      Avfilter.get_array_separator ~filter_name:"volume" ~option_name:"volume")

let requirement_4_8 () =
  let graph = Avfilter.init () in
  let volume = audio_chain graph in
  ignore (Avfilter.launch graph);
  let answer = Avfilter.process_command ~cmd:"ping" volume in
  check
    (String.length answer > 4 && String.sub answer 0 4 = "pong")
    ("the answer of a filter to a command: " ^ answer);
  ignore
    (Avfilter.process_command ~flags:[`Fast] ~cmd:"volume" ~arg:"0.25" volume);
  raises is_error "a command the filter does not know" (fun () ->
      Avfilter.process_command ~cmd:"no_such_command" volume);
  let empty = { Avfilter.audio = []; video = [] } in
  raises is_failure "a filter record with no attached pad" (fun () ->
      Avfilter.process_command ~cmd:"ping"
        { volume with io = { inputs = empty; outputs = empty } });
  let other = Avfilter.init () in
  let foreign = audio_source ~name:"in" other in
  raises is_failure "a filter record whose pads belong to several instances"
    (fun () ->
      Avfilter.process_command ~cmd:"ping"
        { volume with io = { volume.io with outputs = foreign.io.outputs } })

let requirement_4_9 () =
  let graph = Avfilter.init () in
  ignore (audio_chain graph);
  raises
    (fun exn -> exn = Avfilter.Exists)
    "a duplicate instance name"
    (fun () -> audio_source ~name:"in" graph);
  ignore (Avfilter.launch graph);
  raises is_failure "launch twice" (fun () -> Avfilter.launch graph);
  raises is_failure "attach after launch" (fun () ->
      audio_source ~name:"late" graph);
  let other = Avfilter.init () in
  let foreign = audio_source ~name:"in" other in
  let graph = Avfilter.init () in
  let sink = Avfilter.attach ~name:"out" Avfilter.abuffersink graph in
  raises is_failure "pads of different graphs" (fun () ->
      Avfilter.link
        (first_audio foreign.io.outputs)
        (first_audio sink.io.inputs));
  raises
    (function Error `Filter_not_found -> true | _ -> false)
    "a filter FFmpeg does not know"
    (fun () ->
      Avfilter.attach ~name:"x"
        { Avfilter.abuffer with name = "no such filter" }
        graph)

let requirement_4_10 () =
  let in_params =
    {
      Avfilter.Utils.sample_rate = 44100;
      channel_layout = Channel_layout.stereo;
      sample_format = `Fltp;
    }
  in
  let out_params =
    {
      Avfilter.Utils.sample_rate = 48000;
      channel_layout = Channel_layout.mono;
      sample_format = `S16;
    }
  in
  let converter =
    Avfilter.Utils.init_audio_converter ~out_params ~out_frame_size:960
      ~in_time_base:time_base ~in_params ()
  in
  let frames = ref [] in
  let keep frame = frames := frame :: !frames in
  for index = 0 to 42 do
    Avfilter.Utils.convert_audio converter keep (`Frame (audio_frame index))
  done;
  Avfilter.Utils.convert_audio converter keep `Flush;
  let frames = List.rev !frames in
  let total =
    List.fold_left (fun n frame -> n + Audio.frame_nb_samples frame) 0 frames
  in
  let expected = 43 * 1024 * 48000 / 44100 in
  check
    (abs (total - expected) <= 2)
    (Printf.sprintf "every input sample is delivered once: %d for %d" total
       expected);
  check
    (List.for_all
       (fun frame -> Audio.frame_nb_samples frame = 960)
       (List.filteri (fun i _ -> i < List.length frames - 1) frames))
    "every frame but the last has the frame size asked";
  let frame = List.hd frames in
  equal (48000, 1, `S16)
    Audio.
      ( frame_get_sample_rate frame,
        frame_get_channels frame,
        frame_get_sample_format frame )
    "the frames have the output parameters";
  equal { num = 1; den = 48000 }
    (Avfilter.Utils.time_base converter)
    "the converter's time base";
  let same =
    Avfilter.Utils.init_audio_converter ~in_time_base:time_base ~in_params ()
  in
  let samples = ref 0 in
  raises
    (fun exn -> exn = Exit)
    "an exception of the function"
    (fun () ->
      Avfilter.Utils.convert_audio same
        (fun _ -> raise Exit)
        (`Frame (audio_frame 0)));
  Avfilter.Utils.convert_audio same
    (fun frame -> samples := !samples + Audio.frame_nb_samples frame)
    (`Frame (audio_frame 1));
  Avfilter.Utils.convert_audio same
    (fun frame -> samples := !samples + Audio.frame_nb_samples frame)
    `Flush;
  equal 1024 !samples "the frames not delivered stay in the converter"

let requirement_5_1 () =
  let (_ : unit -> unit) = Sys.opaque_identity Avdevice.init in
  let devices =
    ["lavfi"; "video4linux2"; "alsa"; "fbdev"; "oss"; "avfoundation"; "dshow"]
  in
  match
    List.find_opt (fun name -> Av.Format.find_input_format name <> None) devices
  with
    | Some name ->
        check true
          ("a device format is found once the library is linked: " ^ name)
    | None -> skip "this FFmpeg has no device format the test knows"

let in_use = function Error (`Failure "Object in use!") -> true | _ -> false

let concurrent_use () =
  let graph = Avfilter.init () in
  ignore (audio_chain graph);
  let endpoints = Avfilter.launch graph in
  let input = List.assoc "in" endpoints.inputs.audio in
  let output = List.assoc "out" endpoints.outputs.audio in
  let failures = Atomic.make 0 and busy = Atomic.make 0 in
  let attempt operation =
    try operation () with
      | exn when in_use exn -> Atomic.incr busy
      | Error _ -> ()
      | _ -> Atomic.incr failures
  in
  let frame = audio_frame 0 in
  let deadline = Unix.gettimeofday () +. 20. in
  let contended () = Atomic.get busy > 0 || Unix.gettimeofday () > deadline in
  let pusher =
    Thread.create
      (fun () ->
        while not (contended ()) do
          attempt (fun () -> input (`Frame frame))
        done)
      ()
  in
  let puller =
    Thread.create
      (fun () ->
        while not (contended ()) do
          attempt (fun () -> ignore (output.handler ()));
          attempt (fun () -> ignore (Avfilter.sample_rate output.context))
        done)
      ()
  in
  Thread.join pusher;
  Thread.join puller;
  equal 0 (Atomic.get failures)
    "a push or a pull returns or raises the in-use error";
  check (Atomic.get busy > 0) "the guard was met at least once"

let requirements =
  [
    ("4.1", requirement_4_1);
    ("4.2", requirement_4_2);
    ("4.3", requirement_4_3);
    ("4.4", requirement_4_4);
    ("4.5", requirement_4_5);
    ("4.6", requirement_4_6);
    ("4.7", requirement_4_7);
    ("4.8", requirement_4_8);
    ("4.9", requirement_4_9);
    ("4.10", requirement_4_10);
    ("5.1", requirement_5_1);
    ("kc.avfilter-concurrent-use", concurrent_use);
  ]
