(*****************************************************************************

  Liquidsoap, a programmable stream generator.
  Copyright 2003-2026 Savonet team

  This program is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details, fully stated in the COPYING
  file at the root of the liquidsoap distribution.

  You should have received a copy of the GNU General Public License
  along with this program; if not, write to the Free Software
  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301  USA

 *****************************************************************************)

open Builtins_ffmpeg_base
module Queue = Queues.Queue

(** FFmpeg filter graphs initialization is pretty tricky. Things to consider:
    - FFmpeg filters are using a push paradigm, pushing from the sources down to
      the outputs
    - Liquidsoap uses a pull paradigm, pulling data from the outputs.
    - FFmpeg filters inputs need to know the exact content of the data sent to
      them before being initialized, which is only known in the worst case when
      receiving the first frame.

    Therefore, the intended implementation is to: -> Consider ffmpeg filter
    graphs as a single operator with N inputs and M outputs (audio/video) with
    inputs being any source converted to a ffmpeg graph input, even if not used,
    for simplification. -> The outputs are placed in their clock which controls
    a child clock containing the inputs. -> The graph initialization is
    suspended until its initialization conditions are met. -> When all the
    inputs have been initialized and are ready (call to `#is_ready`), the graph
    is considered ready and its output can start requesting content. This is
    captured by the call to `is_ready` below. -> When requesting content, the
    outputs can tick the input clock as many time as needed to generate output
    data. We expect more or less real time with perhaps an accordion pattern
    between input and output so we do not look at latency control like we do for
    crossfades. -> When receiving its first frame, the liquidsoap input will
    then initialize the corresponding ffmpeg graph input with full format info.
    -> When all inputs have been initialized, which is know by checking if all
    of the graph's lazy values for input pads have been forced (executed), the
    whole graph is initialized, outputs connected and data can start to flow!
    This is captured by the call to `initialized` below.

    This is contingent to the inputs being checked for `#is_ready` and the
    outputs only pulling when _all_ inputs are available, to avoid running into
    endless pulling loops. *)

let ffmpeg_filter_audio = Lang.add_module ~base:ffmpeg_filter "audio"
let ffmpeg_filter_video = Lang.add_module ~base:ffmpeg_filter "video"
let log = Log.make ["ffmpeg"; "filter"]

type 'a input = 'a Avfilter.input
type 'a output = 'a Avfilter.output
type 'a setter = 'a -> unit
type 'a entries = (string, 'a setter) Hashtbl.t
type inputs = ([ `Audio ] input entries, [ `Video ] input entries) Avfilter.av

type outputs =
  ([ `Audio ] output entries, [ `Video ] output entries) Avfilter.av

type graph = {
  mutable config : Avfilter.config option;
  mutable failed : bool;
  input_inits : (unit -> bool) Queue.t;
  graph_inputs : Source.source Queue.t;
  input_flushes : (unit -> unit) Queue.t;
  mutable graph_source : Ffmpeg_filter_graph.source option;
  mutable audio_outputs : int;
  mutable video_outputs : int;
  init : unit Lazy.t Queue.t;
  entries : (inputs, outputs) Avfilter.io;
}

let init_graph graph =
  if Queue.fold graph.input_inits (fun v b -> b && v ()) true then (
    try Queue.iter graph.init Lazy.force
    with exn ->
      let bt = Printexc.get_raw_backtrace () in
      graph.failed <- true;
      Printexc.raise_with_backtrace exn bt)

let initialized graph =
  Queue.fold graph.init (fun q cur -> cur && Lazy.is_val q) true

let is_ready graph =
  (match (initialized graph, Queue.peek_opt graph.graph_inputs) with
    | false, Some s ->
        if not (Clock.started s#clock) then Clock.start s#clock;
        Clock.tick ~pull:true s#clock
    (* No liquidsoap input to wait for: the graph is fed by source filters
       alone, so nothing else will ever trigger initialization. Doing it here
       rather than when the graph is built keeps it at streaming time, where
       output content types are known. *)
    | false, None -> init_graph graph
    | _ -> ());
  (not graph.failed)
  && Queue.fold graph.graph_inputs
       (fun (s : Source.source) cur -> cur && s#is_ready)
       true

let pull graph =
  match Queue.peek_opt graph.graph_inputs with
    | Some s -> Clock.tick ~pull:true s#clock
    | None -> ()

(* Once the inputs are done, the graph needs to be told so that filters holding
   a tail release it. *)
let flush_inputs graph = Queue.iter graph.input_flushes (fun flush -> flush ())

let self_sync graph source =
  (Clock_base.self_sync ~source (Queue.elements graph.graph_inputs)) ()

(* Created on the first output: a graph with none needs no source. *)
let graph_source graph =
  match graph.graph_source with
    | Some s -> s
    | None ->
        let s =
          new Ffmpeg_filter_graph.source
            ~name:"ffmpeg.filter"
            ~pull:(fun () -> pull graph)
            ~is_ready:(fun () -> is_ready graph)
            ~flush_inputs:(fun () -> flush_inputs graph)
            ~self_sync:(fun source -> self_sync graph source)
            ()
        in
        graph.graph_source <- Some s;
        s

module Graph = Value.MkCustom (struct
  type content = graph

  let name = "ffmpeg.filter.graph"
  let to_string _ = name

  let to_json ~pos _ =
    Lang.raise_error
      ~message:"Ffmpeg filter graph cannot be represented as json" ~pos "json"

  let compare = Stdlib.compare
end)

module Audio = Value.MkCustom (struct
  type content =
    [ `Input of ([ `Attached ], [ `Audio ], [ `Input ]) Avfilter.pad
    | `Output of ([ `Attached ], [ `Audio ], [ `Output ]) Avfilter.pad Lazy.t
    ]

  let name = "ffmpeg.filter.audio"
  let to_string _ = name

  let to_json ~pos _ =
    Lang.raise_error ~pos
      ~message:"Ffmpeg filter audio input cannot be represented as json" "json"

  let compare = Stdlib.compare
end)

module Video = Value.MkCustom (struct
  type content =
    [ `Input of ([ `Attached ], [ `Video ], [ `Input ]) Avfilter.pad
    | `Output of ([ `Attached ], [ `Video ], [ `Output ]) Avfilter.pad Lazy.t
    ]

  let name = "ffmpeg.filter.video"
  let to_string _ = name

  let to_json ~pos _ =
    Lang.raise_error ~pos
      ~message:"Ffmpeg filter video input cannot be represented as json" "json"

  let compare = Stdlib.compare
end)

let uniq_name =
  let names = Hashtbl.create 10 in
  let name_idx name =
    match Hashtbl.find_opt names name with
      | Some x ->
          Hashtbl.replace names name (x + 1);
          x
      | None ->
          Hashtbl.replace names name 1;
          0
  in
  fun name -> Printf.sprintf "%s_%d" name (name_idx name)

let get_config graph =
  let { config; _ } = Graph.of_value graph in
  match config with
    | Some config -> config
    | None ->
        raise
          (Error.Invalid_value
             ( graph,
               "Graph variables cannot be used outside of ffmpeg.filter.create!",
               [] ))

let apply_filter ~args_parser ~filter ~sources_t p =
  Avfilter.(
    let graph_v = Lang.assoc "" 1 p in
    let config = get_config graph_v in
    let graph = Graph.of_value graph_v in
    let name = uniq_name filter.name in
    let flags = filter.flags in
    let filter = attach ~args:(args_parser p []) ~name filter config in
    let input_set = ref false in
    let meths =
      [
        ( "process_command",
          Lang.val_fun
            [
              ("fast", "fast", Some (Lang.bool false));
              ("", "", None);
              ("", "", None);
            ]
            (fun p ->
              let fast = Lang.to_bool (List.assoc "fast" p) in
              let flags = if fast then [`Fast] else [] in
              let cmd = Lang.to_string (Lang.assoc "" 1 p) in
              let arg = Lang.to_string (Lang.assoc "" 2 p) in
              if initialized graph then (
                try
                  Lang.string (Avfilter.process_command ~flags ~cmd ~arg filter)
                with exn ->
                  let bt = Printexc.get_raw_backtrace () in
                  Lang.raise_as_runtime ~bt ~kind:"ffmpeg.filter" exn)
              else Lang.string "graph not started!") );
        ( "output",
          let audio =
            List.map
              (fun p -> Audio.to_value (`Output (Lazy.from_val p)))
              filter.io.outputs.audio
          in
          let video =
            List.map
              (fun p -> Video.to_value (`Output (Lazy.from_val p)))
              filter.io.outputs.video
          in
          if List.mem `Dynamic_outputs flags then
            Lang.tuple [Lang.list audio; Lang.list video]
          else (match audio @ video with [x] -> x | l -> Lang.tuple l) );
        ( "set_input",
          Lang.val_fun
            (List.map (fun (_, lbl, _) -> (lbl, lbl, None)) sources_t)
            (fun p ->
              if !input_set then (
                let pos =
                  match Value.pos (List.assoc "" p) with
                    | (exception Not_found) | None -> []
                    | Some p -> [p]
                in
                Lang.raise_error ~pos ~message:"Filter input already set!"
                  "ffmpeg.filter");
              let audio_inputs_c = List.length filter.io.inputs.audio in
              let get_input ~mode ~ofs idx =
                if List.mem `Dynamic_inputs flags then (
                  let v = Lang.assoc "" (if mode = `Audio then 1 else 2) p in
                  let inputs = Lang.to_list v in
                  if List.length inputs <= idx then
                    raise
                      (Error.Invalid_value
                         ( v,
                           Printf.sprintf
                             "Invalid number of input for filter %s" filter.name,
                           [] ));
                  List.nth inputs idx)
                else Lang.assoc "" (idx + ofs + 1) p
              in
              let link ~of_value ~mode ~ofs idx input =
                let output = get_input ~mode ~ofs idx in
                let pos =
                  match Value.pos output with None -> [] | Some p -> [p]
                in
                let output =
                  match of_value output with
                    | `Output output -> Lazy.force output
                    | _ -> assert false
                in
                try link output input
                with exn ->
                  Lang.raise_error ~pos
                    ~message:
                      (Printf.sprintf
                         "Error while connecting filter elements %s to %s: %s"
                         (Avfilter.filter_name input)
                         (Avfilter.filter_name output)
                         (Printexc.to_string exn))
                    "ffmpeg.filter"
              in
              Queue.push graph.init
                (Lazy.from_fun (fun () ->
                     List.iteri
                       (link ~of_value:Audio.of_value ~mode:`Audio ~ofs:0)
                       filter.io.inputs.audio;
                     List.iteri
                       (link ~of_value:Video.of_value ~mode:`Video
                          ~ofs:audio_inputs_c)
                       filter.io.inputs.video));
              input_set := true;
              Lang.unit) );
      ]
    in
    Lang.meth Lang.unit meths)

let register_filters () =
  Avfilter.(
    let mk_av_t ~flags ~mode { audio; video } =
      match mode with
        | `Input when List.mem `Dynamic_inputs flags ->
            [Lang.list_t Audio.t; Lang.list_t Video.t]
        | `Output when List.mem `Dynamic_outputs flags ->
            [Lang.list_t Audio.t; Lang.list_t Video.t]
        | _ ->
            let audio = List.map (fun _ -> Audio.t) audio in
            let video = List.map (fun _ -> Video.t) video in
            audio @ video
    in
    List.iter
      (fun ({ name; description; io; flags } as filter) ->
        let args, args_parser =
          Ffmpeg_filter_options.mk_options filter.options
        in
        let args_t = args @ [("", Graph.t, None, None)] in
        let sources_t =
          List.map
            (fun t -> (false, "", t))
            (mk_av_t ~flags ~mode:`Input io.inputs)
        in
        let output_t =
          match mk_av_t ~flags ~mode:`Output io.outputs with
            | [x] -> x
            | l -> Lang.tuple_t l
        in
        let return_t =
          Lang.method_t Lang.unit_t
            [
              ( "process_command",
                ( [],
                  Lang.fun_t
                    [
                      (true, "fast", Lang.bool_t);
                      (false, "", Lang.string_t);
                      (false, "", Lang.string_t);
                    ]
                    Lang.string_t ),
                "`process_command(?fast, \"command\", \"argument\")` sends the \
                 given command to this filter. Set `fast` to `true` to only \
                 execute the command when it is fast." );
              ("output", ([], output_t), "Filter output(s)");
              ( "set_input",
                ([], Lang.fun_t sources_t Lang.unit_t),
                "Set the filter's input(s)" );
            ]
        in
        let explanation =
          String.concat " "
            ((if List.mem `Dynamic_inputs flags then
                [
                  "This filter has dynamic inputs: last two arguments are \
                   lists of audio and video inputs. Total number of inputs is \
                   determined at runtime.";
                ]
              else [])
            @
            if List.mem `Dynamic_outputs flags then
              [
                "This filter has dynamic outputs: returned value is a tuple of \
                 audio and video outputs. Total number of outputs is \
                 determined at runtime.";
              ]
            else [])
        in
        let descr =
          Printf.sprintf "Ffmpeg filter: %s%s" description
            (if explanation <> "" then " " ^ explanation else "")
        in
        let base_filter =
          Lang.add_builtin ~category:(`Source `FFmpegFilter) name
            ~base:ffmpeg_filter ~descr ~flags:[`Extra]
            (args_t
            @ List.map (fun (_, lbl, t) -> (lbl, t, None, None)) sources_t)
            output_t
            (fun p ->
              let named_args = List.filter (fun (lbl, _) -> lbl <> "") p in
              let unnamed_args = List.filter (fun (lbl, _) -> lbl = "") p in
              (* Unnamed args are ordered. The last [n] ones are from [sources_t] *)
              let n_sources = List.length sources_t in
              let n_args = List.length unnamed_args in
              let unnamed_args, inputs =
                List.fold_left
                  (fun (args, inputs) el ->
                    if List.length args < n_args - n_sources then
                      (args @ [el], inputs)
                    else (args, inputs @ [el]))
                  ([], []) unnamed_args
              in
              let args = named_args @ unnamed_args in
              let filter = apply_filter ~args_parser ~filter ~sources_t args in
              ignore
                (Lang.apply ~pos:(Lang.pos p)
                   (Value.invoke filter "set_input")
                   inputs);
              Value.invoke filter "output")
        in
        ignore
          (Lang.add_builtin ~category:(`Source `FFmpegFilter) ~base:base_filter
             "create"
             ~descr:
               (Printf.sprintf
                  "%s. Use this operator to initiate the filter independently \
                   of its inputs, to be able to send commands to the filter \
                   instance."
                  descr)
             ~flags:[`Extra] args_t return_t
             (apply_filter ~args_parser ~filter ~sources_t)))
      filters)

let () = Startup.time "FFmpeg filters registration" register_filters

let abuffer_args frame =
  let sample_rate = Avutil.Audio.frame_get_sample_rate frame in
  let channel_layout = Avutil.Audio.frame_get_channel_layout frame in
  let channel_layout_params =
    match Avutil.Channel_layout.get_mask channel_layout with
      | Some id -> ("channel_layout", `Int64 id)
      | None ->
          let channel_layout =
            Avutil.Channel_layout.get_default
              (Avutil.Channel_layout.get_nb_channels channel_layout)
          in
          ( "channel_layout",
            `Int64 (Option.get (Avutil.Channel_layout.get_mask channel_layout))
          )
  in
  let sample_format = Avutil.Audio.frame_get_sample_format frame in
  [
    `Pair ("sample_rate", `Int sample_rate);
    `Pair ("time_base", `Rational (Ffmpeg_utils.liq_main_ticks_time_base ()));
    `Pair channel_layout_params;
    `Pair ("sample_fmt", `Int (Avutil.Sample_format.get_id sample_format));
  ]

let buffer_args frame =
  let width = Avutil.Video.frame_get_width frame in
  let height = Avutil.Video.frame_get_height frame in
  let pixel_format = Avutil.Video.frame_get_pixel_format frame in
  [
    `Pair ("time_base", `Rational (Ffmpeg_utils.liq_main_ticks_time_base ()));
    `Pair ("width", `Int width);
    `Pair ("height", `Int height);
    `Pair ("pix_fmt", `Int Avutil.Pixel_format.(get_id pixel_format));
  ]

let _ =
  let raw_audio_format = `Kind Ffmpeg_raw_content.Audio.kind in
  let raw_video_format = `Kind Ffmpeg_raw_content.Video.kind in
  let audio_frame_t = Type.make (Format_type.descr raw_audio_format) in
  let video_frame_t = Type.make (Format_type.descr raw_video_format) in

  ignore
    (Lang.add_builtin ~category:(`Source `FFmpegFilter)
       ~base:ffmpeg_filter_audio "input"
       ~descr:"Attach an audio track to a filter's input"
       [
         ("id", Lang.nullable_t Lang.string_t, Some Lang.null, None);
         ( "pass_metadata",
           Lang.bool_t,
           Some (Lang.bool true),
           Some "Pass liquidsoap's metadata to this stream" );
         ("", Graph.t, None, None);
         ("", audio_frame_t, None, None);
       ]
       Audio.t
       (fun p ->
         let id =
           Option.value ~default:"ffmpeg.filter.audio.input"
             (Lang.to_valued_option Lang.to_string (List.assoc "id" p))
         in
         let pass_metadata = Lang.to_bool (List.assoc "pass_metadata" p) in
         let graph_v = Lang.assoc "" 1 p in
         let config = get_config graph_v in
         let graph = Graph.of_value graph_v in
         let track_val = Lang.assoc "" 2 p in
         let field, source = Lang.to_track track_val in

         let frame_t =
           Lang.frame_t Lang.unit_t
             (Frame.Fields.make
              (* We need to make sure that we are using a format here to
                 ensure that its params are properly unified with the underlying source. *)
                ~audio:
                  (Type.make
                     (Format_type.descr
                        (`Format
                           Ffmpeg_raw_content.Audio.(
                             lift_params (default_params `Raw)))))
                ())
         in
         let name = uniq_name "abuffer" in
         let s =
           Ffmpeg_filter_io.(
             audio_output ~pass_metadata ~name ~frame_t ~field source)
         in
         s#set_stack (Liquidsoap_lang.Lang_core.pos p);
         s#set_id id;
         Queue.push graph.graph_inputs (s :> Source.source);
         Queue.push graph.input_flushes (fun () -> s#flush_input);

         let args = ref None in

         let audio =
           Lazy.from_fun (fun () ->
               let _abuffer =
                 Avfilter.attach ~args:(Option.get !args) ~name Avfilter.abuffer
                   config
               in
               Avfilter.(
                 Hashtbl.replace graph.entries.inputs.audio name s#set_input);
               List.hd Avfilter.(_abuffer.io.outputs.audio))
         in

         Queue.push graph.input_inits (fun () -> Lazy.is_val audio);

         s#set_init (fun frame ->
             if !args = None then (
               args := Some (abuffer_args frame);
               ignore (Lazy.force audio);
               init_graph graph));

         Audio.to_value (`Output audio)));

  let return_t = Type.make (Format_type.descr raw_audio_format) in
  ignore
    (Lang.add_track_operator ~base:ffmpeg_filter_audio "output"
       ~category:`FFmpegFilter
       ~descr:"Return an audio track from a filter's output" ~return_t
       [
         ( "pass_metadata",
           Lang.bool_t,
           Some (Lang.bool true),
           Some "Pass ffmpeg stream metadata to liquidsoap" );
         ("", Graph.t, None, None);
         ("", Audio.t, None, None);
       ]
       (fun p ->
         let pass_metadata = Lang.to_bool (List.assoc "pass_metadata" p) in
         let graph_v = Lang.assoc "" 1 p in
         let config = get_config graph_v in
         let graph = Graph.of_value graph_v in

         (* No frame type is built here: [add_track_operator] constrains the
            field this output claims, and it must leave the rest of the source's
            row open for the graph's other outputs. *)
         let s = graph_source graph in
         let field = Frame.Fields.audio_n graph.audio_outputs in
         graph.audio_outputs <- graph.audio_outputs + 1;
         let sink =
           Ffmpeg_filter_io.audio_sink ~field ~pass_metadata ~log
             ~content_type:(fun () -> s#content_type)
             ()
         in
         s#add_sink
           {
             Ffmpeg_filter_graph.field;
             connected = (fun () -> sink#connected);
             drain = (fun ~generator -> sink#drain ~generator);
             eof = (fun () -> sink#eof);
           };

         let pad = Audio.of_value (Lang.assoc "" 2 p) in
         Queue.push graph.init
           (Lazy.from_fun (fun () ->
                let pad =
                  match pad with
                    | `Output pad -> Lazy.force pad
                    | _ -> assert false
                in
                let name = uniq_name "abuffersink" in
                let _abuffersink =
                  Avfilter.attach ~name Avfilter.abuffersink config
                in
                Avfilter.(link pad (List.hd _abuffersink.io.inputs.audio));
                Avfilter.(
                  Hashtbl.replace graph.entries.outputs.audio name
                    sink#set_output)));

         (field, (s :> Source.source))));

  ignore
    (Lang.add_builtin ~category:(`Source `FFmpegFilter)
       ~base:ffmpeg_filter_video "input"
       ~descr:"Attach a video track to a filter's input"
       [
         ("id", Lang.nullable_t Lang.string_t, Some Lang.null, None);
         ( "pass_metadata",
           Lang.bool_t,
           Some (Lang.bool true),
           Some "Pass liquidsoap's metadata to this stream" );
         ("", Graph.t, None, None);
         ("", video_frame_t, None, None);
       ]
       Video.t
       (fun p ->
         let id =
           Option.value ~default:"ffmpeg.filter.video.input"
             (Lang.to_valued_option Lang.to_string (List.assoc "id" p))
         in
         let pass_metadata = Lang.to_bool (List.assoc "pass_metadata" p) in
         let graph_v = Lang.assoc "" 1 p in
         let config = get_config graph_v in
         let graph = Graph.of_value graph_v in
         let track_val = Lang.assoc "" 2 p in
         let field, source = Lang.to_track track_val in

         let frame_t =
           Lang.frame_t Lang.unit_t
             (Frame.Fields.make
              (* We need to make sure that we are using a format here to
                 ensure that its params are properly unified with the underlying source. *)
                ~video:
                  (Type.make
                     (Format_type.descr
                        (`Format
                           Ffmpeg_raw_content.Video.(
                             lift_params (default_params `Raw)))))
                ())
         in
         let name = uniq_name "buffer" in
         let s =
           Ffmpeg_filter_io.(
             video_output ~pass_metadata ~name ~frame_t ~field source)
         in
         s#set_stack (Liquidsoap_lang.Lang_core.pos p);
         s#set_id id;
         Queue.push graph.graph_inputs (s :> Source.source);
         Queue.push graph.input_flushes (fun () -> s#flush_input);

         let args = ref None in

         let video =
           Lazy.from_fun (fun () ->
               let _buffer =
                 Avfilter.attach ~args:(Option.get !args) ~name Avfilter.buffer
                   config
               in
               Avfilter.(
                 Hashtbl.replace graph.entries.inputs.video name s#set_input);
               List.hd Avfilter.(_buffer.io.outputs.video))
         in

         Queue.push graph.input_inits (fun () -> Lazy.is_val video);

         s#set_init (fun frame ->
             if !args = None then (
               args := Some (buffer_args frame);
               ignore (Lazy.force video);
               init_graph graph));

         Video.to_value (`Output video)));

  let return_t = Type.make (Format_type.descr raw_video_format) in
  Lang.add_track_operator ~base:ffmpeg_filter_video "output" ~category:`Video
    ~descr:"Return a video track from a filter's output" ~return_t
    [
      ( "pass_metadata",
        Lang.bool_t,
        Some (Lang.bool true),
        Some "Pass ffmpeg stream metadata to liquidsoap" );
      ("", Graph.t, None, None);
      ("", Video.t, None, None);
    ]
    (fun p ->
      let pass_metadata = Lang.to_bool (List.assoc "pass_metadata" p) in
      let graph_v = Lang.assoc "" 1 p in
      let config = get_config graph_v in
      let graph = Graph.of_value graph_v in

      let s = graph_source graph in
      let field = Frame.Fields.video_n graph.video_outputs in
      graph.video_outputs <- graph.video_outputs + 1;
      let sink =
        Ffmpeg_filter_io.video_sink ~field ~pass_metadata ~log
          ~content_type:(fun () -> s#content_type)
          ()
      in
      s#add_sink
        {
          Ffmpeg_filter_graph.field;
          connected = (fun () -> sink#connected);
          drain = (fun ~generator -> sink#drain ~generator);
          eof = (fun () -> sink#eof);
        };

      Queue.push graph.init
        (Lazy.from_fun (fun () ->
             let pad =
               match Video.of_value (Lang.assoc "" 2 p) with
                 | `Output p -> Lazy.force p
                 | _ -> assert false
             in
             let name = uniq_name "buffersink" in
             let _buffersink =
               Avfilter.attach ~name Avfilter.buffersink config
             in
             Avfilter.(link pad (List.hd _buffersink.io.inputs.video));
             Avfilter.(
               Hashtbl.replace graph.entries.outputs.video name sink#set_output)));

      (field, (s :> Source.source)))

let unify_clocks ~clock sources =
  Queue.iter sources (fun s -> Clock.unify ~pos:s#pos clock s#clock)

let _ =
  let univ_t = Lang.univ_t () in
  Lang.add_builtin ~base:ffmpeg_filter "create"
    ~category:(`Source `FFmpegFilter)
    ~descr:"Configure and launch a filter graph"
    [("", Lang.fun_t [(false, "", Graph.t)] univ_t, None, None)]
    univ_t
    (fun p ->
      let fn = List.assoc "" p in
      let config = Avfilter.init () in
      let graph =
        Avfilter.
          {
            config = Some config;
            failed = false;
            input_inits = Queue.create ();
            graph_inputs = Queue.create ();
            input_flushes = Queue.create ();
            graph_source = None;
            audio_outputs = 0;
            video_outputs = 0;
            init = Queue.create ();
            entries =
              {
                inputs =
                  { audio = Hashtbl.create 10; video = Hashtbl.create 10 };
                outputs =
                  { audio = Hashtbl.create 10; video = Hashtbl.create 10 };
              };
          }
      in
      let ret = Lang.apply ~pos:(Lang.pos p) fn [("", Graph.to_value graph)] in
      let id = "ffmpeg.filter" in
      let output_clock = Clock.create ~id () in
      let controller =
        object
          method id = id
        end
      in
      let input_clock =
        Clock.create ~sync:`Passive ~id:(id ^ ".input")
          ~controller:(`Other ("ffmpeg filter graph", controller))
          ()
      in
      unify_clocks ~clock:input_clock graph.graph_inputs;
      (match graph.graph_source with
        | None -> ()
        | Some s -> Clock.unify ~pos:s#pos output_clock s#clock);
      (* We need an early registration for sources such as source.dynamic. *)
      Clock.register_sub_clock output_clock input_clock;
      (match graph.graph_source with
        | None -> ()
        | Some s ->
            s#on_wake_up (fun () ->
                (* Idempotent, so doing it twice the first time is fine. *)
                Clock.register_sub_clock output_clock input_clock);
            s#on_sleep (fun () ->
                Clock.deregister_sub_clock output_clock input_clock));

      Queue.push graph.init
        (Lazy.from_fun (fun () ->
             log#info "Initializing graph";
             let filter = Avfilter.launch config in
             Avfilter.(
               List.iter
                 (fun (name, input) ->
                   let set_input =
                     Hashtbl.find graph.entries.inputs.audio name
                   in
                   set_input input)
                 filter.inputs.audio);
             Avfilter.(
               List.iter
                 (fun (name, input) ->
                   let set_input =
                     Hashtbl.find graph.entries.inputs.video name
                   in
                   set_input input)
                 filter.inputs.video);
             Avfilter.(
               List.iter
                 (fun (name, output) ->
                   let set_output =
                     Hashtbl.find graph.entries.outputs.audio name
                   in
                   set_output output)
                 filter.outputs.audio);
             Avfilter.(
               List.iter
                 (fun (name, output) ->
                   let set_output =
                     Hashtbl.find graph.entries.outputs.video name
                   in
                   set_output output)
                 filter.outputs.video)));
      graph.config <- None;
      ret)
