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

module V = Filter_values
module D = Description

let filter_module = Lang.add_module ~base:Lang_modules.ffmpeg "filter"
let audio_module = Lang.add_module ~base:filter_module "audio"
let video_module = Lang.add_module ~base:filter_module "video"
let category = `Source `FFmpegFilter
let kind_t kind = Type.make (Format_type.descr (`Kind kind))
let filter_error ~pos message = Lang.raise_error ~pos ~message "ffmpeg.filter"

let value_pos value =
  match Value.pos value with Some pos -> [pos] | None -> []

let audio_pad v = V.Audio_pad.of_value (Lang.demeth v)
let video_pad v = V.Video_pad.of_value (Lang.demeth v)
let graph_value v = V.Graph_value.of_value (Lang.demeth v)

(* §7.3. *)
let graph_of value =
  let graph = graph_value value in
  if not (Graph.in_scope graph) then
    raise
      (Error.Invalid_value
         ( value,
           "Graph variables cannot be used outside of ffmpeg.filter.create!",
           [] ));
  graph

(* Errors of a replay into the probe config, at a script position. *)
let at ~pos fn =
  try fn () with
    | Avutil.Error e -> filter_error ~pos (Avutil.string_of_error e)
    | Failure message -> filter_error ~pos message
    | Avfilter.Exists -> filter_error ~pos "Duplicate filter instance name"

let graph_arg = ("", V.Graph_value.t, None, None)

let id_arg default =
  ( "id",
    Lang.nullable_t Lang.string_t,
    Some Lang.null,
    Some
      (Printf.sprintf "Force the value of the source ID (default: %s)." default)
  )

let pass_metadata_arg descr =
  ("pass_metadata", Lang.bool_t, Some (Lang.bool true), Some descr)

(* §7. *)
let () =
  let a = Lang.univ_t () in
  ignore
    (Lang.add_builtin ~base:filter_module "create" ~category
       ~descr:"Configure and launch a filter graph."
       [("", Lang.fun_t [(false, "", V.Graph_value.t)] a, None, None)]
       a
       (fun p ->
         let pos = Lang.pos p in
         let graph = Graph.create () in
         let result =
           Fun.protect
             ~finally:(fun () -> D.close (Graph.description graph))
             (fun () ->
               let result =
                 Lang.apply (List.assoc "" p)
                   [("", V.Graph_value.to_value graph)]
               in
               at ~pos (fun () ->
                   Graph.close graph
                     ~pos:(match pos with p :: _ -> Some p | [] -> None));
               result)
         in
         result))

(* §8. *)
let input ~base ~media ~kind ~pad_t ~to_value =
  let name = Printf.sprintf "ffmpeg.filter.%s.input" (D.media_name media) in
  ignore
    (Lang.add_builtin ~base "input" ~category
       ~descr:"Attach a track to a filter graph as one of its inputs."
       [
         id_arg name;
         pass_metadata_arg "Pass liquidsoap's metadata to this input's frames.";
         graph_arg;
         ("", kind_t kind, None, None);
       ]
       pad_t
       (fun p ->
         let graph = graph_of (Lang.assoc "" 1 p) in
         let id =
           Lang.to_default_option ~default:name Lang.to_string
             (List.assoc "id" p)
         in
         let pass_metadata = Lang.to_bool (List.assoc "pass_metadata" p) in
         let track = Lang.to_track (Lang.assoc "" 2 p) in
         at ~pos:(Lang.pos p) (fun () ->
             to_value graph ~id ~pass_metadata track)))

let () =
  input ~base:audio_module ~media:`Audio ~kind:Ffmpeg_content.Audio.kind
    ~pad_t:V.Audio_pad.t ~to_value:(fun graph ~id ~pass_metadata track ->
      V.Audio_pad.to_value
        (Graph.add_input graph Graph.audio ~id ~pass_metadata track));
  input ~base:video_module ~media:`Video ~kind:Ffmpeg_content.Video.kind
    ~pad_t:V.Video_pad.t ~to_value:(fun graph ~id ~pass_metadata track ->
      V.Video_pad.to_value
        (Graph.add_input graph Graph.video ~id ~pass_metadata track))

(* §9. *)
let output ~base ~kind ~pad_t ~add =
  ignore
    (Lang.add_track_operator ~base "output" ~category:`FFmpegFilter
       ~descr:
         "Return a track of the graph source: one output of a filter graph. \
          The graph source is fallible: read it through `fallback` or `mksafe` \
          when its consumer needs an infallible source."
       [
         pass_metadata_arg "Pass frame metadata out of the graph.";
         graph_arg;
         ("", pad_t, None, None);
       ]
       ~return_t:(kind_t kind)
       (fun p ->
         let graph = graph_of (Lang.assoc "" 1 p) in
         let pass_metadata = Lang.to_bool (List.assoc "pass_metadata" p) in
         let pad = Lang.assoc "" 2 p in
         at ~pos:(Lang.pos p) (fun () -> add graph ~pass_metadata pad)))

let () =
  output ~base:audio_module ~kind:Ffmpeg_content.Audio.kind ~pad_t:V.Audio_pad.t
    ~add:(fun graph ~pass_metadata pad ->
      Graph.add_output graph Graph.audio ~make_sink:Graph.audio_sink
        ~pass_metadata (audio_pad pad));
  output ~base:video_module ~kind:Ffmpeg_content.Video.kind ~pad_t:V.Video_pad.t
    ~add:(fun graph ~pass_metadata pad ->
      Graph.add_output graph Graph.video ~make_sink:Graph.video_sink
        ~pass_metadata (video_pad pad))

(* §15. *)
let () =
  let pads_t =
    Lang.record_t
      [
        ("audio", Lang.list_t (Lang.product_t Lang.string_t V.Audio_pad.t));
        ("video", Lang.list_t (Lang.product_t Lang.string_t V.Video_pad.t));
      ]
  in
  let labels_t =
    Lang.record_t
      [
        ("audio", Lang.list_t Lang.string_t);
        ("video", Lang.list_t Lang.string_t);
      ]
  in
  let empty = Lang.record [("audio", Lang.list []); ("video", Lang.list [])] in
  ignore
    (Lang.add_builtin ~base:filter_module "parse" ~category ~flags:[`Extra]
       ~descr:
         "Add the filters of a textual description, in FFmpeg's filtergraph \
          syntax, to the graph."
       [
         graph_arg;
         ( "description",
           Lang.string_t,
           None,
           Some "The filtergraph description." );
         ( "inputs",
           pads_t,
           Some empty,
           Some "Pads feeding the open inputs of the description, by label." );
         ( "outputs",
           labels_t,
           Some empty,
           Some "Labels of the open outputs of the description to return." );
       ]
       pads_t
       (fun p ->
         let graph = graph_of (Lang.assoc "" 1 p) in
         let field v name = Lang.to_list (Value.invoke v name) in
         let inputs = List.assoc "inputs" p in
         let outputs = List.assoc "outputs" p in
         let labelled of_value v =
           let label, pad = Lang.to_product v in
           (Lang.to_string label, of_value pad)
         in
         let audio, video =
           at ~pos:(Lang.pos p) (fun () ->
               Graph.parse graph
                 ~description:(Lang.to_string (List.assoc "description" p))
                 ~audio_inputs:
                   (List.map (labelled audio_pad) (field inputs "audio"))
                 ~video_inputs:
                   (List.map (labelled video_pad) (field inputs "video"))
                 ~audio_outputs:
                   (List.map Lang.to_string (field outputs "audio"))
                 ~video_outputs:
                   (List.map Lang.to_string (field outputs "video")))
         in
         let pairs to_value =
           List.map (fun (label, pad) ->
               Lang.product (Lang.string label) (to_value pad))
         in
         Lang.record
           [
             ("audio", Lang.list (pairs V.Audio_pad.to_value audio));
             ("video", Lang.list (pairs V.Video_pad.to_value video));
           ]))

(* §4 and §6: two builtins per filter of the registry. *)
let register (filter : [ `Unattached ] Avfilter.filter) =
  let args = Filter_args.of_filter filter in
  let dynamic_inputs = List.mem `Dynamic_inputs filter.flags in
  let dynamic_outputs = List.mem `Dynamic_outputs filter.flags in
  let input_params =
    if dynamic_inputs then
      [
        (false, "", Lang.list_t V.Audio_pad.t);
        (false, "", Lang.list_t V.Video_pad.t);
      ]
    else
      List.map (fun _ -> (false, "", V.Audio_pad.t)) filter.io.inputs.audio
      @ List.map (fun _ -> (false, "", V.Video_pad.t)) filter.io.inputs.video
  in
  let output_t =
    if dynamic_outputs then
      Lang.product_t (Lang.list_t V.Audio_pad.t) (Lang.list_t V.Video_pad.t)
    else (
      match
        List.map (fun _ -> V.Audio_pad.t) filter.io.outputs.audio
        @ List.map (fun _ -> V.Video_pad.t) filter.io.outputs.video
      with
        | [t] -> t
        | ts -> Lang.tuple_t ts)
  in
  let description =
    let d = filter.description in
    let d =
      if String.length d > 0 && d.[String.length d - 1] = '.' then
        String.sub d 0 (String.length d - 1)
      else d
    in
    String.concat " "
      ([Printf.sprintf "Ffmpeg filter: %s." d]
      @ (if dynamic_inputs then
           [
             "This filter has dynamic inputs: last two arguments are lists of \
              audio and video inputs. Total number of inputs is determined at \
              runtime.";
           ]
         else [])
      @
      if dynamic_outputs then
        [
          "This filter has dynamic outputs: returned value is a tuple of audio \
           and video outputs. Total number of outputs is determined at \
           runtime.";
        ]
      else [])
  in
  let args_proto =
    List.map
      (fun (arg : Filter_args.arg) ->
        (arg.name, Lang.nullable_t arg.typ, Some Lang.null, arg.doc))
      args
  in
  let create p =
    let graph = graph_of (Lang.assoc "" 1 p) in
    let pos = Lang.pos p in
    let rendered = Filter_args.render args p in
    let e = at ~pos (fun () -> Graph.add_instance graph filter rendered) in
    let attached = D.attached e in
    let audio_outputs =
      List.mapi
        (fun i _ ->
          V.Audio_pad.to_value (D.audio_output (Graph.description graph) e i))
        attached.io.outputs.audio
    in
    let video_outputs =
      List.mapi
        (fun i _ ->
          V.Video_pad.to_value (D.video_output (Graph.description graph) e i))
        attached.io.outputs.video
    in
    let output =
      if dynamic_outputs then
        Lang.product (Lang.list audio_outputs) (Lang.list video_outputs)
      else (
        match audio_outputs @ video_outputs with
          | [v] -> v
          | vs -> Lang.tuple vs)
    in
    let input_set = ref false in
    let link media i value =
      at ~pos:(value_pos value) (fun () ->
          match media with
            | `Audio ->
                Graph.link_input graph (audio_pad value) e `Audio i
                  (fun (f : [ `Attached ] Avfilter.filter) -> f.io.inputs.audio)
            | `Video ->
                Graph.link_input graph (video_pad value) e `Video i
                  (fun (f : [ `Attached ] Avfilter.filter) -> f.io.inputs.video))
    in
    let set_input values =
      (match values with
        | first :: _ when !input_set ->
            filter_error ~pos:(value_pos first) "Filter input already set!"
        | _ -> input_set := true);
      let n_audio = List.length attached.io.inputs.audio in
      let n_video = List.length attached.io.inputs.video in
      let audio, video =
        if dynamic_inputs then (
          let check list expected media =
            let given = List.length (Lang.to_list list) in
            if given <> expected then
              raise
                (Error.Invalid_value
                   ( list,
                     Printf.sprintf "Filter %s expects %d %s inputs, %d given."
                       filter.name expected media given,
                     [] ))
          in
          let audio = List.nth values 0 and video = List.nth values 1 in
          check audio n_audio "audio";
          check video n_video "video";
          (Lang.to_list audio, Lang.to_list video))
        else
          ( List.filteri (fun i _ -> i < n_audio) values,
            List.filteri (fun i _ -> i >= n_audio) values )
      in
      List.iteri (link `Audio) audio;
      List.iteri (link `Video) video
    in
    let names =
      List.mapi (fun i _ -> Printf.sprintf "input%d" i) input_params
    in
    let set_input_value =
      Lang.val_fun
        (List.map (fun name -> ("", name, None)) names)
        (fun p ->
          set_input (List.map (fun name -> List.assoc name p) names);
          Lang.unit)
    in
    let process_command =
      Lang.val_fun
        [
          ("fast", "fast", Some (Lang.bool false));
          ("", "command", None);
          ("", "argument", None);
        ]
        (fun p ->
          Lang.string
            (Graph.process_command graph e
               ~fast:(Lang.to_bool (List.assoc "fast" p))
               (Lang.to_string (List.assoc "command" p))
               (Lang.to_string (List.assoc "argument" p))))
    in
    (output, set_input, set_input_value, process_command)
  in
  let base =
    Lang.add_builtin ~base:filter_module filter.name ~category ~flags:[`Extra]
      ~descr:description
      (args_proto @ [graph_arg]
      @ List.map (fun (_, _, t) -> ("", t, None, None)) input_params)
      output_t
      (fun p ->
        let n = List.length input_params in
        let unlabelled = List.filter (fun (l, _) -> l = "") p in
        let leading = List.length unlabelled - n in
        let inputs = List.filteri (fun i _ -> i >= leading) unlabelled in
        let output, set_input, _, _ = create p in
        set_input (List.map snd inputs);
        output)
  in
  ignore
    (Lang.add_builtin ~base "create" ~category ~flags:[`Extra]
       ~descr:
         (description
        ^ " Use this operator to initiate the filter independently of its \
           inputs, to be able to send commands to the filter instance.")
       (args_proto @ [graph_arg])
       (Lang.method_t Lang.unit_t
          [
            ("output", ([], output_t), "The outputs of the filter instance.");
            ( "process_command",
              ( [],
                Lang.fun_t
                  [
                    (true, "fast", Lang.bool_t);
                    (false, "", Lang.string_t);
                    (false, "", Lang.string_t);
                  ]
                  Lang.string_t ),
              "Send a command to the filter instance of the running graph." );
            ( "set_input",
              ([], Lang.fun_t input_params Lang.unit_t),
              "Set the inputs of the filter instance." );
          ])
       (fun p ->
         let output, _, set_input_value, process_command = create p in
         Lang.meth Lang.unit
           [
             ("output", output);
             ("process_command", process_command);
             ("set_input", set_input_value);
           ]))

let () = List.iter register Avfilter.filters
