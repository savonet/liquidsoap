(*****************************************************************************

  Liquidsoap, a programmable stream generator.
  Copyright 2003-2024 Savonet team

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
open Builtins_ffmpeg_filters

let _ =
  Lang.add_builtin ~base:ffmpeg_filter "parse" ~category:(`Source `FFmpegFilter)
    ~descr:
      "Parse a filter graph description string and attach it to the current \
       graph. This allows using FFmpeg's filtergraph syntax to describe \
       complex filter chains (see \
       https://ffmpeg.org/ffmpeg-filters.html#Filtergraph-syntax-1). The graph \
       description can reference input pads by name (e.g., '[in0]', '[in1]') \
       and define output pads (e.g., '[out0]', '[out1]')."
    ~flags:[`Extra]
    [
      ("", Graph.t, None, Some "Filter graph");
      ( "description",
        Lang.string_t,
        None,
        Some
          "Filter graph description using FFmpeg syntax (e.g., \
           '[in0][in1]amix=inputs=2[out]')" );
      ( "inputs",
        Lang.record_t
          [
            ("audio", Lang.list_t (Lang.product_t Lang.string_t Audio.t));
            ("video", Lang.list_t (Lang.product_t Lang.string_t Video.t));
          ],
        Some (Lang.record [("audio", Lang.list []); ("video", Lang.list [])]),
        Some
          "Record with audio and video fields containing lists of (name, pad) \
           tuples. The pads should be output pads from previous filters. The \
           names should match input labels in the graph description." );
      ( "outputs",
        Lang.record_t
          [
            ("audio", Lang.list_t Lang.string_t);
            ("video", Lang.list_t Lang.string_t);
          ],
        Some (Lang.record [("audio", Lang.list []); ("video", Lang.list [])]),
        Some
          "Record with audio and video fields containing lists of output pad \
           names to extract from the parsed graph. These names should match \
           output labels in the graph description." );
    ]
    (Lang.record_t
       [
         ("audio", Lang.list_t (Lang.product_t Lang.string_t Audio.t));
         ("video", Lang.list_t (Lang.product_t Lang.string_t Video.t));
       ])
    (fun p ->
      let graph_v = Lang.assoc "" 1 p in
      let config = get_config graph_v in
      let graph = Graph.of_value graph_v in
      let description = Lang.to_string (List.assoc "description" p) in

      let inputs_record = List.assoc "inputs" p in
      let inputs_meths, _ = Lang.split_meths inputs_record in
      let audio_input_pads =
        Lang.to_list (List.assoc "audio" inputs_meths)
        |> List.map (fun v ->
            let name_v, pad_v = Lang.to_product v in
            (Lang.to_string name_v, pad_v))
      in
      let video_input_pads =
        Lang.to_list (List.assoc "video" inputs_meths)
        |> List.map (fun v ->
            let name_v, pad_v = Lang.to_product v in
            (Lang.to_string name_v, pad_v))
      in

      let outputs_record = List.assoc "outputs" p in
      let outputs_meths, _ = Lang.split_meths outputs_record in
      let audio_output_names =
        Lang.to_list (List.assoc "audio" outputs_meths)
        |> List.map Lang.to_string
      in
      let video_output_names =
        Lang.to_list (List.assoc "video" outputs_meths)
        |> List.map Lang.to_string
      in

      let output_format_filters = ref None in

      Queue.push graph.init
        (Lazy.from_fun (fun () ->
             let audio_output_formats =
               List.map
                 (fun name ->
                   let aformat_name = uniq_name ("parse_aformat_out_" ^ name) in
                   let aformat =
                     Avfilter.attach ~name:aformat_name
                       (Avfilter.find "aformat") config
                   in
                   (name, aformat))
                 audio_output_names
             in

             let video_output_formats =
               List.map
                 (fun name ->
                   let format_name = uniq_name ("parse_format_out_" ^ name) in
                   let format_ =
                     Avfilter.attach ~name:format_name (Avfilter.find "format")
                       config
                   in
                   (name, format_))
                 video_output_names
             in

             let audio_output_pads =
               List.map
                 (fun (name, pad_v) ->
                   let caller_output =
                     match Audio.of_value pad_v with
                       | `Output lazy_pad -> Lazy.force lazy_pad
                       | `Input _ ->
                           failwith
                             "ffmpeg.filter.parse: input pads must be output \
                              pads from previous filters"
                   in
                   Avfilter.
                     {
                       node_name = name;
                       node_args = None;
                       node_pad = caller_output;
                     })
                 audio_input_pads
             in

             let video_output_pads =
               List.map
                 (fun (name, pad_v) ->
                   let caller_output =
                     match Video.of_value pad_v with
                       | `Output lazy_pad -> Lazy.force lazy_pad
                       | `Input _ ->
                           failwith
                             "ffmpeg.filter.parse: input pads must be output \
                              pads from previous filters"
                   in
                   Avfilter.
                     {
                       node_name = name;
                       node_args = None;
                       node_pad = caller_output;
                     })
                 video_input_pads
             in

             let audio_input_pads =
               List.map
                 (fun (name, aformat) ->
                   Avfilter.
                     {
                       node_name = name;
                       node_args = None;
                       node_pad = List.hd aformat.io.inputs.audio;
                     })
                 audio_output_formats
             in

             let video_input_pads =
               List.map
                 (fun (name, format_) ->
                   Avfilter.
                     {
                       node_name = name;
                       node_args = None;
                       node_pad = List.hd format_.io.inputs.video;
                     })
                 video_output_formats
             in

             let parse_io =
               Avfilter.
                 {
                   inputs =
                     { audio = audio_input_pads; video = video_input_pads };
                   outputs =
                     { audio = audio_output_pads; video = video_output_pads };
                 }
             in

             Avfilter.parse parse_io description config;

             output_format_filters :=
               Some (audio_output_formats, video_output_formats)));

      Builtins_ffmpeg_filters.init_graph graph;

      let make_audio_output name =
        let lazy_pad =
          Lazy.from_fun (fun () ->
              let audio_formats, _video_formats =
                Option.get !output_format_filters
              in
              let _, aformat =
                List.find (fun (n, _) -> n = name) audio_formats
              in
              List.hd aformat.io.outputs.audio)
        in
        Lang.product (Lang.string name) (Audio.to_value (`Output lazy_pad))
      in

      let make_video_output name =
        let lazy_pad =
          Lazy.from_fun (fun () ->
              let _audio_formats, video_formats =
                Option.get !output_format_filters
              in
              let _, format_ =
                List.find (fun (n, _) -> n = name) video_formats
              in
              List.hd format_.io.outputs.video)
        in
        Lang.product (Lang.string name) (Video.to_value (`Output lazy_pad))
      in

      Lang.record
        [
          ("audio", Lang.list (List.map make_audio_output audio_output_names));
          ("video", Lang.list (List.map make_video_output video_output_names));
        ])
