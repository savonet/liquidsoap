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

module Display = struct
  type t = Avfilter.Utils.display_converter option

  let on_undecided = Atomic.make None
  let set_on_undecided handler = Atomic.set on_undecided (Some handler)

  let cropping params =
    Avcodec.Packet_side_data.cropping (Avcodec.params_side_data params)

  let stored_layout ?pixel_aspect params =
    {
      Avfilter.Utils.width = Avcodec.Video.get_width params;
      height = Avcodec.Video.get_height params;
      pixel_aspect =
        (match pixel_aspect with
          | Some _ -> pixel_aspect
          | None -> Avcodec.Video.get_pixel_aspect params);
      filters = [];
    }

  (* [None] when the layout is not decided. *)
  let layout ~stored params =
    if not Ffmpeg_utils.conf_autorotate#get then Some stored
    else (
      let { Avfilter.Utils.width; height; pixel_aspect; _ } = stored in
      match
        Avfilter.Utils.display_layout ?cropping:(cropping params)
          ?display_matrix:
            (Avcodec.Packet_side_data.display_matrix
               (Avcodec.params_side_data params))
          ?pixel_aspect ~width ~height ()
      with
        | `Layout layout -> Some layout
        | `Undecided _ -> None)

  let displayed ?pixel_aspect params =
    let stored = stored_layout ?pixel_aspect params in
    Option.value (layout ~stored params) ~default:stored

  let expected_size params =
    let { Avfilter.Utils.width; height; _ } = displayed params in
    (width, height)

  let pixel_aspect ?stored params =
    (displayed ?pixel_aspect:stored params).Avfilter.Utils.pixel_aspect

  let init ?params ~time_base () =
    if Ffmpeg_utils.conf_autorotate#get then
      Some
        (Avfilter.Utils.init_display_converter
           ?cropping:(Option.bind params cropping)
           ?on_undecided:(Atomic.get on_undecided)
           ~ignore:(Ffmpeg_utils.ignored_video_properties ())
           ~time_base ())
    else None

  let convert display frame callback =
    match display with
      | None -> callback frame
      | Some converter ->
          Avfilter.Utils.convert_display converter callback (`Frame frame)

  let eof display callback =
    Option.iter
      (fun converter ->
        Avfilter.Utils.convert_display converter callback `Flush)
      display
end

module Fps = struct
  type filter = {
    time_base : Avutil.rational;
    input : [ `Video ] Avfilter.input;
    output : [ `Video ] Avfilter.output;
  }

  type t = [ `Filter of filter | `Pass_through of Avutil.rational ]

  let time_base = function
    | `Filter { time_base } -> time_base
    | `Pass_through time_base -> time_base

  let init ?start_pts ~width ~height ~pixel_format ~time_base ?pixel_aspect
      ?source_fps ?color_range ~target_fps () =
    let config = Avfilter.init () in
    let _buffer =
      let args =
        [
          `Pair ("video_size", `String (Printf.sprintf "%dx%d" width height));
          `Pair ("pix_fmt", `Int (Avutil.Pixel_format.get_id pixel_format));
          `Pair ("time_base", `Rational time_base);
        ]
        @
          match pixel_aspect with
          | None -> []
          | Some p -> [`Pair ("pixel_aspect", `Rational p)]
      in
      let args =
        match color_range with
          | None -> args
          | Some cr ->
              `Pair ("range", `String (Avutil.Color_range.name cr)) :: args
      in
      let args =
        match source_fps with
          | None -> args
          | Some fps ->
              `Pair ("frame_rate", `Rational { Avutil.num = fps; den = 1 })
              :: args
      in
      Avfilter.attach ~name:"buffer" ~args Avfilter.buffer config
    in
    (* There are two use-case:
       - Decoder assumes no `start_pts` and want to keep negative
         STARTPTS (to be skipped) but re-align positive STARTPTS
         in case the file is a partial copy dump.
       - Encoder wants to apply `start_pts` all the time and realign
         all PTS accordingly. *)
    let setpts =
      match
        List.find_opt
          (fun { Avfilter.name } -> name = "setpts")
          Avfilter.filters
      with
        | Some setpts -> setpts
        | None -> failwith "Could not find setpts ffmpeg filter!"
    in
    let setpts =
      let args =
        match start_pts with
          | Some start_pts ->
              [
                `Pair
                  ("expr", `String (Printf.sprintf "%Ld+PTS-STARTPTS" start_pts));
              ]
          | None -> [`Pair ("expr", `String "PTS-max(STARTPTS, 0)")]
      in
      Avfilter.attach ~name:"setpts" ~args setpts config
    in
    let fps =
      match
        List.find_opt (fun { Avfilter.name } -> name = "fps") Avfilter.filters
      with
        | Some fps -> fps
        | None -> failwith "Could not find fps ffmpeg filter!"
    in
    let fps =
      let args =
        [`Pair ("fps", `Rational { Avutil.num = target_fps; den = 1 })]
      in
      let args =
        if start_pts = None then `Pair ("start_time", `Int 0) :: args else args
      in
      Avfilter.attach ~name:"fps" ~args fps config
    in
    let _buffersink =
      Avfilter.attach ~name:"buffersink" Avfilter.buffersink config
    in
    Avfilter.link
      (List.hd Avfilter.(_buffer.io.outputs.video))
      (List.hd Avfilter.(setpts.io.inputs.video));
    Avfilter.link
      (List.hd Avfilter.(setpts.io.outputs.video))
      (List.hd Avfilter.(fps.io.inputs.video));
    Avfilter.link
      (List.hd Avfilter.(fps.io.outputs.video))
      (List.hd Avfilter.(_buffersink.io.inputs.video));
    let graph = Avfilter.launch config in
    let _, input = List.hd Avfilter.(graph.inputs.video) in
    let _, output = List.hd Avfilter.(graph.outputs.video) in
    let time_base = Avfilter.(time_base output.context) in
    { input; output; time_base }

  (* Source fps is not always known so it is optional here. *)
  let init ?start_pts ~width ~height ~pixel_format ~time_base ?pixel_aspect
      ?source_fps ?color_range ~target_fps () =
    match source_fps with
      | Some f when f = target_fps -> `Pass_through time_base
      | _ ->
          `Filter
            (init ?start_pts ~width ~height ~pixel_format ~time_base
               ?pixel_aspect ?source_fps ?color_range ~target_fps ())

  let of_frame_format ~(format : Avutil.Video.frame_format) ~time_base
      ~target_fps () =
    init ~width:format.width ~height:format.height
      ~pixel_format:format.pixel_format ~time_base
      ?pixel_aspect:format.pixel_aspect
      ?color_range:
        (match format.color_range with
          | `Unspecified -> None
          | color_range -> Some color_range)
      ~target_fps ()

  let rec flush cb output =
    try
      cb (output.Avfilter.handler ());
      flush cb output
    with Avutil.Error `Eagain -> ()

  let convert converter frame cb =
    match converter with
      | `Pass_through _ -> cb frame
      | `Filter { input; output } ->
          input (`Frame frame);
          flush cb output

  let eof converter cb =
    match converter with
      | `Pass_through _ -> ()
      | `Filter { input; output } -> (
          input `Flush;
          try flush cb output with Avutil.Error `Eof -> ())
end

module AFormat = struct
  type filter = {
    input : [ `Audio ] Avfilter.input;
    output : [ `Audio ] Avfilter.output;
    time_base : Avutil.rational;
  }

  type t = [ `Filter of filter | `Pass_through of Avutil.rational ]

  type config = {
    sample_format : Avutil.Sample_format.t;
    channel_layout : Avutil.Channel_layout.t;
    sample_rate : int;
  }

  let channel_layout_arg channel_layout =
    match Avutil.Channel_layout.get_mask channel_layout with
      | Some id -> `Int64 id
      | None ->
          let channel_layout =
            Avutil.Channel_layout.get_default
              (Avutil.Channel_layout.get_nb_channels channel_layout)
          in
          `Int64 (Option.get (Avutil.Channel_layout.get_mask channel_layout))

  let time_base = function
    | `Filter { time_base } -> time_base
    | `Pass_through time_base -> time_base

  let init ~src ~dst ~src_time_base () =
    let config = Avfilter.init () in
    let _abuffer =
      let args =
        [
          `Pair
            ( "sample_fmt",
              `String
                (Option.get (Avutil.Sample_format.get_name src.sample_format))
            );
          `Pair ("channel_layout", channel_layout_arg src.channel_layout);
          `Pair ("sample_rate", `Int src.sample_rate);
          `Pair ("time_base", `Rational src_time_base);
        ]
      in
      Avfilter.attach ~name:"abuffer" ~args Avfilter.abuffer config
    in
    let aformat =
      match
        List.find_opt
          (fun { Avfilter.name } -> name = "aformat")
          Avfilter.filters
      with
        | Some aformat -> aformat
        | None -> failwith "Could not find aformat ffmpeg filter!"
    in
    let aformat =
      let args =
        [
          `Pair
            ( "sample_fmts",
              `String
                (Option.get (Avutil.Sample_format.get_name dst.sample_format))
            );
          `Pair ("channel_layouts", channel_layout_arg dst.channel_layout);
          `Pair ("sample_rates", `Int dst.sample_rate);
        ]
      in
      Avfilter.attach ~name:"aformat" ~args aformat config
    in
    let _abuffersink =
      Avfilter.attach ~name:"abuffersink" Avfilter.abuffersink config
    in
    Avfilter.link
      (List.hd Avfilter.(_abuffer.io.outputs.audio))
      (List.hd Avfilter.(aformat.io.inputs.audio));
    Avfilter.link
      (List.hd Avfilter.(aformat.io.outputs.audio))
      (List.hd Avfilter.(_abuffersink.io.inputs.audio));
    let graph = Avfilter.launch config in
    let _, input = List.hd Avfilter.(graph.inputs.audio) in
    let _, output = List.hd Avfilter.(graph.outputs.audio) in
    let time_base = Avfilter.(time_base output.context) in
    { input; output; time_base }

  let init ?dst_sample_format ?dst_channel_layout ?dst_sample_rate
      ~src_sample_format ~src_channel_layout ~src_sample_rate ~src_time_base ()
      =
    let dst_sample_format =
      Option.value ~default:src_sample_format dst_sample_format
    in
    let dst_channel_layout =
      Option.value ~default:src_channel_layout dst_channel_layout
    in
    let dst_sample_rate =
      Option.value ~default:src_sample_rate dst_sample_rate
    in
    if
      src_sample_format == dst_sample_format
      && Avutil.Channel_layout.compare src_channel_layout dst_channel_layout
      && src_sample_rate == dst_sample_rate
    then `Pass_through src_time_base
    else (
      let src =
        {
          sample_format = src_sample_format;
          channel_layout = src_channel_layout;
          sample_rate = src_sample_rate;
        }
      in
      let dst =
        {
          sample_format = dst_sample_format;
          channel_layout = dst_channel_layout;
          sample_rate = dst_sample_rate;
        }
      in
      `Filter (init ~src ~dst ~src_time_base ()))

  let rec flush cb output =
    try
      cb (output.Avfilter.handler ());
      flush cb output
    with Avutil.Error `Eagain -> ()

  let convert converter frame cb =
    match converter with
      | `Pass_through _ -> cb frame
      | `Filter { input; output } ->
          input (`Frame frame);
          flush cb output

  let eof converter cb =
    match converter with
      | `Pass_through _ -> ()
      | `Filter { input; output } -> (
          input `Flush;
          try flush cb output with Avutil.Error `Eof -> ())
end

module Fit = struct
  type target = {
    width : int;
    height : int;
    pixel_format : Avutil.Pixel_format.t;
    pixel_aspect : Avutil.rational option;
  }

  type graph = {
    source : Avutil.Video.frame_format;
    target : target;
    input : [ `Video ] Avfilter.input;
    output : [ `Video ] Avfilter.output;
  }

  type t = { mutable graph : graph option }

  let init () = { graph = None }
  let square = { Avutil.num = 1; den = 1 }

  (* The largest size inside the target that shows the source with its
     proportions, both pixel aspects accounted for. *)
  let fitted_size ?(pixel_aspect = square) ~width ~height
      (source : Avutil.Video.frame_format) =
    let source_aspect = Option.value ~default:square source.pixel_aspect in
    let num = source.width * source_aspect.num * pixel_aspect.Avutil.den in
    let den = source.height * source_aspect.den * pixel_aspect.num in
    if num * height <= width * den then (max 1 (height * num / den), height)
    else (width, max 1 (width * den / num))

  (* TODO: [scale] drops the chroma location of the frame, so a consumer built
     for the frames before the fit sees one more format change. *)
  let filters ~(source : Avutil.Video.frame_format) target =
    let known_aspect =
      match target.pixel_aspect with
        | Some _ as pixel_aspect -> pixel_aspect
        | None -> source.pixel_aspect
    in
    let width, height =
      fitted_size ?pixel_aspect:known_aspect ~width:target.width
        ~height:target.height source
    in
    [
      ( "scale",
        [
          `Pair ("w", `Int width);
          `Pair ("h", `Int height);
          `Pair ("flags", `String Ffmpeg_utils.conf_scaling_algorithm#get);
        ] );
      ( "pad",
        [
          `Pair ("w", `Int target.width);
          `Pair ("h", `Int target.height);
          `Pair ("x", `String "(ow-iw)/2");
          `Pair ("y", `String "(oh-ih)/2");
        ] );
      ( "setsar",
        [
          `Pair
            ( "sar",
              `Rational
                (Option.value ~default:{ Avutil.num = 0; den = 1 } known_aspect)
            );
        ] );
      ( "format",
        [
          `Pair
            ( "pix_fmts",
              `String
                (Option.get (Avutil.Pixel_format.to_string target.pixel_format))
            );
        ] );
    ]

  let build ~source target =
    let { Avfilter.Utils.chain_source; chain_sink } =
      Avfilter.Utils.video_chain
        ~time_base:(Ffmpeg_utils.liq_main_ticks_time_base ())
        source (filters ~source target)
    in
    { source; target; input = chain_source; output = chain_sink }

  let graph fit ~source target =
    match fit.graph with
      | Some graph
        when graph.target = target
             && Ffmpeg_utils.same_video_format graph.source source ->
          graph
      | _ ->
          let graph = build ~source target in
          fit.graph <- Some graph;
          graph

  let rec deliver cb output =
    match output.Avfilter.handler () with
      | frame ->
          cb frame;
          deliver cb output
      | exception Avutil.Error (`Eagain | `Eof) -> ()

  let convert fit ~target frame cb =
    let { input; output; _ } =
      graph fit ~source:(Avutil.Video.frame_format frame) target
    in
    input (`Frame frame);
    deliver cb output
end
