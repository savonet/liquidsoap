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
      ?source_fps ~target_fps () =
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
      ?source_fps ~target_fps () =
    match source_fps with
      | Some f when f = target_fps -> `Pass_through time_base
      | _ ->
          `Filter
            (init ?start_pts ~width ~height ~pixel_format ~time_base
               ?pixel_aspect ?source_fps ~target_fps ())

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
          `Pair
            ( "channel_layout",
              `String (Avutil.Channel_layout.get_description src.channel_layout)
            );
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
          `Pair
            ( "channel_layouts",
              `String (Avutil.Channel_layout.get_description dst.channel_layout)
            );
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
