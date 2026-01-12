(*****************************************************************************

  Liquidsoap, a programmable stream generator.
  Rawright 2003-2026 Savonet team

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

open Avutil

type 'a frame = {
  stream_idx : Int64.t;
  time_base : Avutil.rational;
  frame : 'a Avutil.frame;
}

module BaseSpecs = struct
  include Content_video.Base

  type kind = [ `Raw ]

  let kind = `Raw

  (* No frame copy for now. *)
  let blit = fill

  let copy : 'a. ('a, 'b) content -> ('a, 'b) content =
   fun src -> copy ~copy:(fun x -> x) src

  let internal_content_type = None
end

module AudioSpecs = struct
  include BaseSpecs

  let string_of_kind = function `Raw -> "ffmpeg.audio.raw"
  let kind_of_string = function "ffmpeg.audio.raw" -> Some `Raw | _ -> None

  type params = {
    channel_layout : Channel_layout.t option;
    sample_format : Sample_format.t option;
    sample_rate : int option;
  }

  type data = (params, audio frame) content

  let name = "ffmpeg.raw.audio"

  let frame_params { frame } =
    {
      channel_layout = Some (Audio.frame_get_channel_layout frame);
      sample_format = Some (Audio.frame_get_sample_format frame);
      sample_rate = Some (Audio.frame_get_sample_rate frame);
    }

  let mk_params p =
    {
      channel_layout = Some (Avcodec.Audio.get_channel_layout p);
      sample_format = Some (Avcodec.Audio.get_sample_format p);
      sample_rate = Some (Avcodec.Audio.get_sample_rate p);
    }

  let default_params _ =
    { channel_layout = None; sample_format = None; sample_rate = None }

  let string_of_params { channel_layout; sample_format; sample_rate } =
    Content.print_optional
      [
        ( "channel_layout",
          Option.map Channel_layout.get_description channel_layout );
        ( "sample_format",
          Option.map
            (fun p ->
              match Sample_format.get_name p with None -> "none" | Some p -> p)
            sample_format );
        ("sample_rate", Option.map string_of_int sample_rate);
      ]

  let parse_param label value =
    match label with
      | "channel_layout" ->
          Some
            {
              channel_layout = Some (Avutil.Channel_layout.find value);
              sample_format = None;
              sample_rate = None;
            }
      | "sample_format" ->
          Some
            {
              channel_layout = None;
              sample_format = Some (Avutil.Sample_format.find value);
              sample_rate = None;
            }
      | "sample_rate" ->
          Some
            {
              channel_layout = None;
              sample_format = None;
              sample_rate = Some (int_of_string value);
            }
      | _ -> None

  let compatible src dst =
    match src with
      | {
       channel_layout = Some src_channel_layout;
       sample_format = Some src_sample_format;
       sample_rate = Some src_sample_rate;
      } -> (
          try
            ignore
              (Ffmpeg_avfilter_utils.AFormat.init ~src_channel_layout
                 ~src_sample_format ~src_sample_rate
                 ~src_time_base:(Ffmpeg_utils.liq_main_ticks_time_base ())
                 ?dst_channel_layout:dst.channel_layout
                 ?dst_sample_format:dst.sample_format
                 ?dst_sample_rate:dst.sample_rate ());
            true
          with _ -> false)
      | _ ->
          let c ?(compare = fun x x' -> x = x') = function
            | None, _ | _, None -> true
            | Some p, Some p' -> compare p p'
          in
          c ~compare:Avutil.Channel_layout.compare
            (src.channel_layout, dst.channel_layout)
          && c (src.sample_format, dst.sample_format)
          && c (src.sample_rate, dst.sample_rate)

  let merge p p' =
    {
      channel_layout =
        Content.merge_param ~compare:Avutil.Channel_layout.compare
          ~name:"channel_layout"
          (p.channel_layout, p'.channel_layout);
      sample_format =
        Content.merge_param ~name:"sample_format"
          (p.sample_format, p'.sample_format);
      sample_rate =
        Content.merge_param ~name:"sample_rate" (p.sample_rate, p'.sample_rate);
    }
end

module Audio = struct
  include Content.MkContent (AudioSpecs)

  let kind = lift_kind `Raw
end

module VideoSpecs = struct
  include BaseSpecs

  let string_of_kind = function `Raw -> "ffmpeg.video.raw"
  let kind_of_string = function "ffmpeg.video.raw" -> Some `Raw | _ -> None

  type params = {
    width : int option;
    height : int option;
    pixel_format : Avutil.Pixel_format.t option;
    pixel_aspect : Avutil.rational option;
  }

  type data = (params, video frame) content

  let name = "ffmpeg.raw.video"

  let frame_params { frame } =
    {
      width = Some (Video.frame_get_width frame);
      height = Some (Video.frame_get_height frame);
      pixel_format = Some (Video.frame_get_pixel_format frame);
      pixel_aspect = Video.frame_get_pixel_aspect frame;
    }

  let mk_params p =
    {
      width = Some (Avcodec.Video.get_width p);
      height = Some (Avcodec.Video.get_height p);
      pixel_format = Avcodec.Video.get_pixel_format p;
      pixel_aspect = Avcodec.Video.get_pixel_aspect p;
    }

  let default_params _ =
    { width = None; height = None; pixel_format = None; pixel_aspect = None }

  let string_of_params { width; height; pixel_format; pixel_aspect } =
    Content.print_optional
      [
        ("width", Option.map string_of_int width);
        ("height", Option.map string_of_int height);
        ( "pixel_format",
          Option.map
            (fun p ->
              match Avutil.Pixel_format.to_string p with
                | None -> "none"
                | Some p -> p)
            pixel_format );
        ( "pixel_aspect",
          Option.map
            (fun { Avutil.num; den } -> Printf.sprintf "%d/%d" num den)
            pixel_aspect );
      ]

  let parse_param label value =
    match label with
      | "width" ->
          Some
            {
              width = Some (int_of_string value);
              height = None;
              pixel_format = None;
              pixel_aspect = None;
            }
      | "height" ->
          Some
            {
              width = None;
              height = Some (int_of_string value);
              pixel_format = None;
              pixel_aspect = None;
            }
      | "pixel_format" ->
          Some
            {
              width = None;
              height = None;
              pixel_format = Some (Avutil.Pixel_format.of_string value);
              pixel_aspect = None;
            }
      | "pixel_aspect" ->
          let pixel_aspect =
            try
              let rex = Re.Pcre.regexp "(\\d+)/(\\d+)" in
              let sub = Re.Pcre.exec ~rex value in
              Some
                {
                  Avutil.num = int_of_string (Re.Pcre.get_substring sub 1);
                  den = int_of_string (Re.Pcre.get_substring sub 2);
                }
            with _ -> None
          in
          Some
            { width = None; height = None; pixel_format = None; pixel_aspect }
      | _ -> None

  let compatible p p' =
    let c = function None, _ | _, None -> true | Some p, Some p' -> p = p' in
    c (p.width, p'.width)
    && c (p.height, p'.height)
    && c (p.pixel_format, p'.pixel_format)
    && c (p.pixel_aspect, p'.pixel_aspect)

  let merge p p' =
    {
      width = Content.merge_param ~name:"width" (p.width, p'.width);
      height = Content.merge_param ~name:"height" (p.height, p'.height);
      pixel_format =
        Content.merge_param ~name:"pixel_format"
          (p.pixel_format, p'.pixel_format);
      pixel_aspect =
        Content.merge_param ~name:"pixel_aspect"
          (p.pixel_aspect, p'.pixel_aspect);
    }
end

module Video = struct
  include Content.MkContent (VideoSpecs)

  let kind = lift_kind `Raw
end
