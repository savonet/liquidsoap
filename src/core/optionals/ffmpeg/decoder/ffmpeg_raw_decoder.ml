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

(** Decode raw ffmpeg frames. *)

let mk_decoder ~stream_idx ~stream_time_base ~field ~lift_data params =
  let duration_converter =
    Ffmpeg_utils.Duration.init ~mode:`PTS ~src:stream_time_base
      ~convert_ts:false ~get_ts:Avutil.Frame.pts ~set_ts:Avutil.Frame.set_pts
      ~get_duration:Avutil.Frame.duration ()
  in
  fun ~buffer -> function
    | `Flush -> ()
    | `Frame frame -> (
        match Ffmpeg_utils.Duration.push duration_converter frame with
          | Some (length, data) ->
              let chunk =
                {
                  Ffmpeg_content_base.length;
                  stream_idx;
                  time_base = stream_time_base;
                  data;
                }
              in
              Generator.put buffer.Decoder.generator field
                (lift_data { Ffmpeg_content_base.params; chunks = [chunk] })
          | None -> ())

let mk_audio_decoder ~stream_idx ~format ~stream ~field src_params =
  let dst_params = Ffmpeg_raw_content.Audio.get_params format in
  let converter =
    let src_channel_layout = Avcodec.Audio.get_channel_layout src_params in
    let src_sample_format = Avcodec.Audio.get_sample_format src_params in
    let src_sample_rate = Avcodec.Audio.get_sample_rate src_params in
    Ffmpeg_avfilter_utils.AFormat.init ~src_channel_layout ~src_sample_format
      ~src_sample_rate ~src_time_base:(Av.get_time_base stream)
      ?dst_channel_layout:
        dst_params.Ffmpeg_raw_content.AudioSpecs.channel_layout
      ?dst_sample_format:dst_params.Ffmpeg_raw_content.AudioSpecs.sample_format
      ?dst_sample_rate:dst_params.Ffmpeg_raw_content.AudioSpecs.sample_rate ()
  in
  let stream_time_base = Ffmpeg_avfilter_utils.AFormat.time_base converter in
  (* No [Content.merge] here, unlike the video decoder below: the converter
     already produces frames in the format the target asked for. *)
  let decoder =
    mk_decoder ~stream_idx ~stream_time_base ~field
      ~lift_data:Ffmpeg_raw_content.Audio.lift_data dst_params
  in
  fun ~buffer -> function
    | `Flush ->
        Ffmpeg_avfilter_utils.AFormat.eof converter (fun frame ->
            decoder ~buffer (`Frame frame));
        decoder ~buffer `Flush
    | `Frame frame ->
        Ffmpeg_avfilter_utils.AFormat.convert converter frame (fun frame ->
            decoder ~buffer (`Frame frame))

let mk_video_decoder ~stream_idx ~format ~stream ~field params =
  let params = Ffmpeg_raw_content.VideoSpecs.mk_params params in
  (* Video frames are passed through as they come, so the target format has to
     take on the source's parameters. *)
  ignore (Content.merge format (Ffmpeg_raw_content.Video.lift_params params));
  mk_decoder ~stream_idx ~stream_time_base:(Av.get_time_base stream) ~field
    ~lift_data:Ffmpeg_raw_content.Video.lift_data params
