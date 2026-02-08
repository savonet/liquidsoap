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

let mk_decoder ~stream_idx ~stream_time_base ~mk_params ~mk_content ~lift_data
    ~put_data params =
  let duration_converter =
    Ffmpeg_utils.Duration.init ~mode:`PTS ~src:stream_time_base
      ~convert_ts:false ~get_ts:Avutil.Frame.pts ~set_ts:Avutil.Frame.set_pts
      ~get_duration:Avutil.Frame.duration ()
  in
  fun ~buffer -> function
    | `Flush -> ()
    | `Frame frame -> (
        match Ffmpeg_utils.Duration.push duration_converter frame with
          | Some (length, frames) ->
              let data = List.map (fun (pos, frame) -> (pos, frame)) frames in
              let content =
                mk_content ~length ~stream_idx ~time_base:stream_time_base
                  ~params:(mk_params params) ~data
              in
              put_data buffer.Decoder.generator (lift_data content)
          | None -> ())

let mk_audio_decoder ~stream_idx ~format ~stream ~field src_params =
  Ffmpeg_decoder_common.set_audio_stream_decoder stream;
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
  let lift_data data = Ffmpeg_raw_content.Audio.lift_data data in
  let mk_params f = f in
  let mk_content ~length ~stream_idx ~time_base ~params ~data :
      Ffmpeg_raw_content.AudioSpecs.data =
    let d : Avutil.audio Avutil.frame Ffmpeg_content_base.data =
      { length; stream_idx; time_base; data }
    in
    { params; chunks = [d] }
  in
  let decoder =
    mk_decoder ~stream_idx ~lift_data ~mk_params ~mk_content ~stream_time_base
      ~put_data:(fun g c -> Generator.put g field c)
      dst_params
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
  Ffmpeg_decoder_common.set_video_stream_decoder stream;
  ignore
    (Content.merge format
       Ffmpeg_raw_content.(Video.lift_params (VideoSpecs.mk_params params)));
  let stream_time_base = Av.get_time_base stream in
  let lift_data data = Ffmpeg_raw_content.Video.lift_data data in
  let mk_params = Ffmpeg_raw_content.VideoSpecs.mk_params in
  let mk_content ~length ~stream_idx ~time_base ~params ~data :
      Ffmpeg_raw_content.VideoSpecs.data =
    let d : Avutil.video Avutil.frame Ffmpeg_content_base.data =
      { length; stream_idx; time_base; data }
    in
    { params; chunks = [d] }
  in
  mk_decoder ~stream_idx ~mk_params ~mk_content ~lift_data ~stream_time_base
    ~put_data:(fun g c -> Generator.put g field c)
    params
