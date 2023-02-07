(*****************************************************************************

   Liquidsoap, a programmable audio stream generator.
   Copyright 2003-2023 Savonet team

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

open Mm

(** Decode media using ffmpeg. *)

let log = Log.make ["decoder"; "ffmpeg"; "internal"]

module ConverterInput = Swresample.Make (Swresample.Frame)
module Converter = ConverterInput (Swresample.PlanarFloatArray)
module Scaler = Swscale.Make (Swscale.Frame) (Swscale.BigArray)

let mk_audio_decoder ~channels ~stream ~field codec =
  Ffmpeg_decoder_common.set_audio_stream_decoder stream;
  let in_sample_rate = ref (Avcodec.Audio.get_sample_rate codec) in
  let in_channel_layout = ref (Avcodec.Audio.get_channel_layout codec) in
  let in_sample_format = ref (Avcodec.Audio.get_sample_format codec) in
  let target_sample_rate = Multicore.force Frame.audio_rate in
  let target_channel_layout = Avutil.Channel_layout.get_default channels in
  let mk_converter () =
    Converter.create !in_channel_layout ~in_sample_format:!in_sample_format
      !in_sample_rate target_channel_layout target_sample_rate
  in
  let converter = ref (mk_converter ()) in
  fun ~buffer frame ->
    let frame_in_sample_rate = Avutil.Audio.frame_get_sample_rate frame in
    let frame_in_channel_layout =
      Avutil.Channel_layout.get_default (Avutil.Audio.frame_get_channels frame)
    in
    let frame_in_sample_format = Avutil.Audio.frame_get_sample_format frame in
    if
      !in_sample_rate <> frame_in_sample_rate
      || !in_channel_layout <> frame_in_channel_layout
      || !in_sample_format <> frame_in_sample_format
    then (
      log#important "Frame format change detected!";
      in_sample_rate := frame_in_sample_rate;
      in_channel_layout := frame_in_channel_layout;
      in_sample_format := frame_in_sample_format;
      converter := mk_converter ());
    let content = Converter.convert !converter frame in
    buffer.Decoder.put_pcm ~field ~samplerate:target_sample_rate content;
    let metadata = Avutil.Frame.metadata frame in
    if metadata <> [] then (
      let m = Hashtbl.create (List.length metadata) in
      List.iter (fun (k, v) -> Hashtbl.add m k v) metadata;
      Generator.add_metadata buffer.Decoder.generator m)

let mk_video_decoder ~width ~height ~stream ~field codec =
  Ffmpeg_decoder_common.set_video_stream_decoder stream;
  let pixel_format =
    match Avcodec.Video.get_pixel_format codec with
      | None -> failwith "Pixel format unknown!"
      | Some f -> f
  in
  let target_width = width in
  let target_height = height in
  let width = Avcodec.Video.get_width codec in
  let height = Avcodec.Video.get_height codec in
  let target_fps = Multicore.force Frame.video_rate in
  let scale =
    let scale_proportional (sw, sh) (tw, th) =
      if th * sw < tw * sh then (sw * th / sh, th) else (tw, sh * tw / sw)
    in
    (* Actual proportional width an height. *)
    let aw, ah =
      scale_proportional (width, height) (target_width, target_height)
    in
    let scaler =
      Scaler.create [] width height pixel_format aw ah
        (Ffmpeg_utils.liq_frame_pixel_format ())
    in
    fun frame : Video.Canvas.Image.t ->
      let img =
        Scaler.convert scaler frame
        |> Ffmpeg_utils.unpack_image ~width:aw ~height:ah
      in
      let x = (target_width - aw) / 2 in
      let y = (target_height - ah) / 2 in
      Video.Canvas.Image.make img
      |> Video.Canvas.Image.translate x y
      |> Video.Canvas.Image.viewport target_width target_height
  in
  let time_base = Av.get_time_base stream in
  let pixel_aspect = Av.get_pixel_aspect stream in
  let cb ~buffer frame =
    let img = scale frame in
    let content = Video.Canvas.single img in
    buffer.Decoder.put_yuva420p ~field
      ~fps:{ Decoder.num = target_fps; den = 1 }
      content;
    let metadata = Avutil.Frame.metadata frame in
    if metadata <> [] then (
      let m = Hashtbl.create (List.length metadata) in
      List.iter (fun (k, v) -> Hashtbl.add m k v) metadata;
      Generator.add_metadata buffer.Decoder.generator m)
  in
  let converter =
    Ffmpeg_avfilter_utils.Fps.init ~width ~height ~pixel_format ~time_base
      ?pixel_aspect ~target_fps ()
  in
  fun ~buffer frame ->
    Ffmpeg_avfilter_utils.Fps.convert converter frame (cb ~buffer)
