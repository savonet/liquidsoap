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

open Mm

(** Decode media using ffmpeg. *)

let log = Log.make ["decoder"; "ffmpeg"; "internal"]

module type Converter_type = sig
  type t

  module Content : sig
    type data

    val lift_data : ?offset:int -> ?length:int -> data -> Content_base.data
  end

  val create :
    ?options:Swresample.options list ->
    Avutil.Channel_layout.t ->
    ?in_sample_format:Avutil.Sample_format.t ->
    int ->
    Avutil.Channel_layout.t ->
    ?out_sample_format:Avutil.Sample_format.t ->
    int ->
    t

  val convert :
    ?offset:int -> ?length:int -> t -> Swresample.Frame.t -> Content.data
end

module ConverterInput = Swresample.Make (Swresample.Frame)

module Converter = struct
  module Content = Content_audio
  include ConverterInput (Swresample.PlanarFloatArray)
end

module Converter_pcm_s16 = struct
  module Content = Content_pcm_s16
  include ConverterInput (Swresample.S16PlanarBigArray)
end

module Converter_pcm_f32 = struct
  module Content = Content_pcm_f32
  include ConverterInput (Swresample.FltPlanarBigArray)
end

module Scaler = Swscale.Make (Swscale.Frame) (Swscale.BigArray)
module SubScaler = Swscale.Make (Swscale.PackedBigArray) (Swscale.BigArray)

let mk_audio_decoder ~channels ~stream ~field ~pcm_kind codec =
  let converter =
    match pcm_kind with
      | _ when Content_audio.is_kind pcm_kind ->
          (module Converter : Converter_type)
      | _ when Content_pcm_s16.is_kind pcm_kind ->
          (module Converter_pcm_s16 : Converter_type)
      | _ when Content_pcm_f32.is_kind pcm_kind ->
          (module Converter_pcm_f32 : Converter_type)
      | _ -> raise Content_base.Invalid
  in
  let module Converter = (val converter : Converter_type) in
  Ffmpeg_decoder_common.set_audio_stream_decoder stream;
  let in_sample_rate = ref (Avcodec.Audio.get_sample_rate codec) in
  let in_channel_layout = ref (Avcodec.Audio.get_channel_layout codec) in
  let in_sample_format = ref (Avcodec.Audio.get_sample_format codec) in
  let target_sample_rate = Lazy.force Frame.audio_rate in
  let target_channel_layout = Avutil.Channel_layout.get_default channels in
  let mk_converter () =
    Converter.create !in_channel_layout ~in_sample_format:!in_sample_format
      !in_sample_rate target_channel_layout target_sample_rate
  in
  let converter = ref (mk_converter ()) in
  fun ~buffer -> function
    | `Flush -> ()
    | `Frame frame ->
        let frame_in_sample_rate = Avutil.Audio.frame_get_sample_rate frame in
        let frame_in_channel_layout =
          Avutil.Channel_layout.get_default
            (Avutil.Audio.frame_get_channels frame)
        in
        let frame_in_sample_format =
          Avutil.Audio.frame_get_sample_format frame
        in
        if
          !in_sample_rate <> frame_in_sample_rate
          || (not
                (Avutil.Channel_layout.compare !in_channel_layout
                   frame_in_channel_layout))
          || !in_sample_format <> frame_in_sample_format
        then (
          log#important "Frame format change detected!";
          in_sample_rate := frame_in_sample_rate;
          in_channel_layout := frame_in_channel_layout;
          in_sample_format := frame_in_sample_format;
          converter := mk_converter ());
        let content = Converter.convert !converter frame in
        Generator.put buffer.Decoder.generator field
          (Converter.Content.lift_data content);
        let metadata = Avutil.Frame.metadata frame in
        if metadata <> [] then
          Generator.add_metadata buffer.Decoder.generator
            (Frame.Metadata.from_list metadata)

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
  let target_fps = Lazy.force Frame.video_rate in
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
        Ffmpeg_utils.liq_frame_pixel_format
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
    buffer.Decoder.put_yuva420p ~field
      ~fps:{ Decoder.num = target_fps; den = 1 }
      img;
    let metadata = Avutil.Frame.metadata frame in
    if metadata <> [] then
      Generator.add_metadata buffer.Decoder.generator
        (Frame.Metadata.from_list metadata)
  in
  let converter =
    Ffmpeg_avfilter_utils.Fps.init ~width ~height ~pixel_format ~time_base
      ?pixel_aspect ~target_fps ()
  in
  fun ~buffer -> function
    | `Frame frame ->
        Ffmpeg_avfilter_utils.Fps.convert converter frame (cb ~buffer)
    | `Flush -> Ffmpeg_avfilter_utils.Fps.eof converter (cb ~buffer)

let main_of_subtitle_time time = Frame.main_of_seconds (float time /. 1000.)

let mk_text_subtitle_decoder ~stream ~field =
  Ffmpeg_decoder_common.set_subtitle_stream_decoder stream;
  let avutil_time_base = Avutil.time_base () in
  let liq_main_ticks_time_base = Ffmpeg_utils.liq_main_ticks_time_base () in
  let to_ticks ts =
    Int64.to_int
      (Ffmpeg_utils.convert_time_base ~src:avutil_time_base
         ~dst:liq_main_ticks_time_base ts)
  in
  let output ?(data = []) ~buffer ~length () =
    let data = Subtitle_content.lift_data ~length data in
    Generator.put buffer.Decoder.generator field data
  in
  let process frame =
    let content = Avutil.Subtitle.get_content frame in
    let position = to_ticks (Option.value ~default:0L content.pts) in
    let subtitles =
      List.filter_map
        (fun (rect : Avutil.Subtitle.rectangle) ->
          match rect.rect_type with
            | (`Ass | `Text) as format ->
                Some
                  ( 0,
                    {
                      Subtitle_content.start_time =
                        main_of_subtitle_time content.start_display_time;
                      end_time = main_of_subtitle_time content.end_display_time;
                      text =
                        (match format with
                          | `Ass -> rect.ass
                          | `Text -> rect.text);
                      format;
                      forced = List.mem `Forced rect.flags;
                    } )
            | _ -> None)
        content.rectangles
    in
    let duration =
      List.fold_left
        (fun duration (_, { Subtitle_content.end_time }) ->
          max end_time duration)
        0 subtitles
    in
    (position, duration, subtitles)
  in
  Ffmpeg_utils.mk_subtitle_decoder ~output ~process ()

let mk_bitmap_subtitle_decoder ~stream ~field ~width ~height =
  Ffmpeg_decoder_common.set_subtitle_stream_decoder stream;
  let cached_scaler = ref None in
  let get_scaler w h =
    match !cached_scaler with
      | Some (w', h', scaler) when w = w' && h = h' -> scaler
      | _ ->
          let scaler =
            SubScaler.create [] w h `Pal8 w h
              Ffmpeg_utils.liq_frame_pixel_format_with_alpha
          in
          cached_scaler := Some (w, h, scaler);
          scaler
  in
  let liq_main_ticks_time_base = Ffmpeg_utils.liq_main_ticks_time_base () in
  let avutil_time_base = Avutil.time_base () in
  let to_ticks ts =
    Int64.to_int
      (Ffmpeg_utils.convert_time_base ~src:avutil_time_base
         ~dst:liq_main_ticks_time_base ts)
  in
  let convert (x, y, w, h, sub) =
    let scaler = get_scaler w h in
    let img =
      SubScaler.convert scaler sub
      |> Ffmpeg_utils.unpack_image ~width:w ~height:h
    in
    Video.Canvas.Image.make ~width ~height ~x ~y img
  in
  let active_canvas = ref [] in
  let generator =
    Content.Video.make_generator
      { Content.Video.width = Some (lazy width); height = Some (lazy height) }
  in
  let pos = ref 0 in
  let generate =
    Content.Video.generate
      ~create:(fun ~pos:p ~width ~height () ->
        let pending_canvas, display_canvas =
          let pos = !pos + p in
          List.fold_left
            (fun (pending_canvas, display_canvas) (start, stop, canvas) ->
              let display_canvas =
                if start <= pos && pos < stop then
                  Video.Canvas.Image.add canvas display_canvas
                else display_canvas
              in
              let pending_canvas =
                if stop <= pos then pending_canvas
                else (start, stop, canvas) :: pending_canvas
              in
              (pending_canvas, display_canvas))
            ([], Video.Canvas.Image.create width height)
            !active_canvas
        in
        active_canvas := pending_canvas;
        display_canvas)
      generator
  in
  let process frame =
    let {
      Avutil.Subtitle.start_display_time;
      end_display_time;
      rectangles;
      pts;
    } =
      Avutil.Subtitle.get_content frame
    in
    let pts = to_ticks (Option.value ~default:0L pts) in
    let start = main_of_subtitle_time start_display_time in
    let stop = main_of_subtitle_time end_display_time in
    let canvas =
      List.fold_left
        (fun canvas (rect : Avutil.Subtitle.rectangle) ->
          match rect with
            | { rect_type = `Bitmap; pict = Some { x; y; w; h; planes } } ->
                Video.Canvas.Image.add (convert (x, y, w, h, planes)) canvas
            | _ -> canvas)
        (Video.Canvas.Image.create width height)
        rectangles
    in
    active_canvas := (pts + start, pts + stop, canvas) :: !active_canvas
  in
  let advance ~buffer position =
    let length = position - !pos in
    let data = Content.Video.lift_data (generate length) in
    pos := position;
    Generator.put buffer.Decoder.generator field data
  in
  let decoder ~buffer = function
    | `Subtitle frame -> process frame
    | `Flush ->
        let last_pos =
          List.fold_left
            (fun last_pos (_, stop, _) -> max last_pos stop)
            !pos !active_canvas
        in
        advance ~buffer last_pos
  in
  { Ffmpeg_decoder_common.decoder; advance }
