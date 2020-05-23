(*****************************************************************************

   Liquidsoap, a programmable audio stream generator.
   Copyright 2003-2017 Savonet team

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

(** Decode and read metadata using ffmpeg. *)

exception End_of_file
exception No_stream

module Generator = Decoder.G

let log = Log.make ["decoder"; "ffmpeg"]

(** Configuration keys for ffmpeg. *)
let mime_types =
  Dtools.Conf.list
    ~p:(Decoder.conf_mime_types#plug "ffmpeg")
    "Mime-types used for decoding with ffmpeg"
    ~d:
      [
        "audio/vnd.wave";
        "audio/wav";
        "audio/wave";
        "audio/x-wav";
        "audio/aac";
        "audio/aacp";
        "audio/x-hx-aac-adts";
        "audio/flac";
        "audio/x-flac";
        "audio/mpeg";
        "audio/MPA";
        "video/x-ms-asf";
        "video/x-msvideo";
        "audio/mp4";
        "application/mp4";
        "video/mp4";
        "video/3gpp";
        "video/webm";
        "video/x-matroska";
        "video/mp2t";
        "video/MP2T";
        "application/ogg";
        "application/x-ogg";
        "audio/x-ogg";
        "audio/ogg";
        "video/ogg";
        "application/ffmpeg";
      ]

let file_extensions =
  Dtools.Conf.list
    ~p:(Decoder.conf_file_extensions#plug "ffmpeg")
    "File extensions used for decoding with ffmpeg"
    ~d:
      [
        "mp1";
        "mp2";
        "mp3";
        "m4a";
        "m4b";
        "m4p";
        "m4v";
        "m4r";
        "3gp";
        "mp4";
        "wav";
        "flac";
        "ogv";
        "oga";
        "ogx";
        "ogg";
        "opus";
        "wma";
        "webm";
        "wmv";
        "avi";
        "mkv";
        "aac";
        "osb";
      ]

let priority =
  Dtools.Conf.int
    ~p:(Decoder.conf_priorities#plug "ffmpeg")
    "Priority for the ffmpeg decoder" ~d:10

module ConverterInput = Swresample.Make (Swresample.Frame)
module Converter = ConverterInput (Swresample.FltPlanarBigArray)
module Scaler = Swscale.Make (Swscale.Frame) (Swscale.BigArray)

let mk_audio_decoder container =
  let idx, stream, codec = Av.find_best_audio_stream container in
  let sample_freq = Avcodec.Audio.get_sample_rate codec in
  let channel_layout = Avcodec.Audio.get_channel_layout codec in
  let target_sample_rate = Lazy.force Frame.audio_rate in
  let in_sample_format = ref (Avcodec.Audio.get_sample_format codec) in
  let mk_converter () =
    Converter.create channel_layout ~in_sample_format:!in_sample_format
      sample_freq channel_layout target_sample_rate
  in
  let converter = ref (mk_converter ()) in
  let decoder_time_base = { Avutil.num = 1; den = target_sample_rate } in
  let internal_time_base = Ffmpeg_utils.liq_internal_audio_time_base () in
  let decoder_pts = ref 0L in
  ( idx,
    stream,
    fun ~buffer frame ->
      let frame_in_sample_format = Avutil.Audio.frame_get_sample_format frame in
      if !in_sample_format <> frame_in_sample_format then (
        log#important "Sample format change detected!";
        in_sample_format := frame_in_sample_format;
        converter := mk_converter () );
      let content = Converter.convert !converter frame in
      let l = Audio.length content in
      let pts =
        Ffmpeg_utils.convert_pts ~src:decoder_time_base ~dst:internal_time_base
          !decoder_pts
      in
      decoder_pts := Int64.add !decoder_pts (Int64.of_int l);
      buffer.Decoder.put_audio ?pts:(Some pts) ~samplerate:target_sample_rate
        content )

let mk_video_decoder container =
  let idx, stream, codec = Av.find_best_video_stream container in
  let pixel_format = Avcodec.Video.get_pixel_format codec in
  let width = Avcodec.Video.get_width codec in
  let height = Avcodec.Video.get_height codec in
  let target_fps = Lazy.force Frame.video_rate in
  let target_width = Lazy.force Frame.video_width in
  let target_height = Lazy.force Frame.video_height in
  let scaler =
    Scaler.create [] width height pixel_format target_width target_height
      `Yuv420p
  in
  let time_base = Av.get_time_base stream in
  let pixel_aspect = Av.get_pixel_aspect stream in
  let decoder_time_base = { Avutil.num = 1; den = target_fps } in
  let internal_time_base = Ffmpeg_utils.liq_internal_video_time_base () in
  let decoder_pts = ref 0L in
  let cb ~buffer frame =
    let img =
      match Scaler.convert scaler frame with
        | [| (y, sy); (u, s); (v, _) |] ->
            Image.YUV420.make target_width target_height y sy u v s
        | _ -> assert false
    in
    let content = Video.single img in
    let pts =
      Ffmpeg_utils.convert_pts ~src:decoder_time_base ~dst:internal_time_base
        !decoder_pts
    in
    decoder_pts := Int64.succ !decoder_pts;
    buffer.Decoder.put_video ?pts:(Some pts)
      ~fps:{ Decoder.num = target_fps; den = 1 }
      [| content |]
  in
  let converter =
    Ffmpeg_utils.Fps.init ~width ~height ~pixel_format ~time_base ~pixel_aspect
      ~target_fps ()
  in
  ( idx,
    stream,
    fun ~buffer frame -> Ffmpeg_utils.Fps.convert converter frame (cb ~buffer)
  )

let mk_decoder ~audio ~video ~container =
  let rec read_input_frame () =
    try Av.read_input_frame container
    with Avutil.Error `Invalid_data -> read_input_frame ()
  in
  fun buffer ->
    match (audio, video, read_input_frame ()) with
      | Some (idx, _, decoder), _, `Audio (i, frame) when i = idx ->
          decoder ~buffer frame
      | _, Some (idx, _, decoder), `Video (i, frame) when i = idx ->
          decoder ~buffer frame
      | _ -> ()
      | exception Avutil.Error `Eof ->
          Generator.add_break ?sync:(Some true) buffer.Decoder.generator;
          raise End_of_file

let seek ~audio ~video ticks =
  let position = Frame.seconds_of_master ticks in
  let position = Int64.of_float (position *. 1000.) in
  let seek stream =
    try
      Av.seek stream `Millisecond position [||];
      ticks
    with Avutil.Error _ -> 0
  in
  match (audio, video) with
    | Some (_, s, _), _ -> seek s
    | _, Some (_, s, _) -> seek s
    | _ -> raise No_stream

let duration file =
  let container = Av.open_input file in
  Tutils.finalize
    ~k:(fun () -> Av.close container)
    (fun () ->
      let duration = Av.get_input_duration container ~format:`Millisecond in
      Int64.to_float duration /. 1000.)

let create_decoder fname =
  let duration = duration fname in
  let remaining = ref duration in
  let m = Mutex.create () in
  let set_remaining stream frame =
    Tutils.mutexify m
      (fun () ->
        match Avutil.frame_pts frame with
          | None -> ()
          | Some pts ->
              let { Avutil.num; den } = Av.get_time_base stream in
              let position =
                Int64.to_float (Int64.mul (Int64.of_int num) pts) /. float den
              in
              remaining := duration -. position)
      ()
  in
  let get_remaining =
    Tutils.mutexify m (fun () -> Frame.master_of_seconds !remaining)
  in
  let container = Av.open_input fname in
  let audio =
    try
      let idx, stream, decoder = mk_audio_decoder container in
      Some
        ( idx,
          stream,
          fun ~buffer frame ->
            set_remaining stream frame;
            decoder ~buffer frame )
    with Avutil.Error _ -> None
  in
  let video =
    try
      let idx, stream, decoder = mk_video_decoder container in
      Some
        ( idx,
          stream,
          fun ~buffer frame ->
            set_remaining stream frame;
            decoder ~buffer frame )
    with Avutil.Error _ -> None
  in
  let close () = Av.close container in
  ( {
      Decoder.seek = seek ~audio ~video;
      decode = mk_decoder ~audio ~video ~container;
    },
    close,
    get_remaining )

let create_file_decoder ~metadata:_ ~ctype filename =
  let decoder, close, remaining = create_decoder filename in
  Decoder.file_decoder ~filename ~close ~remaining ~ctype decoder

(* Get the type of an input container. *)
let get_type ~url container =
  let audio, descr =
    try
      let _, _, codec = Av.find_best_audio_stream container in
      let channels = Avcodec.Audio.get_nb_channels codec in
      let rate = Avcodec.Audio.get_sample_rate codec in
      let codec_name =
        Avcodec.Audio.string_of_id (Avcodec.Audio.get_params_id codec)
      in
      ( channels,
        [
          Printf.sprintf "audio: {codec: %s, %dHz, %d channel(s)}" codec_name
            rate channels;
        ] )
    with Avutil.Error _ -> (0, [])
  in
  let video, descr =
    try
      let _, _, codec = Av.find_best_video_stream container in
      let width = Avcodec.Video.get_width codec in
      let height = Avcodec.Video.get_height codec in
      let pixel_format =
        Avutil.Pixel_format.to_string (Avcodec.Video.get_pixel_format codec)
      in
      let codec_name =
        Avcodec.Video.string_of_id (Avcodec.Video.get_params_id codec)
      in
      ( 1,
        Printf.sprintf "video: {codec: %s, %dx%d, %s}" codec_name width height
          pixel_format
        :: descr )
    with Avutil.Error _ -> (0, descr)
  in
  if audio == 0 && video == 0 then failwith "No valid stream found in file.";
  log#info "ffmpeg recognizes %S as: %s." url
    (String.concat ", " (List.rev descr));
  { Frame.audio; video; midi = 0 }

let get_file_type filename =
  let container = Av.open_input filename in
  Tutils.finalize
    ~k:(fun () -> Av.close container)
    (fun () -> get_type ~url:filename container)

let create_stream_decoder _ input =
  let read = input.Decoder.read in
  let seek_input =
    match input.Decoder.lseek with
      | None -> None
      | Some fn -> Some (fun len _ -> fn len)
  in
  let container = Av.open_input_stream ?seek:seek_input read in
  let audio =
    try Some (mk_audio_decoder container) with Avutil.Error _ -> None
  in
  let video =
    try Some (mk_video_decoder container) with Avutil.Error _ -> None
  in
  {
    Decoder.seek = seek ~audio ~video;
    decode = mk_decoder ~audio ~video ~container;
  }

let () =
  Decoder.decoders#register "FFMPEG"
    ~sdoc:
      "Use libffmpeg to decode any file or stream if its MIME type or file \
       extension is appropriate."
    {
      Decoder.media_type = `Audio_video;
      priority = (fun () -> priority#get);
      file_extensions = (fun () -> Some file_extensions#get);
      mime_types = (fun () -> Some mime_types#get);
      file_type = (fun filename -> Some (get_file_type filename));
      file_decoder = Some create_file_decoder;
      stream_decoder = Some create_stream_decoder;
    }

let log = Log.make ["metadata"; "ffmpeg"]

let get_tags file =
  let container = Av.open_input file in
  Tutils.finalize
    ~k:(fun () -> Av.close container)
    (fun () -> Av.get_input_metadata container)

let () = Request.mresolvers#register "FFMPEG" get_tags

let check filename =
  match Configure.file_mime with
    | Some f -> List.mem (f filename) mime_types#get
    | None -> (
        try
          ignore (get_file_type filename);
          true
        with _ -> false )

let () = Request.dresolvers#register "FFMPEG" duration
