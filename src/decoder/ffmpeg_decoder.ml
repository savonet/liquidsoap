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

let log = Log.make ["decoder"; "ffmpeg"]

(** Configuration keys for ffmpeg. *)
let mime_types =
  Dtools.Conf.list
    ~p:(Decoder.conf_mime_types#plug "ffmpeg")
    "Mime-types used for decoding with ffmpeg" ~d:["application/ffmpeg"]

let file_extensions =
  Dtools.Conf.list
    ~p:(Decoder.conf_file_extensions#plug "ffmpeg")
    "File extensions used for decoding with ffmpeg"
    ~d:["mp3"; "mp4"; "m4a"; "wav"; "flac"; "ogg"; "wma"; "webm"; "osb"]

module ConverterInput = FFmpeg.Swresample.Make (FFmpeg.Swresample.Frame)
module Converter = ConverterInput (FFmpeg.Swresample.FltPlanarBigArray)
module G = Generator.From_audio_video
module Buffered = Decoder.Buffered (G)

let duration file =
  let container = FFmpeg.Av.open_input file in
  Tutils.finalize
    ~k:(fun () -> FFmpeg.Av.close container)
    (fun () ->
      let _, stream, _ = FFmpeg.Av.find_best_audio_stream container in
      let duration = FFmpeg.Av.get_duration stream ~format:`Millisecond in
      Int64.to_float duration /. 1000.)

exception End_of_file

let create_decoder fname =
  let remaining = ref (Frame.master_of_seconds (duration fname)) in
  let container = FFmpeg.Av.open_input fname in
  (* Only audio for now *)
  let _, stream, codec = FFmpeg.Av.find_best_audio_stream container in
  let sample_freq = FFmpeg.Avcodec.Audio.get_sample_rate codec in
  let channel_layout = FFmpeg.Avcodec.Audio.get_channel_layout codec in
  let target_sample_rate = Lazy.force Frame.audio_rate in
  let decr_remaining, get_remaining =
    let m = Mutex.create () in
    let decr_remaining =
      Tutils.mutexify m (fun v -> remaining := !remaining - v)
    in
    let get_remaining = Tutils.mutexify m (fun () -> !remaining) in
    (decr_remaining, get_remaining)
  in
  let in_sample_format = ref (FFmpeg.Avcodec.Audio.get_sample_format codec) in
  let mk_converter () =
    Converter.create channel_layout ~in_sample_format:!in_sample_format
      sample_freq channel_layout target_sample_rate
  in
  let converter = ref (mk_converter ()) in
  let convert frame =
    let frame_in_sample_format =
      FFmpeg.Avutil.Audio.frame_get_sample_format frame
    in
    if !in_sample_format <> frame_in_sample_format then (
      log#important "Sample format change detected!" ;
      in_sample_format := frame_in_sample_format ;
      converter := mk_converter () ) ;
    let data = Converter.convert !converter frame in
    let consumed = Frame.master_of_audio (Audio.length data) in
    decr_remaining consumed ; data
  in
  let seek ticks =
    let position = Frame.seconds_of_master ticks in
    let position = Int64.of_float (position *. 1000.) in
    try
      FFmpeg.Av.seek stream `Millisecond position [||] ;
      ticks
    with FFmpeg.Avutil.Error _ -> 0
  in
  let rec read_frame () =
    try FFmpeg.Av.read_frame stream
    with FFmpeg.Avutil.Error `Invalid_data -> read_frame ()
  in
  let decode gen =
    try
      let frame = read_frame () in
      let content = convert frame in
      G.set_mode gen `Audio ;
      G.put_audio gen content 0 (Audio.length content)
    with FFmpeg.Avutil.Error `Eof -> G.add_break gen ; raise End_of_file
  in
  let close () = FFmpeg.Av.close container in
  ({Decoder.seek; decode}, close, get_remaining)

let create_file_decoder filename kind =
  let generator = G.create `Audio in
  let decoder, close, remaining = create_decoder filename in
  let remaining frame offset =
    let remaining = remaining () in
    remaining + G.length generator + Frame.position frame - offset
  in
  Buffered.make_file_decoder ~filename ~close ~kind ~remaining decoder
    generator

(* Get the number of channels of audio in a file. *)
let get_type filename =
  let container = FFmpeg.Av.open_input filename in
  Tutils.finalize
    ~k:(fun () -> FFmpeg.Av.close container)
    (fun () ->
      let _, _, codec = FFmpeg.Av.find_best_audio_stream container in
      let channels = FFmpeg.Avcodec.Audio.get_nb_channels codec in
      let rate = FFmpeg.Avcodec.Audio.get_sample_rate codec in
      log#info "ffmpeg recognizes %S as: (%dHz,%d channels)." filename rate
        channels ;
      {Frame.audio= channels; video= 0; midi= 0})

let () =
  Decoder.file_decoders#register "FFMPEG"
    ~sdoc:
      "Use libffmpeg to decode any file if its MIME type or file extension is \
       appropriate." (fun ~metadata:_ filename kind ->
      if
        not
          (Decoder.test_file ~mimes:mime_types#get
             ~extensions:file_extensions#get ~log filename)
      then None
      else if
        kind.Frame.audio = Frame.Variable
        || kind.Frame.audio = Frame.Succ Frame.Variable
        ||
        if Frame.type_has_kind (get_type filename) kind then true
        else (
          log#important "File %S has an incompatible number of channels."
            filename ;
          false )
      then Some (fun () -> create_file_decoder filename kind)
      else None)

let log = Log.make ["metadata"; "ffmpeg"]

let get_tags file =
  let container = FFmpeg.Av.open_input file in
  Tutils.finalize
    ~k:(fun () -> FFmpeg.Av.close container)
    (fun () -> FFmpeg.Av.get_input_metadata container)

let () = Request.mresolvers#register "FFMPEG" get_tags

let check filename =
  match Configure.file_mime with
    | Some f ->
        List.mem (f filename) mime_types#get
    | None -> (
      try
        ignore (get_type filename) ;
        true
      with _ -> false )

let () = Request.dresolvers#register "FFMPEG" duration

module Make (Generator : Generator.S_Asio) = struct
  let create_decoder input =
    let read = input.Decoder.read in
    let seek =
      match input.Decoder.lseek with
        | None ->
            None
        | Some fn ->
            Some (fun len _ -> fn len)
    in
    let container = FFmpeg.Av.open_input_stream ?seek read in
    (* Only audio for now *)
    let _, stream, codec = FFmpeg.Av.find_best_audio_stream container in
    let sample_freq = FFmpeg.Avcodec.Audio.get_sample_rate codec in
    let channel_layout = FFmpeg.Avcodec.Audio.get_channel_layout codec in
    let target_sample_rate = Lazy.force Frame.audio_rate in
    let seek ticks =
      let position = Frame.seconds_of_master ticks in
      let position = Int64.of_float (position *. 1000.) in
      try
        FFmpeg.Av.seek stream `Millisecond position [||] ;
        ticks
      with FFmpeg.Avutil.Error _ -> 0
    in
    let rec read_frame () =
      try FFmpeg.Av.read_frame stream
      with FFmpeg.Avutil.Error `Invalid_data -> read_frame ()
    in
    let in_sample_format =
      ref (FFmpeg.Avcodec.Audio.get_sample_format codec)
    in
    let mk_converter () =
      Converter.create channel_layout ~in_sample_format:!in_sample_format
        sample_freq channel_layout target_sample_rate
    in
    let converter = ref (mk_converter ()) in
    let decode gen =
      try
        let frame = read_frame () in
        let frame_in_sample_format =
          FFmpeg.Avutil.Audio.frame_get_sample_format frame
        in
        if !in_sample_format <> frame_in_sample_format then (
          log#important "Sample format change detected!" ;
          in_sample_format := frame_in_sample_format ;
          converter := mk_converter () ) ;
        let content = Converter.convert !converter frame in
        Generator.set_mode gen `Audio ;
        Generator.put_audio gen content 0 (Audio.length content)
      with FFmpeg.Avutil.Error `Eof ->
        Generator.add_break gen ; raise End_of_file
    in
    {Decoder.seek; decode}
end

module D_stream = Make (Generator.From_audio_video_plus)

let () =
  Decoder.stream_decoders#register "FFMPEG"
    ~sdoc:
      "Use ffmpeg/libav to decode any stream with an appropriate MIME type."
    (fun mime kind ->
      let ( <: ) a b = Frame.mul_sub_mul a b in
      if
        List.mem mime mime_types#get
        (* Check that it is okay to have zero video and midi,
         * and at least one audio channel. *)
        && Frame.Zero <: kind.Frame.video
        && Frame.Zero <: kind.Frame.midi
        && kind.Frame.audio <> Frame.Zero
      then
        (* In fact we can't be sure that we'll satisfy the content
         * kind, because the stream might be mono or stereo.
         * For now, we let this problem result in an error at
         * decoding-time. Failing early would only be an advantage
         * if there was possibly another plugin for decoding
         * correctly the stream (e.g. by performing conversions). *)
        Some D_stream.create_decoder
      else None)
