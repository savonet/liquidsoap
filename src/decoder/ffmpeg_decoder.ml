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
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 *****************************************************************************)

(** Decode and read metadata using ffmpeg. *)

open Dtools

let log = Log.make ["decoder";"ffmpeg"]

(** Configuration keys for ffmpeg. *)
let mime_types =
  Conf.list ~p:(Decoder.conf_mime_types#plug "ffmpeg")
    "Mime-types used for decoding with ffmpeg"
    ~d:[]

let file_extensions =
  Conf.list ~p:(Decoder.conf_file_extensions#plug "ffmpeg")
    "File extensions used for decoding with ffmpeg"
    ~d:["mp3";"mp4";"m4a";"wav";"flac";"ogg"] (* Test *)

module ConverterInput = FFmpeg.Swresample.Make(FFmpeg.Swresample.Frame)
module Converter = ConverterInput(FFmpeg.Swresample.PlanarFloatArray)

module G = Generator.From_audio_video
module Buffered = Decoder.Buffered(G)

let duration file =
  let container =
    FFmpeg.Av.open_input file
  in
  Utils.ensure (fun () -> FFmpeg.Av.close container)
    (fun () ->
      let (_, stream, _) =
        FFmpeg.Av.find_best_audio_stream container
      in
      let duration =
        FFmpeg.Av.get_duration stream ~format:`Millisecond
      in
      (Int64.to_float duration) /. 1000.)

 exception End_of_file

let create_decoder fname =
  let remaining = ref
    (Frame.master_of_seconds (duration fname))
  in
  let container =
    FFmpeg.Av.open_input fname
  in
  (* Only audio for now *)
  let (_, stream, codec) =
    FFmpeg.Av.find_best_audio_stream container
  in
  let sample_freq =
    FFmpeg.Avcodec.Audio.get_sample_rate codec
  in
  let in_sample_format =
    FFmpeg.Avcodec.Audio.get_sample_format codec
  in
  let channel_layout =
    FFmpeg.Avcodec.Audio.get_channel_layout codec
  in
  let target_sample_rate =
    Lazy.force Frame.audio_rate
  in
  let converter =
    Converter.create channel_layout ~in_sample_format sample_freq
                     channel_layout target_sample_rate
  in 
  let decr_remaining, get_remaining =
    let m = Mutex.create () in
    let decr_remaining = Tutils.mutexify m (fun v ->
      remaining := !remaining - v)
    in
    let get_remaining = Tutils.mutexify m (fun () ->
      !remaining)
    in
    decr_remaining, get_remaining
  in
  let convert frame =
    let data = 
      Converter.convert converter frame
    in
    let consumed =
      Frame.master_of_audio (Array.length data.(0))
    in
    decr_remaining consumed;
    data
  in
  let seek ticks =
    let position = Frame.seconds_of_master ticks in
    let position = Int64.of_float
      (position *. 1000.)
    in
    try
      FFmpeg.Av.seek stream `Millisecond position [||];
      ticks
    with Failure _ -> 0
  in
  let decode gen =
    match FFmpeg.Av.read_frame stream with
      | `Frame frame ->
          let content = convert frame in
          G.set_mode gen `Audio ;
          G.put_audio gen content 0 (Array.length content.(0))
      | `End_of_file -> 
          G.add_break gen;
          raise End_of_file
  in
  let close () =
    FFmpeg.Av.close container
  in
  { Decoder.
     seek = seek;
     decode = decode }, close, get_remaining

let create_file_decoder filename kind =
  let generator = G.create `Audio in
  let decoder, close, remaining =
    create_decoder filename
  in
  let remaining frame offset =
    let remaining = remaining () in
    remaining + G.length generator + Frame.position frame - offset 
  in
  Buffered.make_file_decoder ~filename ~close ~kind ~remaining decoder generator 

(* Get the number of channels of audio in a file. *)
let get_type filename =
  let container =
    FFmpeg.Av.open_input filename
  in
  Utils.ensure (fun () -> FFmpeg.Av.close container)
    (fun () ->
      let (_, _, codec) =
        FFmpeg.Av.find_best_audio_stream container
      in
      let channels =
        FFmpeg.Avcodec.Audio.get_nb_channels codec
      in
      let rate =
        FFmpeg.Avcodec.Audio.get_sample_rate codec
      in
      log#f 4 "ffmpeg recognizes %S as: (%dHz,%d channels)."
        filename rate channels ;
      {Frame.
         audio = channels ;
         video = 0 ;
         midi  = 0 })

let () =
  Decoder.file_decoders#register
  "FFMPEG"
  ~sdoc:"Use libffmpeg to decode any file \
         if its MIME type or file extension is appropriate."
  (fun ~metadata:_ filename kind ->
     if not (Decoder.test_file ~mimes:mime_types#get 
                               ~extensions:file_extensions#get
                               ~log filename) then
       None
     else
       if kind.Frame.audio = Frame.Variable ||
          kind.Frame.audio = Frame.Succ Frame.Variable ||
          if Frame.type_has_kind (get_type filename) kind then true else begin
            log#f 3
              "File %S has an incompatible number of channels."
              filename ;
            false
          end
       then
         Some (fun () -> create_file_decoder filename kind)
       else
         None)

let log = Dtools.Log.make ["metadata";"ffmpeg"]

let get_tags file =
  let container =
    FFmpeg.Av.open_input file
  in
  Utils.ensure (fun () -> FFmpeg.Av.close container)
    (fun () ->
      FFmpeg.Av.get_input_metadata container)

let () = Request.mresolvers#register "FFMPEG" get_tags

let check filename =
  match Configure.file_mime with
    | Some f -> List.mem (f filename) mime_types#get
    | None -> (try ignore (get_type filename) ; true with _ -> false)

let () =
  Request.dresolvers#register "FFMPEG" duration
