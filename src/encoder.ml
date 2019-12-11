(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2019 Savonet team

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

(** Common infrastructure for encoding streams *)

type format =
  | WAV of Wav_format.t
  | AVI of Avi_format.t
  | Ogg of Ogg_format.t
  | MP3 of Mp3_format.t
  | Shine of Shine_format.t
  | Flac of Flac_format.t
  | Ffmpeg of Ffmpeg_format.t
  | FdkAacEnc of Fdkaac_format.t
  | External of External_encoder_format.t
  | GStreamer of Gstreamer_format.t

let kind_of_format = function
  | WAV w ->
      {Frame.audio= w.Wav_format.channels; Frame.video= 0; Frame.midi= 0}
  | AVI a ->
      {Frame.audio= a.Avi_format.channels; Frame.video= 1; Frame.midi= 0}
  | MP3 m ->
      {
        Frame.audio= (if m.Mp3_format.stereo then 2 else 1);
        Frame.video= 0;
        Frame.midi= 0;
      }
  | Shine m ->
      {Frame.audio= m.Shine_format.channels; Frame.video= 0; Frame.midi= 0}
  | Flac m ->
      {Frame.audio= m.Flac_format.channels; Frame.video= 0; Frame.midi= 0}
  | Ffmpeg m ->
      {Frame.audio= m.Ffmpeg_format.channels; Frame.video= 0; Frame.midi= 0}
  | FdkAacEnc m ->
      {Frame.audio= m.Fdkaac_format.channels; Frame.video= 0; Frame.midi= 0}
  | Ogg l ->
      List.fold_left
        (fun k -> function
          | Ogg_format.Vorbis {Vorbis_format.channels= n; _} ->
              {k with Frame.audio= k.Frame.audio + n}
          | Ogg_format.Opus {Opus_format.channels= n; _} ->
              {k with Frame.audio= k.Frame.audio + n}
          | Ogg_format.Flac {Flac_format.channels= n; _} ->
              {k with Frame.audio= k.Frame.audio + n} | Ogg_format.Theora _ ->
              {k with Frame.video= k.Frame.video + 1}
          | Ogg_format.Speex {Speex_format.stereo; _} ->
              let n = if stereo then 2 else 1 in
              {k with Frame.audio= k.Frame.audio + n})
        {Frame.audio= 0; Frame.video= 0; Frame.midi= 0}
        l
  | External e ->
      {
        Frame.audio= e.External_encoder_format.channels;
        Frame.video= (if e.External_encoder_format.video then 1 else 0);
        Frame.midi= 0;
      }
  | GStreamer e ->
      {
        Frame.audio= Gstreamer_format.audio_channels e;
        Frame.video= Gstreamer_format.video_channels e;
        Frame.midi= 0;
      }

let kind_of_format f =
  let k = kind_of_format f in
  {
    Frame.audio= Frame.mul_of_int k.Frame.audio;
    Frame.video= Frame.mul_of_int k.Frame.video;
    Frame.midi= Frame.mul_of_int k.Frame.midi;
  }

let string_of_format = function
  | WAV w ->
      Wav_format.to_string w
  | AVI w ->
      Avi_format.to_string w
  | Ogg w ->
      Ogg_format.to_string w
  | MP3 w ->
      Mp3_format.to_string w
  | Shine w ->
      Shine_format.to_string w
  | Flac w ->
      Flac_format.to_string w
  | Ffmpeg w ->
      Ffmpeg_format.to_string w
  | FdkAacEnc w ->
      Fdkaac_format.to_string w
  | External w ->
      External_encoder_format.to_string w
  | GStreamer w ->
      Gstreamer_format.to_string w

(** ISO Base Media File Format, see RFC 6381 section 3.3. *)
let iso_base_file_media_file_format = function
  | MP3 _ | Shine _ ->
      "mp4a.40.34" (* I have also seen "mp4a.69" and "mp3" *)
  | FdkAacEnc m -> (
    match m.Fdkaac_format.aot with
      | `Mpeg_4 `AAC_LC ->
          "mp4a.40.2"
      | `Mpeg_4 `HE_AAC ->
          "mp4a.40.5"
      | `Mpeg_4 `HE_AAC_v2 ->
          "mp4a.40.29"
      | `Mpeg_4 `AAC_LD ->
          "mp4a.40.23"
      | `Mpeg_4 `AAC_ELD ->
          "mp4a.40.39"
      | `Mpeg_2 `AAC_LC ->
          "mp4a.67"
      | `Mpeg_2 `HE_AAC ->
          "mp4a.67" (* TODO: check this *)
      | `Mpeg_2 `HE_AAC_v2 ->
          "mp4a.67"
    (* TODO: check this *) )
  | Ogg [Ogg_format.Speex _] ->
      "speex"
  | Ogg [Ogg_format.Vorbis _] ->
      "vorbis"
  | Ogg [Ogg_format.Flac _] ->
      "flac"
  | Ogg [Ogg_format.Theora _] ->
      "theora"
  | Ogg [Ogg_format.Opus _] ->
      "opus"
  | _ ->
      raise Not_found

(** Proposed extension for files. *)
let extension = function
  | WAV _ ->
      "wav"
  | AVI _ ->
      "avi"
  | Ogg _ ->
      "ogg"
  | MP3 _ ->
      "mp3"
  | Shine _ ->
      "mp3"
  | Flac _ ->
      "flac"
  | FdkAacEnc _ ->
      "aac"
  | _ ->
      raise Not_found

(** Mime types *)
let mime = function
  | WAV _ ->
      "audio/wav"
  | AVI _ ->
      "video/avi"
  | Ogg _ ->
      "application/ogg"
  | MP3 _ ->
      "audio/mpeg"
  | Shine _ ->
      "audio/mpeg"
  | Flac _ ->
      "audio/flex"
  | FdkAacEnc _ ->
      "audio/aac"
  | _ ->
      "application/octet-stream"

(** Bitrate estimation in bits per second. *)
let bitrate = function
  | MP3 w ->
      Mp3_format.bitrate w
  | Shine w ->
      Shine_format.bitrate w
  | FdkAacEnc w ->
      Fdkaac_format.bitrate w
  | _ ->
      raise Not_found

(** An encoder, once initialized, is something that consumes
  * frames, insert metadata and that you eventually close 
  * (triggers flushing). 
  * Insert metadata is really meant for inline metadata, i.e.
  * in most cases, stream sources. Otherwise, metadata are
  * passed when creating the encoder. For instance, the mp3 
  * encoder may accept metadata initally and write them as 
  * id3 tags but does not support inline metadata. 
  * Also, the ogg encoder supports inline metadata but restarts
  * its stream. This is ok, though, because the ogg container/streams 
  * is meant to be sequentialized but not the mp3 format. 
  * header contains data that should be sent first to streaming 
  * client. *)
type encoder = {
  insert_metadata: Meta_format.export_metadata -> unit;
  (* Encoder are all called from the main 
   * thread so there's no need to protect this
   * value with a mutex so far.. *)
  mutable header: Strings.t;
  encode: Frame.t -> int -> int -> Strings.t;
  stop: unit -> Strings.t;
}

type factory = string -> Meta_format.export_metadata -> encoder

(** A plugin might or might not accept a given format.
  * If it accepts it, it gives a function creating suitable encoders. *)
type plugin = format -> factory option

let plug : plugin Plug.plug =
  Plug.create ~doc:"Methods to encode streams." ~insensitive:true
    "stream encoding formats"

exception Found of factory

(** Return the first available encoder factory for that format. *)
let get_factory fmt =
  try
    plug#iter (fun _ f ->
        match f fmt with Some factory -> raise (Found factory) | None -> ()) ;
    raise Not_found
  with Found factory -> factory
