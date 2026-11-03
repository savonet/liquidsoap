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

(** Common infrastructure for encoding streams *)

type format =
  | WAV of Wav_format.t
  | AVI of Avi_format.t
  | NDI of Ndi_format.t
  | Ogg of Ogg_format.t
  | MP3 of Mp3_format.t
  | Shine of Shine_format.t
  | Flac of Flac_format.t
  | Ffmpeg of Liquidsoap_ffmpeg_formats.Encoder_format.t
  | FdkAacEnc of Fdkaac_format.t
  | External of External_encoder_format.t

let audio_type = Encoder_types.audio_type
let video_format = Encoder_types.video_format
let audio_video_type = Encoder_types.audio_video_type
let video_type = Encoder_types.video_type

let type_of_format f =
  let audio_type = audio_type ~pcm_kind:Content_audio.kind in
  let audio_video_type = audio_video_type ~pcm_kind:Content_audio.kind in
  match f with
    | WAV w -> audio_type w.Wav_format.channels
    | AVI a -> audio_video_type a.Avi_format.channels
    | MP3 m -> audio_type (if m.Mp3_format.stereo then 2 else 1)
    | Shine m -> audio_type m.Shine_format.channels
    | NDI { audio = false; video = false } -> assert false
    | NDI { audio = true; video = false } ->
        audio_type (Lazy.Mutexed.force Frame.audio_channels)
    | NDI { audio = true; video = true } ->
        audio_video_type (Lazy.Mutexed.force Frame.audio_channels)
    | NDI { audio = false; video = true } -> video_type ()
    | Flac m -> audio_type m.Flac_format.channels
    | Ffmpeg m -> Liquidsoap_ffmpeg_formats.Encoder_format.type_of_format m
    | FdkAacEnc m -> audio_type m.Fdkaac_format.channels
    | Ogg { Ogg_format.audio; video } -> (
        let channels =
          match audio with
            | Some (Ogg_format.Vorbis { Vorbis_format.channels = n; _ })
            | Some (Ogg_format.Opus { Opus_format.channels = n; _ })
            | Some (Ogg_format.Flac { Flac_format.channels = n; _ }) ->
                n
            | Some (Ogg_format.Speex { Speex_format.stereo; _ }) ->
                if stereo then 2 else 1
            | None -> 0
        in
        match (channels, video) with
          | 0, Some _ -> video_type ()
          | n, Some _ -> audio_video_type n
          | n, None -> audio_type n)
    | External e ->
        let channels = e.External_encoder_format.channels in
        if e.External_encoder_format.video <> None then
          audio_video_type channels
        else audio_type channels

let string_of_format = function
  | WAV w -> Wav_format.to_string w
  | AVI w -> Avi_format.to_string w
  | Ogg w -> Ogg_format.to_string w
  | MP3 w -> Mp3_format.to_string w
  | NDI w -> Ndi_format.to_string w
  | Shine w -> Shine_format.to_string w
  | Flac w -> Flac_format.to_string w
  | Ffmpeg w -> Liquidsoap_ffmpeg_formats.Encoder_format.to_string w
  | FdkAacEnc w -> Fdkaac_format.to_string w
  | External w -> External_encoder_format.to_string w

let video_size = function
  | Ogg { Ogg_format.video = Some { Theora_format.width; height } } ->
      Some (Lazy.Mutexed.force width, Lazy.Mutexed.force height)
  | Ffmpeg m -> Liquidsoap_ffmpeg_formats.Encoder_format.video_size m
  | _ -> None

(** ISO Base Media File Format, see RFC 6381 section 3.3. *)
let iso_base_file_media_file_format = function
  | MP3 _ | Shine _ -> "mp4a.40.34" (* I have also seen "mp4a.69" and "mp3" *)
  | FdkAacEnc m -> (
      match m.Fdkaac_format.aot with
        | `Mpeg_4 `AAC_LC -> "mp4a.40.2"
        | `Mpeg_4 `HE_AAC -> "mp4a.40.5"
        | `Mpeg_4 `HE_AAC_v2 -> "mp4a.40.29"
        | `Mpeg_4 `AAC_LD -> "mp4a.40.23"
        | `Mpeg_4 `AAC_ELD -> "mp4a.40.39"
        | `Mpeg_2 `AAC_LC -> "mp4a.67"
        | `Mpeg_2 `HE_AAC -> "mp4a.67" (* TODO: check this *)
        | `Mpeg_2 `HE_AAC_v2 -> "mp4a.67" (* TODO: check this *))
  | _ -> raise Not_found

(** Proposed extension for files. *)
let extension = function
  | WAV _ -> "wav"
  | AVI _ -> "avi"
  | Ogg _ -> "ogg"
  | MP3 _ -> "mp3"
  | Shine _ -> "mp3"
  | Flac _ -> "flac"
  | FdkAacEnc _ -> "aac"
  | Ffmpeg m -> (
      match Liquidsoap_ffmpeg_formats.Encoder_format.extension m with
        | Some ext -> ext
        | None -> raise Not_found)
  | _ -> raise Not_found

(** Mime types *)
let mime = function
  | WAV _ -> "audio/wav"
  | AVI _ -> "video/avi"
  | Ogg _ -> "application/ogg"
  | MP3 _ -> "audio/mpeg"
  | Shine _ -> "audio/mpeg"
  | Flac _ -> "audio/flex"
  | FdkAacEnc _ -> "audio/aac"
  | Ffmpeg m -> Liquidsoap_ffmpeg_formats.Encoder_format.mime m
  | _ -> "application/octet-stream"

(** Bitrate estimation in bits per second. *)
let bitrate = function
  | MP3 w -> Mp3_format.bitrate w
  | Shine w -> Shine_format.bitrate w
  | FdkAacEnc w -> Fdkaac_format.bitrate w
  | _ -> raise Not_found

(** The bytes of one encoder output, readable only during the call that hands
    them over: [blit offset destination destination_offset length] copies a
    range of them. *)
type output = { length : int; blit : int -> bytes -> int -> int -> unit }

(** An encoder, once initialized, is something that consumes frames, insert
    metadata and that you eventually close (triggers flushing). Insert metadata
    is really meant for inline metadata, i.e. in most cases, stream sources.
    Otherwise, metadata are passed when creating the encoder. For instance, the
    mp3 encoder may accept metadata initially and write them as id3 tags but
    does not support inline metadata. Also, the ogg encoder supports inline
    metadata but restarts its stream. This is ok, though, because the ogg
    container/streams is meant to be sequentialized but not the mp3 format.
    header contains data that should be sent first to streaming client. *)
type encoder = {
  encode_metadata : Frame.Metadata.Export.t -> unit;
  header : unit -> Strings.t;
  encode : Frame.t -> Strings.t;
  encode_into : (Frame.t -> (output -> unit) -> unit) option;
  stop : unit -> Strings.t;
  bitrate : unit -> int option;
  restart_offset : unit -> int option;
}

type factory = pos:Pos.t option -> string -> Frame.Metadata.Export.t -> encoder
type hls_id3_mode = [ `None | `In_stream | `Prepended ]

type hls_piece =
  | Bytes of Strings.t
  | Split of { time : int; forced : bool }
  | Restart of { time : int; init : string option }

type hls_ready = { init : string option; pieces : hls_piece list }
type hls_init = Pending | Init of hls_ready

type hls_encoder = {
  init : ?id3:bool -> ?id3_version:int -> unit -> (hls_id3_mode, string) result;
  init_encode : boundary:int -> Frame.t -> hls_init;
  split_encode : boundary:int -> force:bool -> Frame.t -> hls_piece list;
  insert_id3 : time:int -> Frame.Metadata.Export.t -> string option;
  hls_stop : unit -> hls_ready;
  codec_attrs : unit -> string option;
  independent_segments : unit -> bool;
  free_split : unit -> bool;
  split_due : unit -> bool;
  hls_bitrate : unit -> int option;
  hls_video_size : unit -> (int * int) option;
}

type hls_factory = pos:Pos.t option -> string -> hls_encoder

(** A plugin might or might not accept a given format. If it accepts it, it
    gives a function creating suitable encoders. *)
type plugin = format -> factory option

let plug : plugin Plug.t =
  Plug.create ~doc:"Methods to encode streams." "stream encoding formats"

exception Found of factory

let protect
    { encode_metadata; encode; encode_into; stop; header; bitrate; restart_offset }
    =
  let m = Mutex.create () in
  let encode_metadata metadata =
    Mutex.protect m (fun () -> encode_metadata metadata)
  in
  let header () = Mutex.protect m header in
  let encode frame = Mutex.protect m (fun () -> encode frame) in
  let encode_into =
    Option.map
      (fun encode_into frame f ->
        Mutex.protect m (fun () -> encode_into frame f))
      encode_into
  in
  let stop () = Mutex.protect m stop in
  let bitrate () = Mutex.protect m bitrate in
  let restart_offset () = Mutex.protect m restart_offset in
  { encode_metadata; encode; encode_into; stop; header; bitrate; restart_offset }

(** Return the first available encoder factory for that format. *)
let get_factory fmt =
  try
    Plug.iter plug (fun _ f ->
        match f fmt with Some factory -> raise (Found factory) | None -> ());
    raise Not_found
  with Found factory -> fun ~pos name m -> protect (factory ~pos name m)

let hls_plug : (format -> hls_factory option) Plug.t =
  Plug.create ~doc:"Methods to encode HLS segments." "hls encoding formats"

exception Found_hls of hls_factory

let protect_hls e =
  let m = Mutex.create () in
  let p f = Mutex.protect m f in
  {
    init =
      (fun ?id3 ?id3_version () -> p (fun () -> e.init ?id3 ?id3_version ()));
    init_encode = (fun ~boundary f -> p (fun () -> e.init_encode ~boundary f));
    split_encode =
      (fun ~boundary ~force f ->
        p (fun () -> e.split_encode ~boundary ~force f));
    insert_id3 = (fun ~time m -> p (fun () -> e.insert_id3 ~time m));
    hls_stop = (fun () -> p e.hls_stop);
    codec_attrs = (fun () -> p e.codec_attrs);
    independent_segments = (fun () -> p e.independent_segments);
    free_split = (fun () -> p e.free_split);
    split_due = (fun () -> p e.split_due);
    hls_bitrate = (fun () -> p e.hls_bitrate);
    hls_video_size = (fun () -> p e.hls_video_size);
  }

let get_hls_factory fmt =
  try
    Plug.iter hls_plug (fun _ f ->
        match f fmt with
          | Some factory -> raise (Found_hls factory)
          | None -> ());
    raise Not_found
  with Found_hls factory -> fun ~pos name -> protect_hls (factory ~pos name)
