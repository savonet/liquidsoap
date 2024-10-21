(*****************************************************************************

  Liquidsoap, a programmable stream generator.
  Copyright 2003-2024 Savonet team

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
  | Ffmpeg of Ffmpeg_format.t
  | FdkAacEnc of Fdkaac_format.t
  | External of External_encoder_format.t

val audio_type : pcm_kind:Content.kind -> int -> Type.t Frame.Fields.t
val video_format : unit -> Content.format
val video_type : unit -> Type.t Frame.Fields.t
val audio_video_type : pcm_kind:Content.kind -> int -> Type.t Frame.Fields.t
val type_of_format : format -> Type.t Frame.Fields.t
val string_of_format : format -> string

(** ISO Base Media File Format, see RFC 6381 section 3.3. *)
val iso_base_file_media_file_format : format -> string

(** Proposed extension for files. *)
val extension : format -> string

(** Mime types *)
val mime : format -> string

(** Video size when available. *)
val video_size : format -> (int * int) option

(** Bitrate estimation in bits per second. *)
val bitrate : format -> int

(** Encoders that can output to a file. *)
val file_output : format -> bool

val with_file_output : ?append:bool -> format -> string -> format

(** Encoders that can output to a arbitrary url. *)
val url_output : format -> bool

val with_url_output : format -> string -> format

(** An encoder, once initialized, is something that consumes
  * frames, insert metadata and that you eventually close
  * (triggers flushing).
  * Insert metadata is really meant for inline metadata, i.e.
  * in most cases, stream sources. Otherwise, metadata are
  * passed when creating the encoder. For instance, the mp3
  * encoder may accept metadata initially and write them as
  * id3 tags but does not support inline metadata.
  * Also, the ogg encoder supports inline metadata but restarts
  * its stream. This is ok, though, because the ogg container/streams
  * is meant to be sequentialized but not the mp3 format.
  * header contains data that should be sent first to streaming
  * client. *)

type split_result =
  [ (* Returns (flushed, first_bytes_for_next_segment) *)
    `Ok of
    Strings.t * Strings.t
  | `Nope of Strings.t ]

(* Raised by [init_encode] if more data is needed. *)
exception Not_enough_data

type hls = {
  (* Returns true if id3 is enabled. *)
  init : ?id3_enabled:bool -> ?id3_version:int -> unit -> bool;
  (* Returns (init_segment, first_bytes) *)
  init_encode : Frame.t -> Strings.t option * Strings.t;
  split_encode : Frame.t -> split_result;
  codec_attrs : unit -> string option;
  insert_id3 :
    frame_position:int ->
    sample_position:int ->
    (string * string) list ->
    string option;
  bitrate : unit -> int option;
  (* width x height *)
  video_size : unit -> (int * int) option;
}

val dummy_hls : (Frame.t -> Strings.t) -> hls

type encoder = {
  insert_metadata : Frame.Metadata.Export.t -> unit;
  header : unit -> Strings.t;
  hls : hls;
  encode : Frame.t -> Strings.t;
  stop : unit -> Strings.t;
}

type factory =
  ?hls:bool -> pos:Pos.t option -> string -> Frame.Metadata.Export.t -> encoder

(** A plugin might or might not accept a given format.
  * If it accepts it, it gives a function creating suitable encoders. *)
type plugin = format -> factory option

val plug : plugin Plug.t

(** Return the first available encoder factory for that format. *)
val get_factory : format -> factory
