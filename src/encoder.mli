(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2020 Savonet team

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

val audio_kind : int -> Frame.kind Frame.fields
val audio_video_kind : int -> Frame.kind Frame.fields
val kind_of_format : format -> Frame.kind Frame.fields
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

val with_file_output : format -> string -> format

(** Encoders that can output to a arbitrary url. *)
val url_output : format -> bool

val with_url_output : format -> string -> format

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

type split_result =
  [ (* Returns (flushed, first_bytes_for_next_segment) *)
    `Ok of
    Strings.t * Strings.t
  | `Nope of Strings.t ]

type hls = {
  (* Returns (init_segment, first_bytes) *)
  init_encode : Frame.t -> int -> int -> Strings.t option * Strings.t;
  split_encode : Frame.t -> int -> int -> split_result;
  codec_attrs : unit -> string option;
  bitrate : unit -> int option;
  (* width x height *)
  video_size : unit -> (int * int) option;
}

type encoder = {
  insert_metadata : Meta_format.export_metadata -> unit;
  (* Encoder are all called from the main 
   * thread so there's no need to protect this
   * value with a mutex so far.. *)
  mutable header : Strings.t;
  hls : hls;
  encode : Frame.t -> int -> int -> Strings.t;
  stop : unit -> Strings.t;
}

type factory = string -> Meta_format.export_metadata -> encoder

(** A plugin might or might not accept a given format.
  * If it accepts it, it gives a function creating suitable encoders. *)
type plugin = format -> factory option

val plug : plugin Plug.plug

(** Return the first available encoder factory for that format. *)
val get_factory : format -> factory
