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

(** HLS output of an encoder (ffmpeg spec encoder.md §8.4), times in main ticks
    of media time: bytes, an accepted split point, or a container restart, which
    is a split point that also starts a new container with its init segment.
    [forced] marks a split made without its key-packet condition. *)
type hls_split = { time : int; forced : bool }

type hls_restart = { restart_time : int; init : Strings.t option }

type hls_piece =
  [ `Bytes of Strings.t | `Split of hls_split | `Restart of hls_restart ]

(** [`Pending] keeps the bytes for the next call; [`Init (init, pieces)] gives
    the init segment, none for a format without one, and every byte after it. *)
type hls_init = [ `Pending | `Init of Strings.t option * hls_piece list ]

(** The HLS output calls [init], then [init_encode] until it returns [`Init],
    then [split_encode] for each later frame, with the next grid boundary;
    [force] asks for a split before the frame whatever its packets.
    [independent_segments] tells, once [`Init] is returned, that every segment
    starts with a key packet of every stream. *)
type hls = {
  init : ?id3_enabled:bool -> ?id3_version:int -> unit -> bool;
  init_encode : boundary:int -> Frame.t -> hls_init;
  split_encode : boundary:int -> force:bool -> Frame.t -> hls_piece list;
  codec_attrs : unit -> string option;
  independent_segments : unit -> bool;
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

(** An encoder, once initialized, is something that consumes frames, insert
    metadata and that you eventually close (triggers flushing). Insert metadata
    is really meant for inline metadata, i.e. in most cases, stream sources.
    Otherwise, metadata are passed when creating the encoder. For instance, the
    mp3 encoder may accept metadata initially and write them as id3 tags but
    does not support inline metadata. Also, the ogg encoder supports inline
    metadata but restarts its stream. This is ok, though, because the ogg
    container/streams is meant to be sequentialized but not the mp3 format.
    header contains data that should be sent first to streaming client.

    [restart_offset ()] is the offset, in the bytes the latest [encode] or
    [stop] returned, at which a new stream starts with its header pages (an Ogg
    link after a metadata change); [None] when those bytes start none. A
    streaming output starts a late listener there rather than before it. *)
type encoder = {
  encode_metadata : Frame.Metadata.Export.t -> unit;
  header : unit -> Strings.t;
  hls : hls;
  encode : Frame.t -> Strings.t;
  stop : unit -> Strings.t;
  restart_offset : unit -> int option;
}

(** [interrupt] aborts the blocking I/O of an encoder that writes to a target of
    its own (an [%ffmpeg] URL); the other encoders ignore it. *)
type factory =
  ?hls:bool ->
  ?interrupt:(unit -> bool) ->
  pos:Pos.t option ->
  string ->
  Frame.Metadata.Export.t ->
  encoder

(** A plugin might or might not accept a given format. If it accepts it, it
    gives a function creating suitable encoders. *)
type plugin = format -> factory option

val plug : plugin Plug.t

(** Return the first available encoder factory for that format. *)
val get_factory : format -> factory
