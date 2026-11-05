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
  encode : Frame.t -> Strings.t;
  stop : unit -> Strings.t;
  restart_offset : unit -> int option;
}

type factory = pos:Pos.t option -> string -> Frame.Metadata.Export.t -> encoder

(** The HLS encoder seam (spec of the HLS output, encoder-contract.md). Times
    are media time of the run, in main ticks. [init] answers the variant's ID3
    mode or rejects its [id3] field with a reason; the other calls follow the
    contract's order, serialised by {!get_hls_factory}.

    Pieces of [split_encode], in byte order:
    - [Bytes]: output bytes;
    - [Split]: a position where a segment may start, [time] the media time of
      what follows, [forced] when made without the encoder's condition;
    - [Restart]: a split point that also starts a new container, with its init
      segment. *)

type hls_id3_mode = [ `None | `In_stream | `Prepended ]

type hls_piece =
  | Bytes of Strings.t
  | Split of { time : int; forced : bool }
  | Restart of { time : int; init : string option }

type hls_init =
  | Pending
  | Init of { init : string option; pieces : hls_piece list }

type hls_encoder = {
  init : ?id3:bool -> ?id3_version:int -> unit -> (hls_id3_mode, string) result;
  init_encode : boundary:int -> Frame.t -> hls_init;
  split_encode : boundary:int -> force:bool -> Frame.t -> hls_piece list;
  insert_id3 : time:int -> Frame.Metadata.Export.t -> string option;
  hls_header : unit -> Strings.t;
  hls_stop : unit -> hls_piece list;
  codec_attrs : unit -> string option;
  independent_segments : unit -> bool;
  free_split : unit -> bool;
  hls_bitrate : unit -> int option;
  hls_video_size : unit -> (int * int) option;
}

type hls_factory = pos:Pos.t option -> string -> hls_encoder

(** A plugin might or might not accept a given format. If it accepts it, it
    gives a function creating suitable encoders. *)
type plugin = format -> factory option

val plug : plugin Plug.t

(** [protect encoder] serialises the calls to [encoder] with a mutex, as every
    encoder from {!get_factory} is. *)
val protect : encoder -> encoder

(** Return the first available encoder factory for that format. *)
val get_factory : format -> factory

(** Encoders that can write HLS segments register here. *)
val hls_plug : (format -> hls_factory option) Plug.t

(** The HLS factory for that format, its encoders serialised; [Not_found] for a
    format no plug-in segments. *)
val get_hls_factory : format -> hls_factory
