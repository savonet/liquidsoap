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

type format = Format_value.t =
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

    [bitrate ()] is the nominal bit rate, in bits per second, once the encoder
    knows it; streaming outputs announce it.

    [restart_offset ()] is the offset, in the bytes the latest [encode] or
    [stop] returned, at which a new stream starts with its header pages (an Ogg
    link after a metadata change); [None] when those bytes start none. A
    streaming output starts a late listener there rather than before it.

    [access_points ()], when given, are the offsets, in the bytes the latest
    [encode] or [stop] returned, at which a reader can start; an encoder that
    gives none returns whole units, and a reader can start at the first byte of
    each call's output. *)
type encoder = {
  encode_metadata : Frame.Metadata.Export.t -> unit;
  header : unit -> Strings.t;
  encode : Frame.t -> Strings.t;
  access_points : (unit -> int list) option;
  stop : unit -> Strings.t;
  bitrate : unit -> int option;
  restart_offset : unit -> int option;
}

(** The HLS encoder seam (spec of the HLS output, encoder-contract.md). Times
    are media time of the run, in main ticks. [init] answers the variant's ID3
    mode or rejects its [id3] field with a reason; the other calls follow the
    contract's order, serialised by {!get_hls_factory}.

    Pieces of [split_encode], in byte order:
    - [Bytes]: output bytes;
    - [Split]: a position where a segment may start, [time] the media time of
      what follows;
    - [Restart]: a split point that also starts a new container, with its init
      segment. *)

type hls_id3_mode = [ `None | `In_stream | `Prepended ]

type hls_piece =
  | Bytes of Strings.t
  | Split of { time : int }
  | Restart of { time : int; init : string option }

type hls_ready = { init : string option; pieces : hls_piece list }
type hls_init = Pending | Init of hls_ready

type hls_encoder = {
  init : ?id3:bool -> ?id3_version:int -> unit -> (hls_id3_mode, string) result;
  init_encode : boundary:int -> Frame.t -> hls_init;
  split_encode : boundary:int -> Frame.t -> hls_piece list;
  insert_id3 : time:int -> Frame.Metadata.Export.t -> string option;
  hls_stop : unit -> hls_ready;
  codec_attrs : unit -> string option;
  independent_segments : unit -> bool;
  free_split : unit -> bool;
  hls_bitrate : unit -> int option;
  hls_video_size : unit -> (int * int) option;
}

type hls_factory =
  pos:Pos.t option -> segment_duration:float -> string -> hls_encoder

(** {1 Writers}

    A writer writes whole units into a sink its output owns: every [write] of
    the sink ends on a unit boundary, and markers sit between writes. Times are
    media times of the writer's run, in main ticks. *)

type stream_start = {
  header_length : int option;
      (** The header is the bytes that follow; [None] when the writer cannot
          tell. *)
  time : int;
}

type marker = [ `Stream_start of stream_start | `Access_point of int | `End ]

type sink = {
  write : Strings.t -> unit;
  mark : marker -> unit;  (** Refers to the next byte written. *)
  seek : (int -> unit) option;
  read : (int -> int -> string) option;  (** [read offset length] *)
  size : (unit -> int) option;  (** Bytes the destination holds. *)
  name : string option;
      (** The destination's name, for a writer that guesses its format from one.
      *)
}

(** What the destination is, known when a writer is created. *)
type capabilities = {
  seekable : bool;
  continuing : bool;  (** It holds a stream that this writer extends. *)
  live : bool;  (** Readers start in the middle of the stream. *)
  segmented : bool;  (** The output cuts the stream into segments. *)
}

(** A destination written once from its first byte: a pipe, a process, a new
    file that cannot seek. *)
val fresh_stream : capabilities

(** A server or a set of listeners. *)
val live_stream : capabilities

val segmented : capabilities

type creation = {
  pos : Pos.t option;
  name : string;
  capabilities : capabilities;
  sink : sink;
  initial_metadata : Frame.Metadata.Export.t;
}

type writer = {
  write : Frame.t -> unit;
  metadata : Frame.Metadata.Export.t -> unit;  (** In-band offer. *)
  finish : [ `Clean | `Abandoned ] -> unit;
      (** [`Clean] writes the final bytes then [`End]; [`Abandoned] releases the
          writer and writes nothing. *)
  writer_bitrate : unit -> int option;
}

(** A plug-in answers the formats it can write, with what creates their writer.
*)
type plugin = format -> (creation -> writer) option

val plug : plugin Plug.t

(** The writer of an encoder returning its bytes: the stream start comes from
    its header and restart offset, the access points from its [access_points].
    [before_end] runs at a clean end once the final bytes are written. *)
val writer_of_encoder : ?before_end:(unit -> unit) -> encoder -> sink -> writer

(** A plug-in whose writers are such encoders. *)
val of_encoders :
  (format ->
  (pos:Pos.t option -> string -> Frame.Metadata.Export.t -> encoder) option) ->
  plugin

(** Serialises the calls of an encoder used without the plug. *)
val protect_encoder : encoder -> encoder

(** Whether a plug-in answers that format. *)
val supported : format -> bool

(** The writer of the first plug-in answering that format, its calls serialised;
    [Not_found] when none does. *)
val create_writer : format -> creation -> writer

(** Whether a destination takes the file form of a format: a file written from
    its first byte, whose writer can come back to complete what describes the
    whole stream. *)
val file_form : capabilities -> bool

(** The ID3v2 tag an MPEG audio stream starts with for that [id3v2] parameter:
    by default on a file only; empty for none. *)
val id3v2_tag :
  capabilities:capabilities ->
  int option Encoder_formats.adaptable ->
  Frame.Metadata.Export.t ->
  string

(** Why a destination cannot take a format, when it cannot: an explicit
    parameter or a format it contradicts. *)
val refusal : capabilities -> format -> string option

(** A writer for an output that queues bytes itself, as an encoder returning
    them: [header ()] is the header of the current stream, for a reader that
    starts after it, [restart_offset ()] the offset, in the bytes last returned,
    at which a stream started, and [access_points ()] the offsets in them at
    which a reader can start. *)
val buffered :
  ?pos:Pos.t ->
  name:string ->
  capabilities:capabilities ->
  initial_metadata:Frame.Metadata.Export.t ->
  format ->
  encoder

(** Resolves the deferred values that the format's checks need and runs those
    checks, raising a positioned error. *)
val resolve : pos:Pos.t option -> format -> unit

(** Resolves a format given to an output once the script has been evaluated, or
    at once after that. *)
val resolve_at_start : pos:Pos.t option -> format -> unit

(** Encoders that can write HLS segments register here. *)
val hls_plug : (format -> hls_factory option) Plug.t

(** The HLS factory for that format, its encoders serialised; [Not_found] for a
    format no plug-in segments. Each plug-in accepts only the formats it writes
    as RFC 8216 segments. *)
val get_hls_factory : format -> hls_factory
