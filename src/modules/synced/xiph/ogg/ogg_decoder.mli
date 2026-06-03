(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2011 Savonet team

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

(** Ogg stream demuxer *)

(** This module provides a functional abstract API to * decode and seek in Ogg
    streams. * * Decoders are also provided in ocaml-vorbis, * ocaml-speex,
    ocaml-schroedinger, ocaml-flac and * ocaml-theora. * * Functions in this
    module are not thread safe! *)

(** {2 Decoding} *)

(** {3 Types} *)

(** Type of an ogg stream decoder. *)
type t

(** Type for callbacks used to access encoded data. *)
type callbacks = {
  read : bytes -> int -> int -> int;
  seek : (int -> int) option;
  tell : (unit -> int) option;
}

(** Type for a decodable track. * First element is a string describing * the
    decoder used to decode the track. * Second element is the serial number *
    associated to the [Ogg.Stream.stream] logical * stream used to pull data
    packets for that * track. *)
type track =
  | Audio_track of (string * nativeint)
  | Video_track of (string * nativeint)

(** Type for standard tracks (see [get_standard_tracks] below). *)
type standard_tracks = {
  mutable audio_track : track option;
  mutable video_track : track option;
}

(** Type for metadata. First element * is a string describing the vendor, second
    * element is a list of metadata of the form: * [(label,value)]. *)
type metadata = string * (string * string) list

(** Type for audio information. *)
type audio_info = { channels : int; sample_rate : int }

(** Type for audio data. *)
type audio_data = float array array

type audio_ba_data =
  (float, Bigarray.float32_elt, Bigarray.c_layout) Bigarray.Array1.t array

(** Type of a video plane. *)
type video_plane =
  (int, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

(** Supported video formats. *)
type video_format =
  (* Planar YCbCr 4:2:0. Each component is an uint8_t,
              * luma and chroma values are full range (0x00 .. 0xff) *)
  | Yuvj_420
  (* Planar YCbCr 4:2:2. Each component is an uint8_t,
              * luma and chroma values are full range (0x00 .. 0xff) *)
  | Yuvj_422
  (* Planar YCbCr 4:4:4. Each component is an uint8_t,
   * luma and chroma values are full range (0x00 .. 0xff) *)
  | Yuvj_444

(* Type for video information. *)
type video_info = {
  fps_numerator : int;
  fps_denominator : int;
  (* Width of the Y' luminance plane *)
  width : int;
  (* Height of the luminance plane *)
  height : int;
}

(** Type for video data. *)
type video_data = {
  format : video_format;
  frame_width : int;
  frame_height : int;
  y_stride : int;  (** Length, in bytes, per line *)
  uv_stride : int;  (** Length, in bytes, per line *)
  y : video_plane;  (** luminance data *)
  u : video_plane;  (** Cb data *)
  v : video_plane;  (** Cr data *)
}

(** {3 Exceptions} *)

exception Invalid_stream
exception Not_available

(* This exception has a different semantics than [Ogg.End_of_stream].
 * [Ogg.End_of_stream] is raised when end of data has been reached,
 * while this exception is raised when end of a logical stream has
 * been reached.. *)
exception End_of_stream

(** {3 Initialization functions} *)

(** Initiate a decoder with the given callbacks. * [log] is an optional
    functioned used to * return logged messages during the decoding * process.
*)
val init : ?log:(string -> unit) -> callbacks -> t

(** Initiate a decoder from a given file name. *)
val init_from_file : ?log:(string -> unit) -> string -> t * Unix.file_descr

(** Initiate a decoder from a given [Unix.file_descriptor] *)
val init_from_fd : ?log:(string -> unit) -> Unix.file_descr -> t

(** Get the Ogg.Sync handler associated to * the decoder. Use only if know what
    you are doing. *)
val get_ogg_sync : t -> Ogg.Sync.t

(** Reset encoder, try to parse a new sequentialized stream. * To use when
    end_of_stream has been reached. *)
val reset : t -> unit

(** Consume all remaining pages of the current * stream. This function may be
    called to skip * a sequentialized stream but it may be quite * CPU intensive
    if there are many pages remaining.. * * [eos dec] is [true] after this call.
*)
val abort : t -> unit

(** [true] if the decoder has reached the end of each * logical streams and all
    data has been decoded. * * If you do not plan on decoding some data, * you
    should use [drop_track] to indicate it * to the decoder. Otherwise, [eos]
    will return * [false] until you have decoded all data. *)
val eos : t -> bool

(** Get all decodable tracks available. *)
val get_tracks : t -> track list

(** Get the first available audio and * video tracks and drop the other one. *)
val get_standard_tracks : t -> standard_tracks

(** Update a given record of standard tracks. You should * use this after a
    [reset] to update the standard tracks * with the newly created tracks. *)
val update_standard_tracks : t -> standard_tracks -> unit

(** Remove all tracks of the given type. *)
val drop_track : t -> track -> unit

(** {3 Information functions} *)

(** Get information about the * audio track. *)
val audio_info : t -> track -> audio_info * metadata

(** [true] if the decoder can decoder to bigarray data. *)
val can_decode_ba : t -> track -> bool

(** Get information about the * video track. *)
val video_info : t -> track -> video_info * metadata

(** Get the sample_rate of the track * of that type. Returns a pair
    [(numerator,denominator)]. *)
val sample_rate : t -> track -> int * int

(** Get track absolute position. *)
val get_track_position : t -> track -> float

(** Get absolute position in the stream. *)
val get_position : t -> float

(** {3 Seeking functions} *)

(** Returns [true] if the decoder * can be used with the [seek] function. *)
val can_seek : t -> bool

(** Seek to an absolute or relative position in seconds. * * Raises
    [Not_available] if seeking is * not possible. * * Raises [End_of_stream] if
    the end of * current stream has been reached while * seeking. You may call
    [reset] in this * situation to see if there is a new seqentialized * stream
    available. * * Returns the time actually reached, either in * relative time
    or absolute time. *)
val seek : ?relative:bool -> t -> float -> float

(** {3 Decoding functions} *)

(** Decode audio data, if possible. * Decoded data is passed to the second
    argument. * * Raises [End_of_stream] if all stream have ended. * In this
    case, you can try [reset] to see if there is a * new sequentialized stream.
*)
val decode_audio : t -> track -> (audio_data -> unit) -> unit

(** Decode audio data, if possible. * Decoded data is passed to the second
    argument. * * Raises [End_of_stream] if all stream have ended. * In this
    case, you can try [reset] to see if there is a * new sequentialized stream.
*)
val decode_audio_ba : t -> track -> (audio_ba_data -> unit) -> unit

(** Decode video data, if possible. * Decoded data is passed to the second
    argument. * * Raises [End_of_stream] if all streams have ended. * In this
    case, you can try [reset] to see if there is a * new sequentialized stream.
*)
val decode_video : t -> track -> (video_data -> unit) -> unit

(** {2 Implementing decoders} *)

(** {3 Types} *)

(** Generic type for a decoder. *)
type ('a, 'b) decoder = {
  name : string;
  info : unit -> 'a * metadata;
  decode : ('b -> unit) -> unit;
  restart : fill:(unit -> unit) -> Ogg.Stream.stream -> unit;
  (* This function is called after seeking
   * to notify the decoder of the new [Ogg.Stream.stream]
   * that is should use to pull data packets. *)
  samples_of_granulepos : Int64.t -> Int64.t;
}

(** Type for a generic logical stream decoder. *)
type decoders =
  | Video of (video_info, video_data) decoder
  | Audio of (audio_info, audio_data) decoder
  | Audio_ba of (audio_info, audio_ba_data) decoder
  | Audio_both of
      (audio_info, audio_data) decoder * (audio_info, audio_ba_data) decoder
  | Unknown

(** Type used to register a new decoder. First * element is a function used to
    check if the initial [Ogg.Stream.packet] * of an [Ogg.Stream.stream] matches
    the format decodable by this decoder. * Second element is a function that
    instantiates the actual decoder * using the initial [Ogg.Stream.stream] used
    to pull data packets for the * decoder. *)
type register_decoder =
  (Ogg.Stream.packet -> bool)
  * (fill:(unit -> unit) -> Ogg.Stream.stream -> decoders)

(** {3 Functions} *)

(** Register a new decoder. *)
val ogg_decoders : (string, register_decoder) Hashtbl.t
