(*
 * Copyright 2003-2011 The Savonet team
 *
 * This file is part of Ocaml-vorbis.
 *
 * Ocaml-vorbis is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * Ocaml-vorbis is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with Ocaml-vorbis; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *)

(**
  * Decode from or encode to the Ogg Vorbis compressed audio format;
  * or get information about an Ogg Vorbis file.
  *
  * @author Samuel Mimram, Julien Cristau, David Baelde
  *)

(* $Id$ *)

(** {1 Exceptions} *)

(** The call returned a 'false' status (eg, ov_bitrate_instant * can return
    OV_FALSE if playback is not in progress, and thus * there is no
    instantaneous bitrate information to report. *)
exception False

(** Some parameters are invalid for this function. *)
exception Invalid_parameters

(** The given number of channels is invalid. *)
exception Invalid_channels

(** Invalid setup request, e.g. out of range argument. *)
exception Invalid_argument

(** The given file could not be opened. *)
exception Could_not_open_file

(** Bitstream is not Vorbis data. *)
exception Not_vorbis

(** Invalid Vorbis bitstream header. *)
exception Bad_header

(** A read from media returned an error. *)
exception Read_error

(** Ogg packet doesn't contain audio data *)
exception Not_audio

(** Internal logic fault; indicates a bug or heap/stack corruption. *)
exception Internal_fault

(** Indicates there was an interruption in the data (one of: garbage between *
    pages, loss of sync followed by recapture, or a corrupt page). *)
exception Hole_in_data

(** Indicates that an invalid stream section was supplied, * or the requested
    link is corrupt. *)
exception Bad_link

(** Invalid Vorbis bitstream header. *)
exception Version_mismatch

(** Unimplemented mode. *)
exception Not_implemented

(** An unknown error happened (it should not have happened, please report). *)
exception Unknown_error of int

(** Error while converting utf8. *)
exception Utf8_failure of string

(** Return a string representation * of an exception *)
val string_of_exc : exn -> string option

(** {1 Useful types} *)

(** Index of a logical bitstream. The special value -1 means the physical *
    bitsream. *)
type bitstream = int

(** Vorbis information about a file. *)
type info = {
  vorbis_version : int;  (** version of vorbis codec, must be 0 *)
  audio_channels : int;  (** number of audio channels *)
  audio_samplerate : int;  (** samplerate in Hertz *)
  bitrate_upper : int;
  bitrate_nominal : int;
  bitrate_lower : int;
  bitrate_window : int;
}

(** Create a list of vorbis tags. *)
val tags : (string, string) Hashtbl.t -> unit -> (string * string) list

(** {1 Operations with vorbis streams} *)

(** {2 Encoding} *)

module Encoder : sig
  (** Internal state of an encoder. *)
  type t

  (** [create chans rate max_br nom_br min_br] creates a new encoder with *
      [chans] channels, with sample rate [rate] Hz and with respectively
      [max_br], * [nom_br] and [min_br] as maximal, nominal and minimal bitrates
      (in bps). *)
  val create : int -> int -> int -> int -> int -> t

  (** [create_vbr chans rate quality] creates a new encoder in variable bitrate
      * with [chans] channels, with sample rate [rate] Hz and with quality *
      [quality], which should be between -1 and 1 (1 is the best). *)
  val create_vbr : int -> int -> float -> t

  val reset : t -> unit

  (** Encode a header given a list of tags. *)
  val headerout :
    ?encoder:string -> t -> Ogg.Stream.stream -> (string * string) list -> unit

  (** Encoder a header, but do not submit packet to * Ogg Stream. Useful when
      multiplexing ogg streams * since the all first packets of each streams
      must be packed * in the initial pages. *)
  val headerout_packetout :
    ?encoder:string ->
    t ->
    (string * string) list ->
    Ogg.Stream.packet * Ogg.Stream.packet * Ogg.Stream.packet

  (** Get the number of audio channels expected by * the encoder. *)
  val get_channels : t -> int

  (** Encode a buffer of PCM data. * The PCM data array must have at least the
      expected * number of channels. Otherwise, the function raises
      [Invalid_channels]. *)
  val encode_buffer_float :
    t -> Ogg.Stream.stream -> float array array -> int -> int -> unit

  val encode_buffer_float_ba :
    t ->
    Ogg.Stream.stream ->
    (float, Bigarray.float32_elt, Bigarray.c_layout) Bigarray.Array1.t array ->
    int ->
    int ->
    unit

  (** Convert a granulepos to absolute time in seconds. The granulepos is *
      interpreted in the context of a given encoder, and gives * the end time of
      a frame's presentation as used in Ogg mux ordering. *)
  val time_of_granulepos : t -> Int64.t -> Nativeint.t

  val end_of_stream : t -> Ogg.Stream.stream -> unit
end

(** {2 Decoding} *)

module Decoder : sig
  (** Internal decoder state *)
  type t

  (** Initialize decoder. Needs the first 3 packets of the ogg logical * stream.
      Use [check_packet] to check against the first one. *)
  val init : Ogg.Stream.packet -> Ogg.Stream.packet -> Ogg.Stream.packet -> t

  (** Get vorbis infos from the decoder *)
  val info : t -> info

  (** Get vorbis comments from the decoder *)
  val comments : t -> string * (string * string) list

  (** Check whether a ogg packet contains vorbis data. * Useful for parsing ogg
      containers with multiple streams. *)
  val check_packet : Ogg.Stream.packet -> bool

  (** [decode_pcm dec stream buffer pos offset] decodes pcm float data * from
      [stream]. The floats are written in [buffer], starting at * position
      [pos]. The function returns the number of samples actually written.*)
  val decode_pcm :
    t -> Ogg.Stream.stream -> float array array -> int -> int -> int

  (** [decode_pcm_ba dec stream buffer pos offset] decodes pcm float data * from
      [stream]. The floats are written in [buffer], starting at * position
      [pos]. The function returns the number of samples actually written.*)
  val decode_pcm_ba :
    t ->
    Ogg.Stream.stream ->
    (float, Bigarray.float32_elt, Bigarray.c_layout) Bigarray.Array1.t array ->
    int ->
    int ->
    int

  (** Restart the decoder *)
  val restart : t -> unit
end

(** {1 Operations with vorbis files} *)

(** {2 Decoding} *)

module File : sig
  module Decoder : sig
    (** Internal state of a decoder. *)
    type t

    (** [create read_func seek_func tell_func params] opens a * stream like
        [openfile] for decoding but callbacks are used to * manipulate the data.
        [read_func] should return the requested amount of bytes * (or less if it
        is the end of file), [seek_funk] should return 0 if the seek * was ok or
        -1 if the stream is not seekable, [tell_func] should return the current
        * offset or -1 if there is no notion of offset in the stream. * Raises:
        [Read_error], [Not_vorbis], [Version_mismatch], [Bad_header],
        [Internal_fault]. *)
    val create :
      (int -> string * int) ->
      (int -> Unix.seek_command -> int) ->
      (unit -> int) ->
      t

    (** Open a vorbis file for decoding. *)
    val openfile : string -> t * Unix.file_descr

    val openfile_with_fd : Unix.file_descr -> t

    (** [decode_float dec buff ofs len] decodes [len] samples in each channel and puts
      * the result in [buff] starting at position [ofs].
      * @raise Hole_in_data if there was an interruption of the data.
      * @raise Invalid_parameters if all the data cannot fit in the buffer starting at the given position.
      *)
    val decode_float : t -> float array array -> int -> int -> int

    val decode_float_alloc : t -> int -> float array array

    (** [decode_float_ba dec buff ofs len] decodes [len] samples in each channel and puts
      * the result in [buff] starting at position [ofs].
      * @raise Hole_in_data if there was an interruption of the data.
      * @raise Invalid_parameters if all the data cannot fit in the buffer starting at the given position.
      *)
    val decode_float_ba :
      t ->
      (float, Bigarray.float32_elt, Bigarray.c_layout) Bigarray.Array1.t array ->
      int ->
      int ->
      int

    val decode_float_alloc_ba :
      t ->
      int ->
      (float, Bigarray.float32_elt, Bigarray.c_layout) Bigarray.Array1.t array

    (** Same as [decode_float] but decodes to integers. *)
    val decode :
      t ->
      ?big_endian:bool ->
      ?sample_size:int ->
      ?signed:bool ->
      bytes ->
      int ->
      int ->
      int

    (** Get the number of logical bitstreams within a physical bitstream. *)
    val streams : t -> int

    (** Get the index of the sequential logical bitstream currently being
        decoded * (incremented at chaining boundaries even for non-seekable
        streams). For * seekable streams, it represents the actual chaining
        index within the * physical bitstream. *)
    val bitstream : t -> bitstream

    (** Get the vorbis comments from a vorbis file. The second argument is the *
        number of the logical bitstream (the current bitstream is used if it is
        set * to [None]). *)
    val comments : t -> bitstream -> string * (string * string) list

    (** Get the vorbis information from the stream header of a bitstream. *)
    val info : t -> bitstream -> info

    (** Get the bitrate of a bitsream (in bps). *)
    val bitrate : t -> bitstream -> int

    (** Get the total pcm samples of a bitstream. *)
    val samples : t -> bitstream -> int

    (** Get the duration in seconds of a bitstream. *)
    val duration : t -> bitstream -> float

    (** Get the serial number of a bitstream. *)
    val serialnumber : t -> bitstream -> int
  end
end

module Skeleton : sig
  (** Generate a vorbis fisbone packet with * these parameters, to use in an ogg
      skeleton. * Default value for [start_granule] is [Int64.zero], * Default
      value for [headers] is ["Content-type","audio/vorbis"] * * See:
      http://xiph.org/ogg/doc/skeleton.html. *)
  val fisbone :
    ?start_granule:Int64.t ->
    ?headers:(string * string) list ->
    serialno:Nativeint.t ->
    samplerate:Int64.t ->
    unit ->
    Ogg.Stream.packet
end
