(*
 * Copyright 2007-2009 Samuel Mimram, Romain Beauxis
 *
 * This file is part of ocaml-theora.
 *
 * ocaml-theora is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * ocaml-theora is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with ocaml-theora; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *)

(**
  * Functions for encoding theora files using libtheora.
  *
  * @author Samuel Mimram
  * @author Romain Beauxis
  *)

(** {2 Exceptions} *)

(** General failure. *)
exception Internal_error

(** Library encountered invalid internal data. *)
exception Invalid_data

(** An unhandled error happened. *)
exception Unknown_error of int

(** The decoded packet represented a dropped frame. * The player can continue to
    display the current frame, * as the contents of the decoded frame buffer
    have not * changed. *)
exception Duplicate_frame

(** Exceptions used by the decoding module. *)
exception Done

exception Not_initialized
exception Bad_packet
exception Header_not_theora
exception Bad_header
exception Not_implemented

(** {2 General functions} *)

(** * Human-readable string to identify the encoder vendor and version. *)
val version_string : string

(** major, minor and sub version numbers of the encoder. *)
val version_number : int * int * int

(** Determines whether a theora packet is a key frame or not. * * raises
    [Invalid_data] if The packet is not a video data * packet. *)
val is_keyframe : Ogg.Stream.packet -> bool

(** {2 Types and datastructures} *)

(** A Colorspace. *)
type colorspace =
  | CS_unspecified  (** The colorspace is unknown or unspecified *)
  | CS_ITU_REC_470M  (** This is the best option for 'NTSC' content *)
  | CS_ITU_REC_470BG  (** This is the best option for 'PAL' content *)
  | CS_NSPACES  (** This marks the end of the defined colorspaces *)

(** * A Chroma subsampling * * These enumerate the available chroma subsampling
    options supported * by the theora format. See Section 4.4 of the
    specification for * exact definitions. *)
type pixelformat =
  | PF_420  (** Chroma subsampling by 2 in each direction (4:2:0) *)
  | PF_reserved  (** Reserved value *)
  | PF_422  (** Horizonatal chroma subsampling by 2 (4:2:2) *)
  | PF_444  (** No chroma subsampling at all (4:4:4) *)

(** Theora bitstream info. *)
type info = {
  frame_width : int;  (** The encoded frame width. *)
  frame_height : int;  (** The encoded frame height. *)
  picture_width : int;  (** The displayed picture width. *)
  picture_height : int;  (** The displayed picture height. *)
  picture_x : int;  (** The X offset of the displayed picture. *)
  picture_y : int;  (** The Y offset of the displayed picture. *)
  colorspace : colorspace;  (** The color space. *)
  pixel_fmt : pixelformat;  (** The pixel format. *)
  target_bitrate : int;  (** The target bit-rate in bits per second. *)
  quality : int;  (** The target quality level. *)
  keyframe_granule_shift : int;
      (** The amount to shift to extract the last keyframe number from the
          granule position. *)
  version_major : int;
  version_minor : int;
  version_subminor : int;
  fps_numerator : int;
  fps_denominator : int;
  aspect_numerator : int;
  aspect_denominator : int;
}

val default_granule_shift : int

type data_buffer =
  (int, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

(** * A YUV buffer for passing uncompressed frames to and from the codec. * This
    holds a Y'CbCr frame in planar format. The CbCr planes can be * subsampled
    and have their own separate dimensions and row stride * offsets. Note that
    the strides may be negative in some * configurations. For theora the width
    and height of the largest plane * must be a multiple of 16. The actual
    meaningful picture size and * offset are stored in the [info] structure;
    frames returned by * the decoder may need to be cropped for display. * * All
    samples are 8 bits. Within each plane samples are ordered by * row from the
    top of the frame to the bottom. Within each row samples * are ordered from
    left to right. *)
type yuv_buffer = {
  y_width : int;
  y_height : int;
  y_stride : int;
  y : data_buffer;
  u_width : int;
  u_height : int;
  u_stride : int;
  u : data_buffer;
  v_width : int;
  v_height : int;
  v_stride : int;
  v : data_buffer;
}

(** {2 Encoding} *)

module Encoder : sig
  type t

  type settings = {
    keyframe_frequency : int option;
    vp3_compatible : bool option;
    soft_target : bool option;
    buffer_delay : int option;
    speed : int option;
  }

  (** Initialize a [state] handle for decoding. *)
  val create : info -> settings -> (string * string) list -> t

  (** * Fills the given stream with the header packets. *)
  val encode_header : t -> Ogg.Stream.stream -> unit

  (** * Encode data until a page is filled. *)
  val encode_page : t -> Ogg.Stream.stream -> (unit -> yuv_buffer) -> Ogg.Page.t

  (** Encode a buffer. *)
  val encode_buffer : t -> Ogg.Stream.stream -> yuv_buffer -> unit

  (** Convert a granulepos to an absolute frame index, starting at 0. * The
      granulepos is interpreted in the context of a given theora_state handle.
  *)
  val frames_of_granulepos : t -> Int64.t -> Int64.t

  (** Set end of stream *)
  val eos : t -> Ogg.Stream.stream -> unit
  [@@alert
    deprecated
      "This function generates invalid bitstream. Please use \
       Ogg.Stream.terminate instead!"]
end

module Decoder : sig
  (** Type for an uninitialized decoder. *)
  type decoder

  (** Type for an initialized decoder. *)
  type t

  (** * Check whether an ogg logical stream contains theora data * * This
      function shall be called just after you put * the first page in the
      stream. See examples/thdecode.ml * * Raises [Ogg.Bad_data] if the stream
      does not contain theora data. *)
  val check : Ogg.Stream.packet -> bool

  (** Initialize the decoding structure. * The decoder should then be processed
      with [headerin]. *)
  val create : unit -> decoder

  (** Add one packet from the stream and try to parse theora headers. * *
      Returns an initialized decoder. * * Raises [Ogg.Not_enough_data] is
      decoding header needs another packet. * * This function should be called
      with the first packets of the stream * until it returns the requested
      values. It may consume at most 5 packets * (3 header packet, 1 additional
      packet and the initial video packet) *)
  val headerin :
    decoder -> Ogg.Stream.packet -> t * info * string * (string * string) list

  (** * Output the next available frame of decoded YUV data. * * Raises
      [Ogg.Not_enough_data] if the Ogg.Stream.stream which * has been used to
      initialize the handler does not contain * enough data. You should submit a
      new page to it, and * run this function again until it returns. * * Raises
      [Not_initialized] if the decoder was not properly * initialized with
      [headerin]. *)
  val get_yuv : t -> Ogg.Stream.stream -> yuv_buffer

  (** Convert a granulepos to an absolute frame index, starting at 0. * The
      granulepos is interpreted in the context of a given theora_state handle.
  *)
  val frames_of_granulepos : t -> Int64.t -> Int64.t
end

module Skeleton : sig
  (** Generate a theora fisbone packet with * these parameters, to use in an ogg
      skeleton. * Default value for [start_granule] is [Int64.zero], * Default
      value for [headers] is ["Content-type","video/theora"] * * See:
      http://xiph.org/ogg/doc/skeleton.html. *)
  val fisbone :
    ?start_granule:Int64.t ->
    ?headers:(string * string) list ->
    serialno:Nativeint.t ->
    info:info ->
    unit ->
    Ogg.Stream.packet
end
