(*
 * Copyright 2007-2011 Savonet team
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

exception Internal_error
exception Invalid_data
exception Bad_packet
exception Header_not_theora
exception Bad_header
exception Not_implemented
exception Bitstream_version_too_high
exception Unknown_error of int
exception Duplicate_frame
exception Done
exception Not_initialized

let () =
  Callback.register_exception "theora_exn_fault" Internal_error;
  Callback.register_exception "theora_exn_version" Bitstream_version_too_high;
  Callback.register_exception "theora_exn_bad_packet" Bad_packet;
  Callback.register_exception "theora_exn_notformat" Header_not_theora;
  Callback.register_exception "theora_exn_bad_header" Bad_header;
  Callback.register_exception "theora_exn_not_implemented" Not_implemented;
  Callback.register_exception "theora_exn_inval" Invalid_data;
  Callback.register_exception "theora_exn_unknown" (Unknown_error 0);
  Callback.register_exception "theora_exn_dup_frame" Duplicate_frame;
  Callback.register_exception "theora_exn_not_enough_data" Ogg.Not_enough_data;
  Callback.register_exception "theora_exn_end_of_file" End_of_file

external version_string : unit -> string = "ocaml_theora_version_string"

let version_string = version_string ()

external version_number : unit -> int = "ocaml_theora_version_number"

let version_number =
  let n = version_number () in
  (n lsr 16, (n lsr 8) land 0xff, n land 0xff)

type colorspace =
  | CS_unspecified
  | CS_ITU_REC_470M
  | CS_ITU_REC_470BG
  | CS_NSPACES

type pixelformat = PF_420 | PF_reserved | PF_422 | PF_444

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

external default_granule_shift : unit -> int
  = "ocaml_theora_default_granuleshift"

let default_granule_shift = default_granule_shift ()

type data_buffer =
  (int, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

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

let encoder_tag = "ocaml-theora by the savonet team (http://savonet.sf.net/)"

external is_keyframe : Ogg.Stream.packet -> int
  = "ocaml_theora_ogg_packet_iskeyframe"

let is_keyframe op =
  match is_keyframe op with 1 -> true | 0 -> false | _ -> raise Invalid_data

module Encoder = struct
  type t

  type settings = {
    keyframe_frequency : int option;
    vp3_compatible : bool option;
    soft_target : bool option;
    buffer_delay : int option;
    speed : int option;
  }

  external create : info -> settings -> (string * string) array -> t
    = "ocaml_theora_encode_init"

  let create info params comments =
    let comments = ("ENCODER", encoder_tag) :: comments in
    create info params (Array.of_list comments)

  external encode_header : t -> Ogg.Stream.stream -> bool
    = "ocaml_theora_encode_header"

  let encode_header enc os =
    let rec f () = if not (encode_header enc os) then f () in
    f ()

  external encode_buffer : t -> Ogg.Stream.stream -> yuv_buffer -> unit
    = "ocaml_theora_encode_buffer"

  let encode_page enc os generator =
    let rec f () =
      try
        let yuv = generator () in
        encode_buffer enc os yuv;
        Ogg.Stream.get_page os
      with Ogg.Not_enough_data -> f ()
    in
    f ()

  external frames_of_granulepos : t -> Int64.t -> Int64.t
    = "ocaml_theora_encoder_frame_of_granulepos"

  external eos : t -> Ogg.Stream.stream -> unit = "ocaml_theora_encode_eos"
end

module Decoder = struct
  type decoder
  type t

  external check : Ogg.Stream.packet -> bool = "caml_theora_check"
  external create : unit -> decoder = "ocaml_theora_create_dec"

  external headerin : decoder -> Ogg.Stream.packet -> info * string array
    = "ocaml_theora_dec_headerin"

  let headerin dec p =
    let info, comments = headerin dec p in
    let vendor, comments =
      match Array.to_list comments with e :: l -> (e, l) | [] -> ("", [])
    in
    let split s =
      try
        let pos = String.index s '=' in
        (String.sub s 0 pos, String.sub s (pos + 1) (String.length s - pos - 1))
      with Not_found -> ("", s)
    in
    (Obj.magic dec, info, vendor, List.map split comments)

  external frames_of_granulepos : t -> Int64.t -> Int64.t
    = "ocaml_theora_decoder_frame_of_granulepos"

  external get_yuv : t -> Ogg.Stream.stream -> yuv_buffer
    = "ocaml_theora_decode_YUVout"
end

module Skeleton = struct
  external fisbone :
    Nativeint.t -> info -> Int64.t -> string -> Ogg.Stream.packet
    = "ocaml_theora_skeleton_fisbone"

  let fisbone ?(start_granule = Int64.zero)
      ?(headers = [("Content-type", "video/theora")]) ~serialno ~info () =
    let concat s (h, v) = Printf.sprintf "%s%s: %s\r\n" s h v in
    let s = List.fold_left concat "" headers in
    fisbone serialno info start_granule s
end
