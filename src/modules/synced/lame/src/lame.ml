(*
 * Copyright 2005-2006 Savonet team
 *
 * This file is part of ocaml-lame.
 *
 * ocaml-lame is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * ocaml-lame is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with ocaml-lame; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *)

exception Init_params_failed
exception Init_params_not_called
exception Psychoacoustic_problem
exception Unknown_error of int

let _ =
  Callback.register_exception "lame_exn_init_params_failed" Init_params_failed;
  Callback.register_exception "lame_exn_init_params_not_called"
    Init_params_not_called;
  Callback.register_exception "lame_exn_psychoacoustic_problem"
    Psychoacoustic_problem;
  Callback.register_exception "lame_exn_unknown_error" (Unknown_error 0)

type encoder

external create_encoder : unit -> encoder = "ocaml_lame_init"
external frame_size : encoder -> int = "ocaml_lame_frame_size"

external set_in_samplerate : encoder -> int -> unit
  = "ocaml_lame_set_in_samplerate"

external set_num_channels : encoder -> int -> unit
  = "ocaml_lame_set_num_channels"

external set_out_samplerate : encoder -> int -> unit
  = "ocaml_lame_set_out_samplerate"

external set_disable_reservoir : encoder -> bool -> unit
  = "ocaml_lame_set_disable_reservoir"

external set_quality : encoder -> int -> unit = "ocaml_lame_set_quality"

let set_quality e q =
  if q < 0 || q > 9 then invalid_arg "quality";
  set_quality e q

type mode = Stereo | Joint_stereo | Dual_channel | Mono

external set_mode : encoder -> mode -> unit = "ocaml_lame_set_mode"

external set_bWriteVbrTag : encoder -> bool -> unit
  = "ocaml_lame_set_bWriteVbrTag"

type vbr_mode = Vbr_off | Vbr_rh | Vbr_abr | Vbr_mtrh | Vbr_max_indicator

external set_vbr_mode : encoder -> vbr_mode -> unit = "ocaml_lame_set_vbr"
external set_vbr_quality : encoder -> int -> unit = "ocaml_lame_set_VBR_q"

external set_vbr_mean_bitrate : encoder -> int -> unit
  = "ocaml_lame_set_VBR_mean_bitrate_kbps"

external set_vbr_min_bitrate : encoder -> int -> unit
  = "ocaml_lame_set_VBR_min_bitrate_kbps"

external set_vbr_max_bitrate : encoder -> int -> unit
  = "ocaml_lame_set_VBR_max_bitrate_kbps"

external set_vbr_hard_min : encoder -> bool -> unit
  = "ocaml_lame_set_VBR_hard_min"

external set_brate : encoder -> int -> unit = "ocaml_lame_set_brate"

external set_compression_ratio : encoder -> float -> unit
  = "ocaml_lame_set_compression_ratio"

external set_private : encoder -> bool -> unit = "ocaml_lame_set_extension"
external get_private : encoder -> bool = "ocaml_lame_get_extension"
external set_original : encoder -> bool -> unit = "ocaml_lame_set_original"
external get_original : encoder -> bool = "ocaml_lame_get_original"
external set_copyright : encoder -> bool -> unit = "ocaml_lame_set_copyright"
external get_copyright : encoder -> bool = "ocaml_lame_get_copyright"
external init_params : encoder -> unit = "ocaml_lame_init_params"
external init_bitstream : encoder -> unit = "ocaml_lame_init_bitstream"

external encode_buffer_part : encoder -> string -> int -> int -> string
  = "ocaml_lame_encode_buffer_interleaved"

(** [encode_buffer_float_part enc left right offset samples] encodes float
    samples, expected to be in [-1;1]. *)
external encode_buffer_float_part :
  encoder -> float array -> float array -> int -> int -> string
  = "ocaml_lame_encode_buffer_float"

(** Encode left and right buffers. Data are supposed to be scaled -/+32768. *)
external encode_buffer_float_ba :
  encoder ->
  (float, Bigarray.float32_elt, Bigarray.c_layout) Bigarray.Array1.t ->
  (float, Bigarray.float32_elt, Bigarray.c_layout) Bigarray.Array1.t ->
  string = "ocaml_lame_encode_buffer_float_ba"

(** [encode_buffer enc buf smpl] encodes [smpl] samples of PCM audio into MP3.
    Input samples are expected to be 16bits little-endian, other input/output
    params are specified using [set_*] functions. *)
let encode_buffer enc buf smpl = encode_buffer_part enc buf 0 smpl

let encode_buffer_float enc left right smpl =
  encode_buffer_float_part enc left right 0 smpl

external encode_flush : encoder -> string = "ocaml_lame_encode_flush"

external encode_flush_nogap : encoder -> string
  = "ocaml_lame_encode_flush_nogap"

module Id3tag = struct
  external init : encoder -> unit = "ocaml_lame_id3tag_init"
  external set_title : encoder -> string -> unit = "ocaml_lame_id3tag_set_title"

  external set_artist : encoder -> string -> unit
    = "ocaml_lame_id3tag_set_artist"

  external set_album : encoder -> string -> unit = "ocaml_lame_id3tag_set_album"
  external set_year : encoder -> string -> unit = "ocaml_lame_id3tag_set_year"

  external set_comment : encoder -> string -> unit
    = "ocaml_lame_id3tag_set_comment"

  external set_track : encoder -> string -> unit = "ocaml_lame_id3tag_set_track"
  external set_genre : encoder -> string -> unit = "ocaml_lame_id3tag_set_genre"
end

external get_lame_version : unit -> string = "ocaml_lame_get_lame_version"

external get_lame_short_version : unit -> string
  = "ocaml_lame_get_lame_short_version"

external get_lame_very_short_version : unit -> string
  = "ocaml_lame_get_lame_very_short_version"

external get_psy_version : unit -> string = "ocaml_lame_get_psy_version"
external get_lame_url : unit -> string = "ocaml_lame_get_lame_url"

type mpeg_version = Mpeg_1 | Mpeg_2 | Mpeg_2_5

external get_version : encoder -> mpeg_version = "ocaml_lame_get_version"
external get_encoder_delay : encoder -> int = "ocaml_lame_get_encoder_delay"
external get_framesize : encoder -> int = "ocaml_lame_get_framesize"

external get_nb_samples_to_encode : encoder -> int
  = "ocaml_lame_get_mf_samples_to_encode"

external get_nb_encoded_frames : encoder -> int = "ocaml_lame_get_frameNum"
external get_nb_frames : encoder -> int = "ocaml_lame_get_totalframes"
