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

(**
  * Bindings to the lame library to encode mp3 files.
  *
  * @author Samuel Mimram
  *)

(** {1 General information} *)

(** Get the lame version number. *)
val get_lame_version : unit -> string

(** Get the lame version number (shorter than [get_lame_version]). *)
val get_lame_short_version : unit -> string

(** Get the lame version number (shorter than [get_lame_short_version]). *)
val get_lame_very_short_version : unit -> string

(** Get the url of the lame website. *)
val get_lame_url : unit -> string

(** Get the version of the psy model. *)
val get_psy_version : unit -> string

(** {1 Encoders} *)

(** Type for lame encoders. *)
type encoder

(** Create a new lame encoder. *)
val create_encoder : unit -> encoder

(** Frame size. All audio frames submitted to the encoder * should have this
    number of samples per channel. *)
val frame_size : encoder -> int

(** {2 Parameters} *)

(** Input sample rate in Hz (default: 44100). *)
val set_in_samplerate : encoder -> int -> unit

(** Number of channels in input stream (default: 2). *)
val set_num_channels : encoder -> int -> unit

(** * Output sample rate in Hz (default: 0, which means LAME picks best value *
    based on the amount of compression). MPEG only allows: * - MPEG1 (32, 44.1,
    48 kHz) * - MPEG2 (16, 22.05, 24 kHz) * - MPEG2.5 (8, 11.025, 12 kHz) *)
val set_out_samplerate : encoder -> int -> unit

(** * Internal algorithm selection. True quality is determined by the bitrate *
    but this variable will effect quality by selecting expensive or cheap *
    algorithms. The quality is an integer between 0 and 9, where 0=best (very *
    slow) and 9=worst. More precisely: * - 2: near-best quality, not too slow
    (recommended) * - 5: good quality, fast * - 7: ok quality, really fast *)
val set_quality : encoder -> int -> unit

(* Enable/disable the bit reservoir. *)
val set_disable_reservoir : encoder -> bool -> unit

(** Compression mode. *)
type mode =
  | Stereo  (** stereo, channels encoded independely *)
  | Joint_stereo  (** stereo, channels encoded together *)
  | Dual_channel  (** not supported *)
  | Mono  (** mono *)

(** Set the compression mode. *)
val set_mode : encoder -> mode -> unit

(** VBR (Variable BitRate) mode. *)
type vbr_mode =
  | Vbr_off  (** constant bitrate *)
  | Vbr_rh
  | Vbr_abr
  | Vbr_mtrh
  | Vbr_max_indicator

(** Write a Xing VBR header frame. (default: {true}) *)
val set_bWriteVbrTag : encoder -> bool -> unit

(** Set the VBR mode. *)
val set_vbr_mode : encoder -> vbr_mode -> unit

(** Set the VBR quality level (0:highest, 9: lowest). *)
val set_vbr_quality : encoder -> int -> unit

(** Set the VBR mean birate in kbps. Only used by [Vbr_abr] mode. *)
val set_vbr_mean_bitrate : encoder -> int -> unit

(** Set the minimal VBR bitrate in kbps. *)
val set_vbr_min_bitrate : encoder -> int -> unit

(** Set the maximal VBR bitrate in kbps. *)
val set_vbr_max_bitrate : encoder -> int -> unit

(** If [true], enforce the minimal VBR bitrate. Normally it will be violated *
    for analog silence. *)
val set_vbr_hard_min : encoder -> bool -> unit

(** Set the bitrate of compressed data. You can either set this or use *
    [set_compression_ratio]. *)
val set_brate : encoder -> int -> unit

(** Set the compression ratio (default: 11). *)
val set_compression_ratio : encoder -> float -> unit

(** Get/set private bit. *)
val set_private : encoder -> bool -> unit

val get_private : encoder -> bool

(** Get/set copyright bit. *)
val set_copyright : encoder -> bool -> unit

val get_copyright : encoder -> bool

(** Get/set original bit. *)
val set_original : encoder -> bool -> unit

val get_original : encoder -> bool

(** A call to [init_params] failed for some reason. *)
exception Init_params_failed

(** Initialize lame's parameters. Should be called before encoding (and after *
    having set the parameters). Raises [Init_params_failed] on error. *)
val init_params : encoder -> unit

(** Write id3v2 and Xing headers into the front of the bitstream, and set *
    frame counters and bitrate histogram data to 0. Normally, this is called by
    * [init_params]. You can call this after [encode_flush_nogap]. *)
val init_bitstream : encoder -> unit

(** {2 Encoding} *)

(** You should have called [init_params]. *)
exception Init_params_not_called

(** A problem occurred with psychoacoustics. *)
exception Psychoacoustic_problem

(** This should not have happened. Please report. *)
exception Unknown_error of int

(** [encode_buffer_part enc buf ofs smpl] encodes [samples] * samples (per
    channel) of [buf] starting at position [ofs]. *)
val encode_buffer_part : encoder -> string -> int -> int -> string

(** Same as [encode_buffer_part] but with [ofs = 0]. *)
val encode_buffer : encoder -> string -> int -> string

(** [encode_buffer_float_part enc left right ofs smpl] encodes [samples] *
    samples of [left] and [right] channels starting at position [ofs]. * Floats
    are expected to range over [-1;1]. *)
val encode_buffer_float_part :
  encoder -> float array -> float array -> int -> int -> string

(** Same as [encode_buffer_float_part] but with [ofs = 0]. *)
val encode_buffer_float : encoder -> float array -> float array -> int -> string

(** Encode a buffer of samples. The samples follow here lame's convention of
    having floats in the range [-32768.,32768.] (yes, this is crazy but that's
    the way things are). *)
val encode_buffer_float_ba :
  encoder ->
  (float, Bigarray.float32_elt, Bigarray.c_layout) Bigarray.Array1.t ->
  (float, Bigarray.float32_elt, Bigarray.c_layout) Bigarray.Array1.t ->
  string

(** Flush the PCM buffers, padding with zeros if needed to make a complete *
    frame. Will also write id3v1 tags (if any) into the bitstream. *)
val encode_flush : encoder -> string

(** Flush the PCM buffers, padding with zeros if needed to make a complete *
    frame. This function will not write id3v1 tags into the bitstream. *)
val encode_flush_nogap : encoder -> string

(** {2 Id3 tags} *)

module Id3tag : sig
  (** This function should be called before any other in the [Id3tag] module. *)
  val init : encoder -> unit

  (** Set the title. *)
  val set_title : encoder -> string -> unit

  (** Set the artist. *)
  val set_artist : encoder -> string -> unit

  (** Set the album. *)
  val set_album : encoder -> string -> unit

  (** Set the year. *)
  val set_year : encoder -> string -> unit

  (** Set the comment. *)
  val set_comment : encoder -> string -> unit

  (** Set the track number. *)
  val set_track : encoder -> string -> unit

  (** Set the genre. *)
  val set_genre : encoder -> string -> unit
end

(** {2 Information about encoding} *)

(** MPEG version. *)
type mpeg_version =
  | Mpeg_1  (** MPEG v1 *)
  | Mpeg_2  (** MPEG v2 *)
  | Mpeg_2_5  (** MPEG v2.5 *)

(** The MPEG version used by the encoder. *)
val get_version : encoder -> mpeg_version

(** Get the encoder delay. *)
val get_encoder_delay : encoder -> int

(** Size of an mpeg frame in bytes. *)
val get_framesize : encoder -> int

(** Number of PCM samples buffered, but not yet encoded to mp3 data. *)
val get_nb_samples_to_encode : encoder -> int

(** Number of frames encoded so far. *)
val get_nb_encoded_frames : encoder -> int

(** Estimate of the total number of frames to be encoded * (only valid if
    [set_nb_samples] was called). *)
val get_nb_frames : encoder -> int
