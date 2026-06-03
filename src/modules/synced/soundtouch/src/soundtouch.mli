(*
 * Copyright 2007-2011 Samuel Mimram
 *
 * This file is part of ocaml-soundtouch.
 *
 * ocaml-soundtouch is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * ocaml-soundtouch is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with ocaml-soundtouch; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 * As a special exception to the GNU Library General Public License, you may
 * link, statically or dynamically, a "work that uses the Library" with a publicly
 * distributed version of the Library to produce an executable file containing
 * portions of the Library, and distribute that executable file under terms of
 * your choice, without any of the additional requirements listed in clause 6
 * of the GNU Library General Public License.
 * By "a publicly distributed version of the Library", we mean either the unmodified
 * Library as distributed by The Savonet Team, or a modified version of the Library that is
 * distributed under the conditions defined in clause 3 of the GNU Library General
 * Public License. This exception does not however invalidate any other reasons why
 * the executable file might be covered by the GNU Library General Public License.
 *)

(** Soundtouch is a library to change tempo or pitch of sound. *)

(** {2 Initialization} *)

(** A converter. *)
type t

(*
(** Create a new soundtouch converter. *)
val create : unit -> t

(** Indicate the number of audio channels. *)
val set_channels : t -> int -> unit = "ocaml_st_set_channels"

(** Indicate the samplerate of audio data. *)
val set_samplerate : t -> int -> unit = "ocaml_st_set_samplerate"
*)

(** Create a new soundtouch converte with given number of channels and
    samplerate. *)
val make : int -> int -> t

(** Soundtouch library version. *)
val get_version_string : t -> string

(** Soundtouch library version identifier. *)
val get_version_id : t -> int

(** {2 Sound parameters} *)

(** Set playing rate (default is [1.], smaller means slower). *)
val set_rate : t -> float -> unit

(** Set tempo (default [1.], smaller means slower). *)
val set_tempo : t -> float -> unit

(** Set pitch (default [1.], smaller means lower). *)
val set_pitch : t -> float -> unit

(** {2 Sound manipulation} *)

(** Put samples. Data is interleaved with given number of channels. *)
val put_samples_ba :
  t ->
  (float, Bigarray.float32_elt, Bigarray.c_layout) Bigarray.Array1.t ->
  unit

(** Put samples (in a non-interleaved format) with given offset in array and
    number of samples. *)
val put_samples_ni : t -> float array array -> int -> int -> unit

(** Number of available output samples. *)
val get_available_samples : t -> int

(** Retrieve samples. *)
val get_samples_ba :
  t -> (float, Bigarray.float32_elt, Bigarray.c_layout) Bigarray.Array1.t -> int

(** Retrieve samples (in a non-interleaved format). *)
val get_samples_ni : t -> float array array -> int -> int -> int

(** Flush the last samples from the processing pipeline to the output. *)
val flush : t -> unit

(** Clear all samples in output and internal processing buffers. *)
val clear : t -> unit

(** Tempo (beats-per-minute) detection. *)
module BPM : sig
  (** A beat detector. *)
  type t

  (** Create a new beat detector with given number of channels and samplerate.
  *)
  val make : int -> int -> t

  (** Put samples in the detector. *)
  val put_samples_ba :
    t ->
    (float, Bigarray.float32_elt, Bigarray.c_layout) Bigarray.Array1.t ->
    unit

  (** Put samples in the detector. *)
  val put_samples_ni : t -> float array array -> int -> int -> unit

  (** Analyze samples and estimate tempo (in beats per minute). *)
  val get_bpm : t -> float
end
