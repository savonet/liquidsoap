(*
 * Copyright 2007 Samuel Mimram
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
 *
 *)

type t

external create : unit -> t = "ocaml_st_make"
external set_channels : t -> int -> unit = "ocaml_st_set_channels"
external set_samplerate : t -> int -> unit = "ocaml_st_set_samplerate"

let make chans rate =
  let st = create () in
  set_channels st chans;
  set_samplerate st rate;
  st

external get_version_string : t -> string = "ocaml_st_get_version_string"
external get_version_id : t -> int = "ocaml_st_get_version_id"
external set_rate : t -> float -> unit = "ocaml_st_set_rate"
external set_tempo : t -> float -> unit = "ocaml_st_set_tempo"
external set_pitch : t -> float -> unit = "ocaml_st_set_pitch"
external flush : t -> unit = "ocaml_st_flush"
external clear : t -> unit = "ocaml_st_clear"

external put_samples_ba :
  t ->
  (float, Bigarray.float32_elt, Bigarray.c_layout) Bigarray.Array1.t ->
  unit = "ocaml_st_putsamples_ba"

external put_samples_ni : t -> float array array -> int -> int -> unit
  = "ocaml_st_putsamples_ni"

external get_available_samples : t -> int = "ocaml_st_num_samples"

external get_samples_ba :
  t -> (float, Bigarray.float32_elt, Bigarray.c_layout) Bigarray.Array1.t -> int
  = "ocaml_st_receive_samples_ba"

external get_samples_ni : t -> float array array -> int -> int -> int
  = "ocaml_st_receive_samples_ni"

module BPM = struct
  type t

  external make : int -> int -> t = "ocaml_st_bpm_make"

  external put_samples_ba :
    t ->
    (float, Bigarray.float32_elt, Bigarray.c_layout) Bigarray.Array1.t ->
    unit = "ocaml_st_bpm_putsamples_ba"

  external put_samples_ni : t -> float array array -> int -> int -> unit
    = "ocaml_st_bpm_putsamples_ni"

  external get_bpm : t -> float = "ocaml_st_bpm_get_bpm"
end
