(*
 * Copyright 2013 Savonet team
 *
 * This file is part of ocaml-fdkaac.
 *
 * ocaml-fdkaac is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * ocaml-fdkaac is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with ocaml-fdkaac; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *)

module Encoder : sig
  (** {1 AAC encoding module for OCaml} *)

  (** {2 Exceptions} *)

  exception Invalid_handle
  exception Unsupported_parameter
  exception Invalid_config
  exception Error of int
  exception End_of_file
  exception Unknown of int

  val string_of_exception : exn -> string option

  (** {2 Types} *)

  type t
  type mpeg2_aac = [ `AAC_LC | `HE_AAC | `HE_AAC_v2 ]
  type mpeg4_aac = [ mpeg2_aac | `AAC_LD | `AAC_ELD ]
  type aot = [ `Mpeg_4 of mpeg4_aac | `Mpeg_2 of mpeg2_aac ]

  (** VBR modes gives roughly the following bit rates per channel: * * VBR |
      kbps/channel | AOTs * ------------------------- * 1 | 20-32 | LC,HE,HEv2 *
      2 | 32-40 | LC,HE,HEv2 * 3 | 48-56 | LC,HE,HEv2 * 4 | 64-72 | LC * 5 |
      96-112 | LC * * HE bit rates will be much lower. *)
  type bitrate_mode = [ `Constant | `Full_bitreservoir | `Variable of int ]

  type transmux = [ `Raw | `Adif | `Adts | `Latm | `Latm_out_of_band | `Loas ]

  type param_name =
    [ `Aot
    | `Bitrate
    | `Bitrate_mode
    | `Samplerate
    | `Sbr_mode
    | `Granule_length
    | `Afterburner
    | `Bandwidth
    | `Transmux ]

  type param =
    [ `Aot of aot
    | `Bitrate of int
    | `Bitrate_mode of bitrate_mode
    | `Samplerate of int
    | `Sbr_mode of bool
    | `Granule_length of int
    | `Afterburner of bool
    | `Bandwidth of int
    | `Transmux of transmux ]

  (** {2 Functions} *)

  val create : int -> t
  val set : t -> param -> unit
  val get : t -> param_name -> param
  val encode : t -> string -> int -> int -> string
  val flush : t -> string
end
