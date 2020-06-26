(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2019 Savonet team

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

(** Generic content registration API. *)

(* Raised during any invalid operation below. *)
exception Invalid

module Contents : sig
  type format
  type params
  type data
end

module type ContentSpecs = sig
  type format
  type param
  type data

  (** Data *)

  val make : param list -> data
  val blit : data -> int -> data -> int -> int -> unit
  val bytes : data -> int
  val copy : data -> data

  (** Params *)

  val params : data -> param list
  val merge : param list -> param list -> param list
  val string_of_param : param -> string

  (* Format: [param_of_string "label" "value"] *)
  val param_of_string : string -> string -> param option

  (** Format *)

  val format : format
  val default_params : format -> param list
  val string_of_format : format -> string
  val format_of_string : string -> format option
end

module type Content = sig
  include ContentSpecs

  (** Data *)

  val is_data : Contents.data -> bool
  val lift_data : data -> Contents.data
  val get_data : Contents.data -> data

  (** Params *)

  val is_params : Contents.params -> bool
  val lift_params : param list -> Contents.params
  val get_params : Contents.params -> param list

  (** Format *)

  val is_format : Contents.format -> bool
  val lift_format : format -> Contents.format
  val get_format : Contents.format -> format
end

module MkContent (C : ContentSpecs) :
  Content
    with type format = C.format
     and type param = C.param
     and type data = C.data

type params = Contents.params
type data = Contents.data
type format = Contents.format

(** Data *)

val make : params -> data
val blit : data -> int -> data -> int -> int -> unit
val bytes : data -> int
val copy : data -> data

(** Params *)

val params : data -> params
val merge : params -> params -> unit
val string_of_params : params -> string

(* Format: [params_of_string "label" "value"] *)
val params_of_string : string -> string -> params

(** Format *)

val format : params -> format
val default_params : format -> params
val string_of_format : format -> string
val format_of_string : string -> format

(** Internal content types. *)

(* None content type is abstract and only used
   via its params and data. *)
module None : sig
  val data : Contents.data
  val params : Contents.params
  val is_params : Contents.params -> bool
end

module Audio : sig
  include
    Content
      with type format = [ `Pcm ]
       and type param = [ `Mono | `Stereo | `Five_point_one ]
       and type data = Audio.Mono.buffer array

  val channels_of_params : Contents.params -> int
  val params_of_channels : int -> Contents.params
end

module Video : Content with type format = [ `Yuv420p ] and type data = Video.t

module Midi :
  Content
    with type format = [ `Midi ]
     and type param = [ `Channels of int ]
     and type data = MIDI.Multitrack.buffer

val default_audio : unit -> Contents.params
val default_video : unit -> Contents.params
val default_midi : unit -> Contents.params
val is_internal : format -> bool
