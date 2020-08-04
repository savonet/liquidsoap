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

module Contents : sig
  type kind
  type format
  type data
end

(* Raised during any invalid operation below. *)
exception Invalid

(* Raised when calling [merge] below. *)
exception Incompatible_format of Contents.format * Contents.format

module type ContentSpecs = sig
  type kind
  type param
  type data

  (** Data *)

  val make : param list -> data
  val blit : data -> int -> data -> int -> int -> unit
  val bytes : data -> int
  val copy : data -> data
  val clear : data -> unit

  (** Params *)

  val params : data -> param list
  val merge : param list -> param list -> param list
  val string_of_param : param -> string

  (* [param_of_string "label" "value"] *)
  val param_of_string : string -> string -> param option

  (** Kind *)

  val kind : kind
  val default_params : kind -> param list
  val string_of_kind : kind -> string
  val kind_of_string : string -> kind option
end

module type Content = sig
  include ContentSpecs

  (** Data *)

  val is_data : Contents.data -> bool
  val lift_data : data -> Contents.data
  val get_data : Contents.data -> data

  (** Format *)

  val is_format : Contents.format -> bool
  val lift_params : param list -> Contents.format
  val get_params : Contents.format -> param list

  (** Kind *)

  val is_kind : Contents.kind -> bool
  val lift_kind : kind -> Contents.kind
  val get_kind : Contents.kind -> kind
end

module MkContent (C : ContentSpecs) :
  Content
    with type kind = C.kind
     and type param = C.param
     and type data = C.data

type format = Contents.format
type kind = Contents.kind
type data = Contents.data

(** Data *)

val make : format -> data
val blit : data -> int -> data -> int -> int -> unit
val bytes : data -> int
val copy : data -> data
val clear : data -> unit

(** Format *)

val format : data -> format
val merge : format -> format -> unit
val string_of_format : format -> string

(* [format_of_param "label" "value"] *)
val format_of_param : string -> string -> format

(** Kind *)

val kind : format -> kind
val default_format : kind -> format
val string_of_kind : kind -> string
val kind_of_string : string -> kind

(** Internal content types. *)

(* None content type is abstract and only used
   via its params and data. *)
module None : sig
  val data : Contents.data
  val format : Contents.format
  val is_format : Contents.format -> bool
end

module Audio : sig
  include
    Content
      with type kind = [ `Pcm ]
       and type param = [ `Mono | `Stereo | `Five_point_one ]
       and type data = Audio.Mono.buffer array

  val channels_of_format : Contents.format -> int
  val format_of_channels : int -> Contents.format
end

module Video : Content with type kind = [ `Yuv420p ] and type data = Video.t

module Midi :
  Content
    with type kind = [ `Midi ]
     and type param = [ `Channels of int ]
     and type data = MIDI.Multitrack.buffer

val default_audio : unit -> Contents.format
val default_video : unit -> Contents.format
val default_midi : unit -> Contents.format
val is_internal : kind -> bool
