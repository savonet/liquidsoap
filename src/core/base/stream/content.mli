(*****************************************************************************

  Liquidsoap, a programmable stream generator.
  Copyright 2003-2026 Savonet team

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

open Mm

(** Generic content registration API. *)

module Contents = Content_base.Contents

(* Raised during any invalid operation below. *)
exception Invalid

(* Raised when calling [merge] below. *)
exception Incompatible_format of Contents.format * Contents.format

module type ContentSpecs = sig
  type kind
  type params
  type data

  val name : string

  (** Data *)

  (* Length is in main ticks. *)
  val make : ?length:int -> params -> data
  val length : data -> int
  val blit : data -> int -> data -> int -> int -> unit
  val copy : data -> data
  val checksum : data -> string

  (** Params *)

  val params : data -> params
  val merge : params -> params -> params

  (* [compatible src dst] *)
  val compatible : params -> params -> bool
  val string_of_params : params -> string

  (* [parse_param "label" "value"] *)
  val parse_param : string -> string -> params option

  (** Kind *)

  val kind : kind
  val default_params : kind -> params
  val string_of_kind : kind -> string
  val kind_of_string : string -> kind option
end

module type Content = sig
  include ContentSpecs

  (** Data *)

  val is_data : Contents.data -> bool
  val lift_data : ?offset:int -> ?length:int -> data -> Contents.data
  val get_data : Contents.data -> data

  (** Format *)

  val is_format : Contents.format -> bool
  val lift_params : params -> Contents.format
  val get_params : Contents.format -> params

  (** Kind *)

  val is_kind : Contents.kind -> bool
  val lift_kind : kind -> Contents.kind
  val get_kind : Contents.kind -> kind
end

module MkContent (C : ContentSpecs) :
  Content
    with type kind = C.kind
     and type params = C.params
     and type data = C.data

type format = Contents.format
type kind = Contents.kind
type data = Contents.data

(** Data *)

val make : ?length:int -> format -> data
val sub : data -> int -> int -> data
val truncate : data -> int -> data
val copy : data -> data
val checksum : data -> string
val length : data -> int
val append : data -> data -> data
val is_empty : data -> bool

(** Format *)

val format : data -> format
val duplicate : format -> format
val merge : format -> format -> unit

(* [compatible src dst] *)
val compatible : format -> format -> bool
val string_of_format : format -> string

(* [parse_param kind "label" "value"] *)
val parse_param : kind -> string -> string -> format

(** Kind *)

val kind : format -> kind
val default_format : kind -> format
val string_of_kind : kind -> string
val kind_of_string : string -> kind

(** Internal content types. *)

module Audio : sig
  type audio_params = Content_audio.Specs.params = {
    channel_layout : [ `Mono | `Stereo | `Five_point_one ] Lazy.t;
  }

  include
    Content
      with type kind = [ `Pcm ]
       and type params = audio_params
       and type data = Audio.t

  val kind : Contents.kind
  val channels_of_format : Contents.format -> int
  val format_of_channels : int -> Contents.format
end

module Video : sig
  type ('a, 'b) video_content = ('a, 'b) Content_video.Base.content = {
    length : int;
    mutable params : 'a;
    mutable data : (int * 'b) list;
  }

  type video_params = Content_video.Specs.params = {
    width : int Lazy.t option;
    height : int Lazy.t option;
  }

  include
    Content
      with type kind = [ `Canvas ]
       and type params = video_params
       and type data = (video_params, Video.Canvas.image) video_content

  val kind : Contents.kind
  val dimensions_of_format : Contents.format -> int * int
  val lift_image : Video.Canvas.image -> Contents.data

  type generator

  val make_generator : params -> generator

  val generate :
    ?create:(pos:int -> width:int -> height:int -> unit -> Video.Canvas.image) ->
    generator ->
    int ->
    data
end

module Midi : sig
  type midi_params = Content_midi.Specs.params = { channels : int }

  include
    Content
      with type kind = [ `Midi ]
       and type params = midi_params
       and type data = MIDI.Multitrack.buffer

  val kind : Contents.kind
end

module Metadata : sig
  include Content with type kind = [ `Metadata ] and type params = unit

  val format : format
  val get_data : Contents.data -> (int * Frame_base.metadata) list
  val set_data : Contents.data -> (int * Frame_base.metadata) list -> unit
  val lift_data : (int * Frame_base.metadata) list -> Contents.data
end

module Track_marks : sig
  include Content with type kind = [ `Track_marks ] and type params = unit

  val format : format
  val get_data : Contents.data -> int list
  val set_data : Contents.data -> int list -> unit
  val lift_data : int list -> Contents.data
end

(* Some tools *)
val merge_param :
  ?compare:('a -> 'a -> bool) ->
  name:string ->
  'a option * 'a option ->
  'a option

val print_optional : (string * string option) list -> string
