(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2021 Savonet team

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

type 'a chunk = { data : 'a; offset : int; size : int }
type ('a, 'b) chunks = { mutable params : 'a; mutable chunks : 'b chunk list }

module Contents : sig
  type kind
  type format
  type data

  type audio_params = {
    channel_layout : [ `Mono | `Stereo | `Five_point_one ] Lazy.t;
  }

  type video_params = { width : int Lazy.t option; height : int Lazy.t option }
  type midi_params = { channels : int }
end

(* Raised during any invalid operation below. *)
exception Invalid

(* Raised when calling [merge] below. *)
exception Incompatible_format of Contents.format * Contents.format

module type ContentSpecs = sig
  type kind
  type params
  type data

  (** Data *)

  (* Size is in main ticks. *)
  val make : size:int -> params -> data

  (* TODO: This will be removed when reworking
     the streaming API. *)
  val blit : data -> int -> data -> int -> int -> unit

  (* Returns length in main ticks. *)
  val length : data -> int
  val copy : data -> data

  (* TODO: this will be removed when rewriting
     streaming API. *)
  val clear : data -> unit

  (** Params *)

  val params : data -> params
  val merge : params -> params -> params
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
  val lift_data : data -> Contents.data
  val get_data : Contents.data -> data
  val get_chunked_data : Contents.data -> (params, data) chunks

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

val make : size:int -> format -> data
val length : data -> int

(* TODO: This will be removed when reworking
   the streaming API. *)
val blit : data -> int -> data -> int -> int -> unit
val fill : data -> int -> data -> int -> int -> unit
val sub : data -> int -> int -> data
val copy : data -> data

(* TODO: This will be removed when reworking
   the streaming API. *)
val clear : data -> unit
val is_empty : data -> bool
val append : data -> data -> data

(** Format *)

val format : data -> format
val duplicate : format -> format
val merge : format -> format -> unit
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

(* None content type is abstract and only used
   via its params and data. *)
module None : sig
  val is_data : Contents.data -> bool
  val data : int -> Contents.data
  val format : Contents.format
  val is_format : Contents.format -> bool
end

module Audio : sig
  include
    Content
      with type kind = [ `Pcm ]
       and type params = Contents.audio_params
       and type data = Audio.Mono.buffer array

  val kind : Contents.kind
  val channels_of_format : Contents.format -> int
  val format_of_channels : int -> Contents.format
end

module Video : sig
  include
    Content
      with type kind = [ `Yuv420p ]
       and type params = Contents.video_params
       and type data = Video.t

  val kind : Contents.kind
end

module Midi : sig
  include
    Content
      with type kind = [ `Midi ]
       and type params = Contents.midi_params
       and type data = MIDI.Multitrack.buffer

  val kind : Contents.kind
end

module Metadata : sig
  include Content with type kind = [ `Metadata ] and type params = unit

  val format : format
  val lift_data : size:int -> (int * Frame_base.metadata) list -> Contents.data
  val get_data : Contents.data -> (int * Frame_base.metadata) list
  val set_data : Contents.data -> (int * Frame_base.metadata) list -> unit
end

module Breaks : sig
  include Content with type kind = [ `Breaks ] and type params = unit

  val format : format
  val lift_data : size:int -> int list -> Contents.data
  val get_data : Contents.data -> int list
  val set_data : Contents.data -> int list -> unit
end

(** Meta content module for frame data. Should never be used for source content type! *)
module Frame : sig
  type 'a frame_content = {
    breaks : 'a;
    metadata : 'a;
    media : 'a Frame_base.fields;
  }

  include Content with type data = Contents.data frame_content

  val lift_params : Contents.format Frame_base.fields -> Contents.format

  (* TODO: This will be removed when reworking
     the streaming API. *)
  val blit_media : Contents.data -> int -> Contents.data -> int -> int -> unit
  val get_audio : Contents.data -> Contents.data
  val set_audio : Contents.data -> Contents.data -> unit
  val get_video : Contents.data -> Contents.data
  val set_video : Contents.data -> Contents.data -> unit
  val get_midi : Contents.data -> Contents.data
  val set_midi : Contents.data -> Contents.data -> unit
  val get_breaks : Contents.data -> int list
  val add_break : Contents.data -> int -> unit
  val set_breaks : Contents.data -> int list -> unit
  val get_all_metadata : Contents.data -> (int * Frame_base.metadata) list

  val set_all_metadata :
    Contents.data -> (int * Frame_base.metadata) list -> unit

  val set_metadata : Contents.data -> int -> Frame_base.metadata -> unit
  val get_metadata : Contents.data -> int -> Frame_base.metadata option
  val free_metadata : Contents.data -> int -> unit
  val free_all_metadata : Contents.data -> unit
end

val default_audio : unit -> Contents.format
val default_video : unit -> Contents.format
val default_midi : unit -> Contents.format
val is_internal : kind -> bool

(* Some tools *)
val merge_param : name:string -> 'a option * 'a option -> 'a option
val print_optional : (string * string option) list -> string
