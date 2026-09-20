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

module type FormatSpecs = Content_base.FormatSpecs
module type DataSpecs = Content_base.DataSpecs
module type ContentSpecs = Content_base.ContentSpecs
module type Format = Content_base.Format
module type Content = Content_base.Content

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

(** A format as it crosses a dump: its kind, and what its content encodes of its
    parameters. [parse_format] answers [None] for anything else. *)
val serialize_format : format -> string

val parse_format : string -> format option

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
    channel_layout : [ `Mono | `Stereo | `Five_point_one ] Lazy.Mutexed.t;
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
    width : int Lazy.Mutexed.t option;
    height : int Lazy.Mutexed.t option;
    alpha : bool option Unifier.t;
  }

  include
    Content
      with type kind = [ `Canvas ]
       and type params = video_params
       and type data = (video_params, Video.Canvas.image) video_content

  val kind : Contents.kind
  val dimensions_of_format : Contents.format -> int * int
  val alpha_of_format : Contents.format -> bool
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

(** Custom Liquidsoap value type for [format]. *)
module Format_val : sig
  val t : Type.t

  val to_value :
    ?pos:Liquidsoap_lang_prelude.Pos.Option.base -> Contents.format -> Value.t

  val of_value : Value.t -> Contents.format
end

(** Record type with one optional method per registered content type. Call after
    all content modules have been initialized. *)
val content_types : unit -> Type.t

(** Convert a format to [(normalized_name, value)]. Returns [None] for types
    with no registered Lang spec (metadata, track_marks). *)
val value_of_format : Contents.format -> (string * Value.t) option

(* Some tools *)
val merge_param :
  ?compare:('a -> 'a -> bool) ->
  name:string ->
  'a option * 'a option ->
  'a option

val print_optional : (string * string option) list -> string
