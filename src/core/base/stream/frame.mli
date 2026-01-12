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

(** Operations on frames, which are small portions of streams. *)

(** {2 Frame definitions} *)

module Fields : sig
  include module type of Liquidsoap_lang.Methods

  type field = Frame_base.Fields.field
  type nonrec 'a t = (field, 'a) t

  val metadata : field
  val track_marks : field
  val audio : field
  val audio_n : int -> field
  val video : field
  val video_n : int -> field
  val data : field
  val data_n : int -> field
  val midi : field
  val register : string -> field
  val make : ?audio:'a -> ?video:'a -> ?midi:'a -> unit -> 'a t
  val string_of_field : field -> string
  val field_of_string : string -> field
end

type field = Fields.field

(** Precise description of the channel types for the current track. *)
type content_type = Content.format Fields.t

module Metadata : sig
  type t = Frame_base.Metadata.t

  val to_string : t -> string
  val from_list : (string * string) list -> t
  val to_list : t -> (string * string) list
  val is_empty : t -> bool
  val empty : t
  val append : t -> t -> t
  val cardinal : t -> int
  val fold : (string -> string -> 'a -> 'a) -> t -> 'a -> 'a
  val find : string -> t -> string
  val find_opt : string -> t -> string option
  val mem : string -> t -> bool
  val remove : string -> t -> t
  val add : string -> string -> t -> t
  val mapi : (string -> string -> string) -> t -> t
  val map : (string -> string) -> t -> t
  val filter : (string -> string -> bool) -> t -> t
  val for_all : (string -> string -> bool) -> t -> bool
  val exists : (string -> string -> bool) -> t -> bool
  val iter : (string -> string -> unit) -> t -> unit

  module Export : sig
    type metadata = t
    type t

    val from_metadata : ?cover:bool -> metadata -> t
    val to_metadata : t -> metadata
    val to_list : t -> (string * string) list
    val equal : t -> t -> bool
    val empty : t
    val is_empty : t -> bool
  end
end

(** Metadata of a frame. *)
type metadata = Metadata.t

(** A frame is a chunk of data which should be at most [size] *)
type t = Content.data Fields.t

(** {2 Content-independent frame operations} *)

(** All units are in ticks (main clock). *)

(** Create a frame of a given content type. *)
val create : length:int -> content_type -> t

(** Get at most length data from the start of the given frame. *)
val slice : t -> int -> t

(** Get the content after a given offset. *)
val after : t -> int -> t

(** [sub frame ofs len]: get the a subset of length [len] of the frame content,
    starting at [ofs]. *)
val sub : t -> int -> int -> t

(** Get a frame's content type. *)
val content_type : t -> content_type

(** [append f f'] appends the data from [f'] to [f]. [f'] can possibly have more
    fields that [f]. *)
val append : t -> t -> t

(** Get a frame's content. *)
val get : t -> field -> Content.data

(** Set a frame's content. *)
val set : t -> field -> Content.data -> t

(** Set a frame's content using data. Data is assumed to be of the frame's
    position length. The type is designed to work with `Content.*.lift_data`
    functions. *)
val set_data :
  t -> field -> (?offset:int -> ?length:int -> 'a -> Content.data) -> 'a -> t

(** Get a frame's audio content. *)
val audio : t -> Content.data

(** Get a frame's video content. *)
val video : t -> Content.data

(** Get a frame's midi content. *)
val midi : t -> Content.data

(* Position in the frame. *)
val position : t -> int

(** Remaining data length in the frame. *)
val remaining : t -> int

(** Is the frame partially filled, i.e. is its end [position] strictly before
    [Lazy.force Frame.size]? *)
val is_partial : t -> bool

(** {3 Track marks} *)

(** List of track marks in a frame. *)
val track_marks : t -> int list

(** Add a track mark to a frame *)
val add_track_mark : t -> int -> t

(** Add a multiple track marks to a frame *)
val add_track_marks : t -> int list -> t

(** [true] is frame has a track mark. *)
val has_track_marks : t -> bool

(** [true] is frame has a track mark at the given position *)
val has_track_mark : t -> int -> bool

(** Remove all track marks from the frame. *)
val drop_track_marks : t -> t

(** {3 Metadata} *)

exception No_metadata

(** Attach metadata at a given position to the frame. *)
val add_metadata : t -> int -> metadata -> t

(* Remove a metadata at a given position. *)
val remove_metadata : t -> int -> t

(** Retrieve metadata at a given position. *)
val get_metadata : t -> int -> metadata option

(** Retrieve all metadata. *)
val get_all_metadata : t -> (int * metadata) list

(** [true] if the frame contains metadata. *)
val has_metadata : t -> bool

(** Attach multiple metadata to a frame. *)
val add_all_metadata : t -> (int * metadata) list -> t

(** Map a function over the frame's metadata. *)
val map_metadata : t -> (int * metadata -> (int * metadata) option) -> t

(** {2 Content operations} *)

val string_of_content_type : content_type -> string
val assert_compatible : content_type -> content_type -> unit
val compatible : content_type -> content_type -> bool

(** {2 Format settings} *)

(** The channel numbers are only defaults, used when channel numbers cannot be
    inferred / are not forced from the context. I'm currently unsure how much
    they are really useful. *)

(** Default number of audio channels. *)
val audio_channels : int Lazy.t

(** Is video enabled? *)
val default_video_enabled : bool Lazy.t

(** Default number of MIDI channels. *)
val midi_channels : int Lazy.t

(** Width of video images. *)
val video_width : int Lazy.t

(** Height of video images. *)
val video_height : int Lazy.t

(** Rate of audio (in samples per second). *)
val audio_rate : int Lazy.t

(** Video rate (in images per second). *)
val video_rate : int Lazy.t

val midi_rate : int Lazy.t

(** Ticks per second. *)
val main_rate : int Lazy.t

val size : int Lazy.t

(** Duration of a frame in seconds. *)
val duration : float Lazy.t

(** {2 Time and frequency conversions} *)

(** Conversion between the internal unit (main ticks), seconds, and data units.
*)

(** Duration of given number of samples in ticks. *)
val audio_of_main : int -> int

val video_of_main : int -> int
val video_of_main_f : int -> float
val midi_of_main : int -> int
val main_of_audio : int -> int
val main_of_video : int -> int
val main_of_midi : int -> int
val main_of_seconds : float -> int
val audio_of_seconds : float -> int
val video_of_seconds : float -> int
val seconds_of_main : int -> float
val seconds_of_audio : int -> float
val seconds_of_video : int -> float
