(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2023 Savonet team

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
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 *****************************************************************************)

(** Operations on frames, which are small portions of streams. *)

(** {2 Frame definitions} *)

module Fields : sig
  include module type of Liquidsoap_lang.Methods

  type field = Frame_base.Fields.field
  type 'a typ = (field, 'a) t
  type 'a t = 'a typ

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

  val from_list : (string * string) list -> t
  val to_list : t -> (string * string) list
  val is_empty : t -> bool
  val empty : t
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
    type t = Frame_base.Metadata.Export.t

    val from_metadata : metadata -> t
    val to_metadata : t -> metadata
    val to_list : t -> (string * string) list
    val equal : t -> t -> bool
    val empty : t
    val is_empty : t -> bool
  end
end

(** Metadata of a frame. *)
type metadata = Metadata.t

(** A frame. *)
type t = Generator.t

(** {2 Content-independent frame operations} *)

(** All units are in ticks (main clock). *)

(** Create a frame of a given content type. *)
val create : content_type -> t

(** A dummy frame which should never written to or read from. This is however
    useful as a placeholder before initialization of references. *)
val dummy : unit -> t

(** Get a frame's content type. *)
val content_type : t -> content_type

(** Get a frame's content. *)
val get : t -> field -> Content.data

(** Set a frame's content. *)
val set : t -> field -> Content.data -> unit

(** Get a frame's audio content. *)
val audio : t -> Content.data

(** Set a frame's audio content. *)
val set_audio : t -> Content.data -> unit

(** Get a frame's video content. *)
val video : t -> Content.data

(** Set a frame's video content. *)
val set_video : t -> Content.data -> unit

(** Get a frame's midi content. *)
val midi : t -> Content.data

(** Set a frame's midi content. *)
val set_midi : t -> Content.data -> unit

(** Position of the end of the last chunk of the frame (i.e. the offset of the
    end of the frame). *)
val position : t -> int

(** Remaining data length in the frame. *)
val remaining : t -> int

(** Is the frame partially filled, i.e. is its end [position] strictly before
    its size? *)
val is_partial : t -> bool

(** Make the frame empty. *)
val clear : t -> unit

(** {3 Breaks} *)

(** List of breaks in a frame. *)
val breaks : t -> int list

(** Set all the breaks of a frame. *)
val set_breaks : t -> int list -> unit

(** Add a break to a frame (which should be past its current end position). *)
val add_break : t -> int -> unit

(** {3 Metadata} *)

exception No_metadata

(** Attach metadata at a given position in the frame. *)
val set_metadata : t -> int -> metadata -> unit

(** Retrieve metadata at a given position. *)
val get_metadata : t -> int -> metadata option

(** Remove all metadata at given position. *)
val free_metadata : t -> int -> unit

(** Remove all metadata. *)
val free_all_metadata : t -> unit

(** Retrieve all metadata. *)
val get_all_metadata : t -> (int * metadata) list

(** Set all metadata. *)
val set_all_metadata : t -> (int * metadata) list -> unit

(** {2 Content operations} *)

exception No_chunk

val get_chunk : t -> t -> unit
val string_of_content_type : content_type -> string
val compatible : content_type -> content_type -> bool

(** {2 Format settings} *)

(** The channel numbers are only defaults, used when channel numbers
  * cannot be inferred / are not forced from the context.
  * I'm currently unsure how much they are really useful. *)

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

(** Conversion between the internal unit (main ticks), seconds, and data
    units. *)

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
