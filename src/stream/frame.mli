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
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 *****************************************************************************)

(** Operations on frames, which are small portions of streams. *)

(** {2 Frame definitions} *)

(** A frame contains fields which hold audio, video and MIDI data. *)
type ('a, 'b, 'c) fields = { audio : 'a; video : 'b; midi : 'c }

(** Multiplicity of a field, used in types to impose constraints on channels
    (empty, variable, at least k, etc.). *)
type multiplicity = Any | Zero | Succ of multiplicity

(** Multiplicity of each field of a frame. *)
type content_kind = (multiplicity, multiplicity, multiplicity) fields

(** Content type of a frame: number of channels for audio, video and MIDI. *)
type content_type = (int, int, int) fields

(** Actual content of a frame. *)
type content = (audio_t array, video_t array, midi_t array) fields

(** Audio data. *)
and audio_t = Audio.Mono.buffer

(** Video data. *)
and video_t = Video.t

(** MIDI data. *)
and midi_t = MIDI.buffer

(** [blit_content c1 o1 c2 o2 l] copies [l] data from [c1] starting at offset
    [o1] into [c2] starting at offset [o2]. All numerical values are in
    ticks. *)
val blit_content : content -> int -> content -> int -> int -> unit

(** Make a copy of the content of a frame. *)
val copy : content -> content

(** Metadata of a frame. *)
type metadata = (string, string) Hashtbl.t

(** A frame. *)
type t = {
  mutable breaks : int list;
      (** End of track markers. A break at the end of the
                                 buffer is not an end of track (if needed, the
                                 end-of-track needs to be put at the beginning
                                 of the next frame). *)
  mutable metadata : (int * metadata) list;
      (** Metadata along with the time they occur. *)
  mutable content : content;
}

(** {2 Content-independent frame operations} *)

(** All units are in ticks (master clock). *)

(** Create a frame of a given content type. *)
val create_type : content_type -> t

(** Create a frame of given content kind. *)
val create : content_kind -> t

(** Get a frame's content type. *)
val content_type : t -> content_type

(** Get a frame's audio content. *)
val audio : t -> audio_t array

(** Set a frame's audio content. *)
val set_audio : t -> audio_t array -> unit

(** Get a frame's video content. *)
val video : t -> video_t array

(** Set a frame's video content. *)
val set_video : t -> video_t array -> unit

(** Get a frame's midi content. *)
val midi : t -> midi_t array

(** Set a frame's midi content. *)
val set_midi : t -> midi_t array -> unit

(** Position of the end of the last chunk of the frame (i.e. the offset of the
    end of the frame). *)
val position : t -> int

(** Is the frame partially filled, i.e. is its end [position] strictly before
    its size? *)
val is_partial : t -> bool

(** Make the frame empty. *)
val clear : t -> unit

(** Same as [clear] from a given position. *)
val clear_from : t -> int -> unit

(** Same as [clear] but leaves the last metadata at position [-1]. *)
val advance : t -> unit

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

(** Retreive "past metadata" which are stored at offset [-1] (cf. [advance]). *)
val get_past_metadata : t -> metadata option

(** {2 Content operations} *)

exception No_chunk

val get_chunk : t -> t -> unit

(** {2 Compatibilities between content values, types and kinds} *)

(** Compatibilities between content kinds, types and values: [sub a b] is [true]
    when [b] is more permissive than [a]. *)

val mul_sub_mul : multiplicity -> multiplicity -> bool
val int_sub_mul : int -> multiplicity -> bool
val mul_eq_int : multiplicity -> int -> bool
val kind_sub_kind : content_kind -> content_kind -> bool
val type_has_kind : content_type -> content_kind -> bool
val type_of_content : content -> content_type
val type_of_kind : content_kind -> content_type
val mul_of_int : int -> multiplicity
val add_mul : multiplicity -> multiplicity -> multiplicity
val string_of_content_kind : content_kind -> string
val string_of_content_type : content_type -> string

(** {2 Format settings} *)

(** The channel numbers are only defaults, used when channel numbers
  * cannot be inferred / are not forced from the context.
  * I'm currently unsure how much they are really useful. *)

(** Prevent forcing the value of a lazy configuration value before the user gets
    a chance to override the default. *)
val allow_lazy_config_eval : unit -> unit

(** Default number of audio channels. *)
val audio_channels : int Lazy.t

(** Default number of video channels. *)
val video_channels : int Lazy.t

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
val master_rate : int Lazy.t

val size : int Lazy.t

(** Duration of a frame in seconds. *)
val duration : float Lazy.t

(** {2 Time and frequency conversions} *)

(** Conversion between the internal unit (master ticks), seconds, and data
    units. *)

(** Duration of given number of samples in ticks. *)
val audio_of_master : int -> int

val video_of_master : int -> int
val midi_of_master : int -> int
val master_of_audio : int -> int
val master_of_video : int -> int
val master_of_midi : int -> int
val master_of_seconds : float -> int
val audio_of_seconds : float -> int
val video_of_seconds : float -> int
val seconds_of_master : int -> float
val seconds_of_audio : int -> float
val seconds_of_video : int -> float
