(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2012 Savonet team

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

(** Frames are the units in which streams are split into. *)

type ('a, 'b, 'c) fields = { audio : 'a; video : 'b; midi : 'c; }

type multiplicity = Variable | Zero | Succ of multiplicity
type content_kind = (multiplicity, multiplicity, multiplicity) fields

type content_type = (int, int, int) fields

type content = (audio_t array, video_t array, midi_t array) fields
and audio_t = ABuf.buffer
and video_t = Video.buffer
and midi_t = MIDI.buffer

val blit_content : content -> int -> content -> int -> int -> unit

val copy : content -> content

type metadata = (string, string) Hashtbl.t
type t = {
  mutable breaks : int list;
  mutable metadata : (int * metadata) list;
  mutable contents : (int * content) list
}

(** {2 Content-independent frame operations}
  * All units are in ticks (master clock). *)

val create : content_kind -> t

val position : t -> int
val is_partial : t -> bool

val clear : t -> unit
val clear_from : t -> int -> unit
val advance : t -> unit

(** {3 Breaks} *)

val breaks : t -> int list
val set_breaks : t -> int list -> unit
val add_break : t -> int -> unit

(** {3 Metadata} *)

exception No_metadata
val set_metadata : t -> int -> metadata -> unit
val get_metadata : t -> int -> metadata option
val free_metadata : t -> int -> unit
val free_all_metadata : t -> unit
val get_all_metadata : t -> (int * metadata) list
val set_all_metadata : t -> (int * metadata) list -> unit
val get_past_metadata : t -> metadata option

(** {2 Content operations} *)

val content : t -> int -> int * content
val content_of_type : ?force:content -> t -> int -> content_type -> content

val hide_contents : t -> (unit -> unit)
type content_layer = { content : content ; start : int ; length : int }
val get_content_layers : t -> content_layer list

exception No_chunk
val get_chunk : t -> t -> unit

(** {2 Compatibilities between content values, types and kinds} *)

val mul_sub_mul : multiplicity -> multiplicity -> bool
val int_sub_mul : int -> multiplicity -> bool
val mul_eq_int  : multiplicity -> int -> bool
val kind_sub_kind : content_kind -> content_kind -> bool
val type_has_kind : content_type -> content_kind -> bool
val content_has_type : content -> content_type -> bool
val type_of_content : content -> content_type
val type_of_kind : content_kind -> content_type

val mul_of_int : int -> multiplicity
val add_mul : multiplicity -> multiplicity -> multiplicity

val string_of_content_kind : content_kind -> string
val string_of_content_type : content_type -> string

(** {2 Format settings} *)

(** The channel numbers are only defaults, used when channel numbers
  * cannot be infered / are not forced from the context.
  * I'm currently unsure how much they are really useful. *)

(* This variable prevents forcing
 * the value of a lazy configuration
 * value before the user gets a chance to
 * override the default. *)
val allow_lazy_config_eval : unit -> unit

val audio_channels : int Lazy.t
val video_channels : int Lazy.t
val midi_channels : int Lazy.t

val video_width : int Lazy.t
val video_height : int Lazy.t

val audio_rate : int Lazy.t
val video_rate : int Lazy.t
val midi_rate : int Lazy.t
val master_rate : int Lazy.t

val size : int Lazy.t
val duration : float Lazy.t

(** {2 Time and frequency conversions} *)

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
