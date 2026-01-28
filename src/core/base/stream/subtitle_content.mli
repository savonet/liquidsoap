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

(** Subtitle content type for text-based subtitles (ASS or plain text). *)

(** A single subtitle entry. Times are relative to position in main ticks. *)
type subtitle = {
  start_time : int;  (** Start time relative to position, in main ticks. *)
  end_time : int;  (** End time relative to position, in main ticks. *)
  text : string;  (** Subtitle text content. *)
  format : [ `Ass | `Text ];  (** Format: ASS dialogue or plain text. *)
  forced : bool;  (** Whether this is a forced subtitle. *)
}

include
  Content_base.Content with type kind = [ `Subtitle ] and type params = unit

(** The subtitle content format. *)
val format : Content_base.format

(** Create subtitle content data from a list of (position, subtitle) pairs.
    Position is in main ticks. If length is not provided, it defaults to the
    maximum position + 1. *)
val lift_data : ?length:int -> (int * subtitle) list -> Content_base.data

(** Set the subtitle data in existing content. *)
val set_data : Content_base.data -> (int * subtitle) list -> unit

(** Get the subtitle data as a list of (position, subtitle) pairs, sorted by
    position. *)
val get_data : Content_base.data -> (int * subtitle) list
