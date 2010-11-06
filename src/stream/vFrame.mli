(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2010 Savonet team

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

(** Video frame manipulation *)

type t = Frame.t

(** Is it partially filled ? *)
val is_partial : t -> bool

(** Number of video frames. *)
val size : t -> int

(** Position of the first break. *)
val position : t -> int

(** Add a break. *)
val add_break : t -> int -> unit

type metadata = (string,string) Hashtbl.t

val set_metadata     : t -> int -> metadata -> unit
val get_metadata     : t -> int -> metadata option

(** Get the video channels at a given position.
  * Requires that the frame contains only video data starting at this point. *)
val content : t -> int -> Video.buffer array

val get_content :
      Frame.t -> Source.source -> (Video.buffer array * int * int) option

(** Get video channels starting at a given position,
  * creating them if needed.
  * This is the function to call for writing pure video in a frame. *)
val content_of_type : channels:int -> t -> int -> Video.buffer array
