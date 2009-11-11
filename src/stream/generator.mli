(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2009 Savonet team

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

module Generator :
sig
  type 'a t
  val create : unit -> 'a t
  val clear : 'a t -> unit
  val length : 'a t -> int
  val remove : 'a t -> int -> unit
  val put : 'a t -> 'a -> int -> int -> unit
  val get : 'a t -> int -> ('a * int * int * int) list
end

(** Generator that consumes frames (or frame content)
  * and produces frames. *)
module From_frames :
sig
  type t
  val create : unit -> t
  val clear : t -> unit
  val length : t -> int
  val remaining : t -> int
  val add_metadata : t -> Frame.metadata -> unit
  val add_break : t -> unit
  val remove : t -> int -> unit
  val feed : t -> Frame.content -> int -> int -> unit
  val feed_from_frame : t -> Frame.t -> unit
  val fill : t -> Frame.t -> unit
end

(** Generator that consumes audio and video asynchronously,
  * and produces frames. *)
module From_audio_video :
sig
  type t
  type mode = Audio | Video | Both
  val create : mode -> t
  val set_mode : t -> mode -> unit
  val length : t -> int
  val put_audio : t -> Frame.audio_t array -> int -> int -> unit
  val put_video : t -> Frame.video_t array -> int -> int -> unit
  val fill : t -> Frame.t -> unit
end
