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

(** {1 Video frame manipulation} *)

type t = Frame.t

(** Is it partially filled ? *)
val is_partial : t -> bool

(** Number of video frames. *)
val size : 'a -> int

val position : t -> int

(** Position (in video samples) of the next video sample to fill-in. *)
val next_sample_position : t -> int

(** Add a track mark at given video position. *)
val add_track_mark : t -> int -> t

(** Get video contents. Raises [Not_found] is frame has no video *)
val content : ?field:Frame.field -> t -> Content.data

(** Get video content. Raises [Content.Invalid] if video content is not in
    internal format and [Not_found] if frame has no video content. *)
val data : ?field:Frame.field -> t -> Content.Video.data
