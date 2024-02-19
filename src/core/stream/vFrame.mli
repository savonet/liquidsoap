(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2024 Savonet team

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

(** {1 Video frame manipulation}
  *
  * This is a simplified video-only version of Frame. Some parts of Frame,
  * such as the metadata API, is unavailable. This is because it must
  * be used with care.
  *
  * Even video-only operators must comply to the general requirements
  * of sources in liquidsoap. In particular they should be able to fill
  * a frame starting at any position. That position might not be
  * a video position -- in that case, the first video sample to work on,
  * if there is one, will be a little farther in the frame. When looking
  * for metadata (usually attached at the beginning of a track) a
  * video position cannot be used, for the same reason: the track might
  * not start on a video sample position. *)

type t = Frame.t

(** Is it partially filled ? *)
val is_partial : t -> bool

(** Number of video frames. *)
val size : 'a -> int

val position : t -> int

(** Position (in video samples) of the next video sample to fill-in. *)
val next_sample_position : t -> int

(** Add a break at given video position. *)
val add_break : t -> int -> unit

(** [get_content source frame] has [source] fill [frame],
  * and returns the produced chunk of video content.
  * It is possible that a successful filling produced audio samples
  * but no video sample. *)
val get_content : Frame.t -> Source.source -> (Content.data * int * int) option

(** Get video contents. Raises [Not_found] is frame has no video *)
val content : ?field:Frame.field -> t -> Content.data

(** Get video content. Raises [Content.Invalid] if video content is not in
    internal format and [Not_found] if frame has no video content. *)
val data : ?field:Frame.field -> t -> Content.Video.data
