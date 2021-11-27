(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2021 Savonet team

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

(** In [`Audio] mode, only audio can be put in the buffer, and similarly for
      the [`Video] mode.
      In [`Both] mode, both types of content can be fed into the generator,
      asynchronously, and they exit the buffer synchronously. PTS are only
      used in this mode and are used to make sure that all audio and video
      content is synchronized when exported. Typically, during muxing, the
      audio source may not be ready for a while while the video source keeps
      filling up the buffer. In this case, when the audio source starts
      filling up the buffer as well, we can filter out all the video content
      that has no corresponding audio.
      [`Undefined] forbids any feeding, it's useful to make sure a meaningful
      mode is assigned before any use. *)
type mode = [ `Audio | `Video | `Both | `Undefined ]

type t

val create :
  ?overfull:[ `Drop_old of int ] ->
  ?log:(string -> unit) ->
  ?log_overfull:bool ->
  mode ->
  t

(** Current mode: in Audio mode (resp. Video), only audio (resp. Audio) can be
      fed, otherwise both have to be fed. *)
val mode : t -> mode

(** Change the generator mode. *)
val set_mode : t -> mode -> unit

val audio_length : t -> int
val video_length : t -> int
val length : t -> int
val buffered_length : t -> int
val remaining : t -> int
val add_metadata : ?pos:int -> t -> Frame.metadata -> unit
val add_break : ?sync:bool -> ?pos:int -> t -> unit
val remove : t -> int -> unit
val clear : t -> unit
val put_audio : ?pts:int64 -> t -> Content.data -> int -> int -> unit
val put_video : ?pts:int64 -> t -> Content.data -> int -> int -> unit

val feed :
  ?copy:[ `None | `Audio | `Video | `Both ] ->
  ?mode:mode ->
  t ->
  Content.data ->
  int ->
  int ->
  unit

val feed_from_frame :
  ?copy:[ `None | `Audio | `Video | `Both ] ->
  ?mode:mode ->
  t ->
  Frame.t ->
  unit

val get : t -> int -> Content.data

(** Fill a frame from the generator. *)
val fill : t -> Frame.t -> unit
