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

(** The copy tracks of one container (encoder-copy.md) and their source
    switching: tenures, groups, keyframe readiness, held packets, and every
    timestamp rule of timestamps.md §6–§7.

    The instance creates one {!t} per container and one {!track} per copy field,
    in track order. For each frame of positive length it calls {!encode} on each
    copy track, then {!end_of_frame} once; {!flush} before the container closes.
*)

type t
type track

(** Raised by {!encode} when a source's parameters cannot follow the stream
    header in this container (§12.7): the track, the header's parameters and the
    new ones. *)
exception
  Incompatible of { field : Frame.field; header : string; source : string }

(** [whole_output] is [true] when every stream of the output is a copy track, so
    that a group of every track is a whole-output switch. *)
val create :
  log:Log.t ->
  whole_output:bool ->
  stores_header:bool ->
  Avutil.output Avutil.container ->
  t

(** [on_packet] is called before each packet reaches the muxer, with its key
    flag and media time (encoder.md §8.2). *)
val track :
  t ->
  field:Frame.field ->
  keyframe:Liquidsoap_ffmpeg_formats.Encoder_format.keyframe ->
  on_packet:(key:bool -> time:int option -> unit) ->
  track

(** The known parameters of the field's first run that has them, in this frame.
*)
val first_params : track -> Frame.t -> Copy_params.t option

(** Creates the muxer stream from the field's first known parameters. *)
val mk_stream : track -> Frame.t -> unit

(** The position in the frame of the first run whose parameters cannot follow
    its track's stream header, for an instance that restarts its container there
    (encoder.md §13.3). *)
val switch_position : t -> Frame.t -> int option

(** [encode track ~position frame]: [position] is the output position of the
    frame's start, in main ticks. *)
val encode : track -> position:int -> Frame.t -> unit

val end_of_frame : t -> position:int -> unit

(** Starts every waiting group as the wait limit does and writes what it holds.
*)
val flush : t -> position:int -> unit

val intra_only : track -> bool
val codec_attr : track -> string option
val bitrate : track -> int option
val video_size : track -> (int * int) option
