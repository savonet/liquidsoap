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

(** The audio frame writer (encoder-internal.md §5.1, timestamps.md §3): sets
    each frame's pts from a sample counter and, with a frame size, re-frames to
    it before [write]. The container stream encoder and the inline encoders
    share it. *)

type t

(** [start] is the starting sample count, at the target rate; [priming] is the
    codec's initial padding, in samples. *)
val create :
  frame_size:int option ->
  params:Avfilter.Utils.audio_params ->
  priming:int ->
  start:int ->
  (Avutil.audio Avutil.frame -> unit) ->
  t

val feed : t -> Avutil.audio Avutil.frame -> unit

(** Moves the counter to [target] samples at a gap of the input; the partial
    frame the re-framer holds ends there. A first jump to a positive target, the
    continuing timeline of a restarted container, lands [priming] samples later
    so that the codec's first packet starts at [target] (encoder.md §13.2). *)
val jump : t -> int -> unit

(** Drains the re-framer: its last frame is short when the input did not fill
    it. *)
val flush : t -> unit
