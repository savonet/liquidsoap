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

(** The internal video converter (decoder.md §16.1): decoded frames into
    internal images of a fixed [width × height], at the video rate. It holds one
    stage, built for one frame format, time base and source id, and rebuilt
    after a drain when any of them changes; a format change never ends the
    track. The internal video decoder and the inline decoders use it. *)

type t

(** [target] gives the pixel format to scale to for a frame format. *)
val create :
  log:Log.t ->
  width:int ->
  height:int ->
  target:(Avutil.Video.frame_format -> Avutil.Pixel_format.t) ->
  unit ->
  t

(** [put] receives each image, translated and clipped to [width × height], with
    the frame it came from. *)
val convert :
  ?source_id:Source_id.t ->
  ?format:Avutil.Video.frame_format ->
  t ->
  time_base:Avutil.rational ->
  put:(Mm.Video.Canvas.image -> Avutil.video Avutil.frame -> unit) ->
  Avutil.video Avutil.frame ->
  unit

(** Drains the current stage into [put]; the stage stays usable. *)
val flush :
  t -> put:(Mm.Video.Canvas.image -> Avutil.video Avutil.frame -> unit) -> unit
