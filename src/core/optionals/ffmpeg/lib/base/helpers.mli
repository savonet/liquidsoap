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

(** The helper graphs of filters.md §16: private chains a stage builds for one
    stream. [convert] pushes a frame and delivers every frame then ready; [eof]
    drains and delivers the rest. They raise [Avutil.Error] on an FFmpeg
    failure, which their owner counts as an item fault and answers by building a
    new helper for the next frame.

    [Fps] retimes video to a constant frame rate (§16.2); [Fit] fits video into
    a picture of another size and pixel format (§16.4), and its [fitted_size] is
    the largest size inside a target that keeps a source's proportions;
    [Aformat] converts audio sample format, layout and rate (§16.3). *)

module Fps : sig
  type start = [ `None | `Pts of Int64.t | `Time of float ]
  type t

  val create :
    ?source_rate:Avutil.rational ->
    target:int ->
    start:start ->
    time_base:Avutil.rational ->
    Avutil.Video.frame_format ->
    t

  val time_base : t -> Avutil.rational

  val convert :
    t ->
    (Avutil.video Avutil.frame -> unit) ->
    Avutil.video Avutil.frame ->
    unit

  val eof : t -> (Avutil.video Avutil.frame -> unit) -> unit
end

module Fit : sig
  type target = {
    width : int;
    height : int;
    pixel_format : Avutil.Pixel_format.t;
    pixel_aspect : Avutil.rational option;
  }

  val fitted_size :
    ?pixel_aspect:Avutil.rational ->
    ?source_aspect:Avutil.rational ->
    width:int ->
    height:int ->
    int * int ->
    int * int

  type t

  val create : target -> t

  (** Timestamps and colour properties kept. *)
  val convert : t -> Avutil.video Avutil.frame -> Avutil.video Avutil.frame list
end

module Aformat : sig
  type t

  val create :
    ?sample_format:Avutil.Sample_format.t ->
    ?channel_layout:Avutil.Channel_layout.t ->
    ?sample_rate:int ->
    time_base:Avutil.rational ->
    Avutil.Audio.frame_format ->
    t

  val time_base : t -> Avutil.rational

  val convert :
    t ->
    (Avutil.audio Avutil.frame -> unit) ->
    Avutil.audio Avutil.frame ->
    unit

  val eof : t -> (Avutil.audio Avutil.frame -> unit) -> unit
end
