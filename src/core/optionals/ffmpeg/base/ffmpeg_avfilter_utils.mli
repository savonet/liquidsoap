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

(** Shows decoded video upright: applies the rotation, the flip and the cropping
    its side data asks for, when [settings.ffmpeg.autorotate] is set. Every
    place that turns decoded frames into content goes through it. *)
module Display : sig
  type t

  (** [params] are the parameters of the stream the frames are decoded from;
      they hold the cropping. *)
  val init :
    ?params:Avutil.video Avcodec.params ->
    time_base:Avutil.rational ->
    unit ->
    t

  (** The picture {!convert} delivers for a stream, as far as its parameters
      tell. [None] when that is not decided: see {!set_on_undecided}. *)
  val layout :
    Avutil.video Avcodec.params -> Avfilter.Utils.display_layout option

  (** The size of {!layout}, and of the picture as stored when that is not
      decided: a hint for sizing video before the first frame. *)
  val expected_size : Avutil.video Avcodec.params -> int * int

  (** Installs, for every decoder created afterwards, what answers the cases the
      bindings do not decide. Without it they log a warning and leave the
      picture as stored. *)
  val set_on_undecided :
    (Avfilter.Utils.undecided_frame -> Avfilter.Utils.filter_spec list option) ->
    unit

  val convert :
    t ->
    Avutil.video Avutil.frame ->
    (Avutil.video Avutil.frame -> unit) ->
    unit

  val eof : t -> (Avutil.video Avutil.frame -> unit) -> unit
end

module Fps : sig
  type t

  val time_base : t -> Avutil.rational

  val init :
    ?start_pts:int64 ->
    width:int ->
    height:int ->
    pixel_format:Avutil.Pixel_format.t ->
    time_base:Avutil.rational ->
    ?pixel_aspect:Avutil.rational ->
    ?source_fps:int ->
    ?color_range:Avutil.Color_range.t ->
    target_fps:int ->
    unit ->
    t

  val convert :
    t -> [ `Video ] Avutil.frame -> ([ `Video ] Avutil.frame -> unit) -> unit

  val eof : t -> ([ `Video ] Avutil.frame -> unit) -> unit
end

module AFormat : sig
  type t

  val time_base : t -> Avutil.rational

  val init :
    ?dst_sample_format:Avutil.Sample_format.t ->
    ?dst_channel_layout:Avutil.Channel_layout.t ->
    ?dst_sample_rate:int ->
    src_sample_format:Avutil.Sample_format.t ->
    src_channel_layout:Avutil.Channel_layout.t ->
    src_sample_rate:int ->
    src_time_base:Avutil.rational ->
    unit ->
    t

  val convert :
    t -> [ `Audio ] Avutil.frame -> ([ `Audio ] Avutil.frame -> unit) -> unit

  val eof : t -> ([ `Audio ] Avutil.frame -> unit) -> unit
end
