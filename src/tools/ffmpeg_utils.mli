(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2020 Savonet team

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

val log : Log.t
val conf_ffmpeg : Dtools.Conf.ut
val conf_log : Dtools.Conf.ut
val conf_verbosity : string Dtools.Conf.t
val conf_level : int Dtools.Conf.t
val conf_scaling_algorithm : string Dtools.Conf.t
val liq_master_ticks_time_base : unit -> Avutil.rational
val liq_audio_sample_time_base : unit -> Avutil.rational
val liq_video_sample_time_base : unit -> Avutil.rational
val liq_frame_time_base : unit -> Avutil.rational

val convert_time_base :
  src:Avutil.rational -> dst:Avutil.rational -> int64 -> int64

val mk_hardware_context :
  opts:Avutil.opts ->
  target_pixel_format:Avutil.Pixel_format.t ->
  target_width:int ->
  target_height:int ->
  ([< `Audio | `Video ], Avcodec.encode) Avcodec.codec ->
  Avcodec.Video.hardware_context option * Avutil.Pixel_format.t

module Fps : sig
  type t

  val time_base : t -> Avutil.rational

  val init :
    width:int ->
    height:int ->
    pixel_format:Avutil.Pixel_format.t ->
    time_base:Avutil.rational ->
    pixel_aspect:Avutil.rational ->
    ?source_fps:int ->
    target_fps:int ->
    unit ->
    t

  val convert :
    t -> [ `Video ] Avutil.frame -> ([ `Video ] Avutil.frame -> unit) -> unit
end
