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
  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301  USA

 *****************************************************************************)

open Mm

val log : Log.t
val log_start_atom : Dtools.Init.t
val conf_ffmpeg : Dtools.Conf.ut
val conf_log : Dtools.Conf.ut
val conf_verbosity : string Dtools.Conf.t
val conf_level : int Dtools.Conf.t
val conf_scaling_algorithm : string Dtools.Conf.t
val liq_main_ticks_time_base : unit -> Avutil.rational
val liq_audio_sample_time_base : unit -> Avutil.rational
val liq_video_sample_time_base : unit -> Avutil.rational
val liq_frame_time_base : unit -> Avutil.rational
val liq_frame_pixel_format : unit -> Avutil.Pixel_format.t
val pixel_format : 'a Avcodec.Video.t -> string option -> Avutil.Pixel_format.t
val pack_image : Image.YUV420.t -> (Image.Data.t * int) array

val unpack_image :
  width:int -> height:int -> (Image.Data.t * int) array -> Image.YUV420.t

val best_pts : _ Avutil.frame -> int64 option

val convert_time_base :
  src:Avutil.rational -> dst:Avutil.rational -> int64 -> int64

val mk_hardware_context :
  hwaccel:Ffmpeg_format.hwaccel ->
  hwaccel_device:string option ->
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
    ?pixel_aspect:Avutil.rational ->
    ?source_fps:int ->
    target_fps:int ->
    unit ->
    t

  val convert :
    t -> [ `Video ] Avutil.frame -> ([ `Video ] Avutil.frame -> unit) -> unit
end

(* Ffmpeg doesn't really support a consistent duration API. Thus, we
   use PTS increment between packets to emulate duration. This means that
   a packet's duration is, in effect, the time between the last packet and
   the current one. We also need to group packets to make sure that we
   always submit chunks with non-null length. Some files have shown two
   successive packet with the same DTS. *)
module Duration : sig
  type 'a t

  val init : src:Avutil.rational -> get_ts:('a -> Int64.t option) -> 'a t
  val push : 'a t -> 'a -> (int * (int * 'a) list) option
  val flush : 'a t -> (int * 'a) list
end
