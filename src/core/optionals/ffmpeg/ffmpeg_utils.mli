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

open Mm

val log : Log.t
val conf_ffmpeg : Dtools.Conf.ut
val conf_log : Dtools.Conf.ut
val conf_verbosity : string Dtools.Conf.t
val conf_level : int Dtools.Conf.t
val conf_scaling_algorithm : string Dtools.Conf.t
val liq_main_ticks_time_base : unit -> Avutil.rational
val liq_audio_sample_time_base : unit -> Avutil.rational
val liq_video_sample_time_base : unit -> Avutil.rational
val liq_frame_time_base : unit -> Avutil.rational
val liq_frame_pixel_format : Avutil.Pixel_format.t
val liq_frame_pixel_format_with_alpha : Avutil.Pixel_format.t
val pixel_format : 'a Avcodec.Video.t -> string option -> Avutil.Pixel_format.t
val pack_image : Image.YUV420.t -> (Image.Data.t * int) array

val unpack_image :
  width:int -> height:int -> (Image.Data.t * int) array -> Image.YUV420.t

val convert_time_base :
  src:Avutil.rational -> dst:Avutil.rational -> int64 -> int64

val mk_hardware_context :
  hwaccel:Ffmpeg_format.hwaccel ->
  hwaccel_pixel_format:Avutil.Pixel_format.t option ->
  hwaccel_device:string option ->
  opts:Avutil.opts ->
  target_pixel_format:Avutil.Pixel_format.t ->
  target_width:int ->
  target_height:int ->
  ([< `Audio | `Video ], Avcodec.encode) Avcodec.codec ->
  Avcodec.Video.hardware_context option * Avutil.Pixel_format.t

(* Ffmpeg doesn't really support a consistent duration API. Thus, we
   use PTS increment between packets to emulate duration. This means that
   a packet's duration is, in effect, the time between the last packet and
   the current one. We also need to group packets to make sure that we
   always submit chunks with non-null length. Some files have shown two
   successive packet with the same DTS. *)
module Duration : sig
  type 'a t

  val init :
    ?offset:int64 ->
    ?last_ts:int64 ->
    mode:[ `DTS | `PTS ] ->
    src:Avutil.rational ->
    convert_ts:bool ->
    get_ts:('a -> int64 option) ->
    set_ts:('a -> int64 option -> unit) ->
    get_duration:('a -> int64 option) ->
    unit ->
    'a t

  val last_ts : 'a t -> int64 option
  val push : 'a t -> 'a -> (int * (int * 'a) list) option
  val flush : 'a t -> int * (int * 'a) list
end
