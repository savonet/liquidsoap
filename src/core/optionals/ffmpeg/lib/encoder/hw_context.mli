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

(** Hardware context selection for a video encoder (common.md §11): the
    candidates of the codec's configurations, tried in order. [select] raises
    [Failure] when a requested method other than [auto] cannot be met, the
    caller's configuration error. *)

val select :
  log:Log.t ->
  hwaccel:Liquidsoap_ffmpeg_formats.Encoder_format.hwaccel ->
  ?hardware_pixel_format:Avutil.Pixel_format.t ->
  ?device:string ->
  opts:Avutil.opts ->
  target:Avutil.Pixel_format.t ->
  width:int ->
  height:int ->
  [ `Encoder ] Avcodec.Video.t ->
  Avcodec.Video.hardware_context option * Avutil.Pixel_format.t
