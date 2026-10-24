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

(** Pixel-format choices (common.md §7) and the comparison of frame formats
    every consumer uses to decide a rebuild (common.md §9). *)

val has_alpha : Avutil.Pixel_format.t -> bool
val is_hardware : Avutil.Pixel_format.t -> bool

(** [`Yuva420p] when the format has alpha, [`Yuv420p] otherwise. *)
val internal_for : Avutil.Pixel_format.t -> Avutil.Pixel_format.t

(** The pixel format a codec encodes from: a configured name, else the preferred
    internal format when the codec supports it, else the first supported
    software format (with alpha when [alpha]), else [`Yuv420p].

    @raise Failure naming an unknown configured name. *)
val for_encoder :
  ?configured:string ->
  alpha:bool ->
  [ `Encoder ] Avcodec.Video.t ->
  Avutil.Pixel_format.t

(** Two video frame formats are the same: size and pixel format always count,
    colour and pixel aspect as [settings.ffmpeg.format_change.*] say, read at
    each call. *)
val same_video : Avutil.Video.frame_format -> Avutil.Video.frame_format -> bool

val describe_video : Avutil.Video.frame_format -> string
val describe_audio : Avutil.Audio.frame_format -> string

(** The scaling algorithm of [settings.ffmpeg.scaling_algorithm], read at each
    call (common.md §10). *)
val scaling_flag : unit -> Swscale.flag
