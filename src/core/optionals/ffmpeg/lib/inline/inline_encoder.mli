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

(** The inline encoder transforms (inline-codecs.md §6, §7, §9): audio and
    video, encoded to [ffmpeg.copy] or kept as raw frames, on the timeline of
    the input's source-id record (§5). The track operators hand them to
    {!Producer.create}. *)

val audio :
  log:Log.t ->
  encoded:Liquidsoap_ffmpeg_formats.Encoder_format.encoded ->
  a:Liquidsoap_ffmpeg_formats.Encoder_format.audio ->
  Producer.builder

val video :
  log:Log.t ->
  encoded:Liquidsoap_ffmpeg_formats.Encoder_format.encoded ->
  v:Liquidsoap_ffmpeg_formats.Encoder_format.video ->
  Producer.builder
