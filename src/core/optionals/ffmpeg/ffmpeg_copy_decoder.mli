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

val mk_audio_decoder :
  stream_idx:int64 ->
  format:Content_base.Contents.format ->
  field:int ->
  stream:(Avutil.input, Avutil.audio, [ `Packet ]) Av.stream ->
  Avutil.audio Avcodec.params ->
  buffer:Decoder.buffer ->
  [ `Packet of Avutil.audio Avcodec.Packet.t | `Flush ] ->
  unit

val mk_video_decoder :
  stream_idx:int64 ->
  format:Content_base.Contents.format ->
  stream:(Avutil.input, Avutil.video, [ `Packet ]) Av.stream ->
  field:int ->
  Avutil.video Avcodec.params ->
  buffer:Decoder.buffer ->
  [ `Packet of Avutil.video Avcodec.Packet.t | `Flush ] ->
  unit

val mk_subtitle_decoder :
  stream_idx:int64 ->
  format:Content_base.Contents.format ->
  stream:(Avutil.input, Avutil.subtitle, [ `Packet ]) Av.stream ->
  field:int ->
  Avutil.subtitle Avcodec.params ->
  buffer:Decoder.buffer ->
  [ `Packet of Avutil.subtitle Avcodec.Packet.t | `Flush ] ->
  unit
