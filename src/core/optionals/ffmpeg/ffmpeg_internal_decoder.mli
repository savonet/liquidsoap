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
  channels:int ->
  stream:(Avutil.input, Avutil.audio, [ `Frame ]) Av.stream ->
  field:Frame.field ->
  pcm_kind:Content.kind ->
  Avutil.audio Avcodec.params ->
  buffer:Decoder.buffer ->
  [ `Frame of Avutil.audio Avutil.Frame.t | `Flush ] ->
  unit

val mk_video_decoder :
  width:int ->
  height:int ->
  stream:(Avutil.input, Avutil.video, [ `Frame ]) Av.stream ->
  field:Frame.field ->
  Avutil.video Avcodec.params ->
  buffer:Decoder.buffer ->
  [ `Frame of Avutil.video Avutil.Frame.t | `Flush ] ->
  unit

val mk_text_subtitle_decoder :
  stream:(Avutil.input, [ `Subtitle ], [ `Frame ]) Av.stream ->
  field:Frame.field ->
  [ `Subtitle of Avutil.Subtitle.frame | `Flush ]
  Ffmpeg_decoder_common.sparse_decoder

val mk_bitmap_subtitle_decoder :
  stream:(Avutil.input, [ `Subtitle ], [ `Frame ]) Av.stream ->
  field:Frame.field ->
  width:int ->
  height:int ->
  [ `Subtitle of Avutil.Subtitle.frame | `Flush ]
  Ffmpeg_decoder_common.sparse_decoder
