(*****************************************************************************

  Liquidsoap, a programmable stream generator.
  Copyright 2003-2024 Savonet team

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

(** Resampling module for any Frame.content *)

type samplerate_converter =
  samplerate:int ->
  Content.Audio.data ->
  int ->
  int ->
  Content.Audio.data * int * int

val samplerate_converter : unit -> samplerate_converter

type wav_converter = bytes -> int -> int -> Content.Audio.data

(** samplesize is in bits. Formats: unsigned 8 bit (u8) or signed 16 bit little
    endian (s16le) *)
val from_iff :
  format:Wav_aiff.format -> channels:int -> samplesize:int -> wav_converter

type channels_converter = Content.Audio.data -> Content.Audio.data

val channels_converter :
  Audio_converter.Channel_layout.layout -> channels_converter

val video_scale :
  width:int ->
  height:int ->
  unit ->
  Video.Canvas.Image.t ->
  Video.Canvas.Image.t

type fps = { num : int; den : int }

val video_resample :
  unit ->
  in_freq:fps ->
  out_freq:fps ->
  Video.Canvas.image ->
  Video.Canvas.image list
