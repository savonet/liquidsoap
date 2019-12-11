(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2019 Savonet team

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
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 *****************************************************************************)

(** Resampling module for any Frame.content *)

(** TODO: video ! :-) *)

type audio_converter =
  ?audio_src_rate:float -> Frame.audio_t array -> Frame.audio_t array

val create_audio : unit -> audio_converter

type wav_converter = audio_src_rate:float -> string -> Frame.audio_t array

(** samplesize is in bits.
      Formats: unsigned 8 bit (u8) or
               signed 16 bit little endian (s16le) *)
val create_from_iff :
  format:Wav_aiff.format -> channels:int -> samplesize:int -> wav_converter
