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
  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301  USA

 *****************************************************************************)

module Swresample = FFmpeg.Swresample
module Resampler =
  Swresample.Make (Swresample.FltPlanarBigArray) (Swresample.FltPlanarBigArray)

let samplerate_converter () =
  let chans = `Mono in
  let in_freq = Lazy.force Frame.audio_rate in
  let rs = ref None in
  let rs_out_freq = ref 0 in
  fun x buf ->
    let out_freq = int_of_float (float in_freq *. x) in
    if !rs = None || !rs_out_freq <> out_freq then (
      rs := Some (Resampler.create chans in_freq chans out_freq) ;
      rs_out_freq := out_freq ) ;
    let rs = Utils.get_some !rs in
    (Resampler.convert rs [|buf|]).(0)

let () =
  Audio_converter.Samplerate.converters#register "ffmpeg" samplerate_converter
