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

module Resampler =
  Swresample.Make (Swresample.PlanarFloatArray) (Swresample.PlanarFloatArray)

let samplerate_converter channels =
  let chans = Avutil.Channel_layout.get_default channels in
  let in_freq = Lazy.force Frame.audio_rate in
  let rs = ref None in
  let rs_out_freq = ref 0 in
  fun x buf offset length ->
    let out_freq = int_of_float (float in_freq *. x) in
    if !rs = None || !rs_out_freq <> out_freq then (
      rs := Some (Resampler.create chans in_freq chans out_freq);
      rs_out_freq := out_freq);
    let rs = Option.get !rs in
    let data = Resampler.convert ~offset ~length rs buf in
    (data, 0, Audio.length data)

let () =
  Plug.register Audio_converter.Samplerate.converters "ffmpeg" ~doc:""
    samplerate_converter
