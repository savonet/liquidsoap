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

let samplerates = [8000; 12000; 16000; 24000; 48000]

let () =
  ignore
    (Dtools.Init.at_start (fun () ->
         let rate = Lazy.force Frame.audio_rate in
         let rec f = function
           | [] -> 48000
           | x :: l when x < rate -> f l
           | x :: _ -> x
         in
         Ogg_demuxer_opus_decoder.decoder_samplerate := f samplerates));
  Ogg_demuxer_opus_decoder.register ()
