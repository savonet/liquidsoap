(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2010 Savonet team

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

(** AAC encoder *)

open Encoder
open Encoder.VoAacEnc

module G = Generator.Generator

let create_encoder ~samplerate ~bitrate ~channels ~adts =
  let bitrate = bitrate*1000 in
  let params = 
    { Voaacenc.
       bitrate = bitrate;
       samplerate = samplerate;
       channels = channels;
       adts = adts }
  in
  Voaacenc.create params

let encoder aac =
  let channels = aac.channels in
  let samplerate = aac.samplerate in
  let enc = create_encoder ~samplerate ~channels
                         ~bitrate:aac.bitrate 
                         ~adts:aac.adts 
  in
  let samplerate_converter =
    Audio_converter.Samplerate.create channels
  in
  let src_freq = float (Frame.audio_of_seconds 1.) in
  let dst_freq = float samplerate in
  let data = 
    Buffer.create (Voaacenc.recommended_minimum_input enc) 
  in
  let encode frame start len =
    let start = Frame.audio_of_master start in
    let b = AFrame.content_of_type ~channels frame start in
    let len = Frame.audio_of_master len in
    let b,start,len =
      if src_freq <> dst_freq then
        let b = Audio_converter.Samplerate.resample
          samplerate_converter (dst_freq /. src_freq)
          b start len
        in
        b,0,Array.length b.(0)
      else
        b,start,len
    in
    let s16le = Audio.S16LE.make b start len in
    Buffer.add_string data s16le;
    Voaacenc.encode_buffer enc data
  in
    {
      insert_metadata = (fun m -> ()) ;
      header = None ;
      encode = encode ;
      stop = fun () -> ""
    }

let () =
  Encoder.plug#register "AAC"
    (function
       | Encoder.VoAacEnc m -> Some (fun _ _ -> encoder m)
       | _ -> None)
