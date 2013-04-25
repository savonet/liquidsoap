(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2013 Savonet team

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

(** FDK-AAC encoder *)

module G = Generator.Generator

let create_encoder params =
  let encoder =
    Fdkaac.Encoder.create params.Encoder.FdkAacEnc.channels
  in
  let params = [
    `Aot params.Encoder.FdkAacEnc.aot;
    `Bitrate (params.Encoder.FdkAacEnc.bitrate*1000);
    `Bitrate_mode params.Encoder.FdkAacEnc.bitrate_mode;
    `Granule_length params.Encoder.FdkAacEnc.granule_length;
    `Samplerate params.Encoder.FdkAacEnc.samplerate;
    `Sbr_mode params.Encoder.FdkAacEnc.sbr_mode;
    `Transmux params.Encoder.FdkAacEnc.transmux ]
  in
  List.iter (Fdkaac.Encoder.set encoder) params;
  encoder

let encoder aac =
  let enc = create_encoder aac in
  let channels = aac.Encoder.FdkAacEnc.channels in
  let samplerate = aac.Encoder.FdkAacEnc.samplerate in
  let samplerate_converter =
    Audio_converter.Samplerate.create channels
  in
  let src_freq = float (Frame.audio_of_seconds 1.) in
  let dst_freq = float samplerate in
  let n = 1024 in
  let buf = Buffer.create n in
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
    let encoded = Buffer.create n in
    Buffer.add_string buf (Audio.S16LE.make b start len);
    let len = Buffer.length buf in
    let rec f start =
      if start+n > len then
       begin
        Utils.buffer_drop buf start;
        Buffer.contents encoded
       end
      else
       begin
        let data = Buffer.sub buf start n in
        Buffer.add_string encoded
          (Fdkaac.Encoder.encode enc data 0 n);
        f (start+n)
      end
    in
    f 0
  in
  let stop () =
    let rem = Buffer.contents buf in
    let s =
      Fdkaac.Encoder.encode enc rem 0 (String.length rem)
    in
    s ^ (Fdkaac.Encoder.flush enc)
  in
    {
      Encoder.
       insert_metadata = (fun m -> ()) ;
       header = None ;
       encode = encode ;
       stop = stop
    }

let () =
  Encoder.plug#register "AAC"
    (function
       | Encoder.FdkAacEnc m -> Some (fun _ _ -> encoder m)
       | _ -> None)
