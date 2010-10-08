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

let check = Ogg_flac.Decoder.check_packet

let buflen = 1024

let decoder os =
  let ogg_dec = ref None in
  let packet = ref None in
  let decoder = ref None in
  let is_first = ref true in 
  let dummy_c  = 
    Ogg_flac.Decoder.get_callbacks (fun _ -> ()) 
  in
  let fill feed = 
    (* Decoder is created upon first decoding..*)
    let decoder,sample_freq,meta = 
      match !decoder with
        | None -> 
           let packet =
             match !packet with
               | None ->
                  let p = Ogg.Stream.get_packet os in
                  packet := Some p; p
               | Some p -> p
           in
           let ogg_dec = 
             match !ogg_dec with
               | None ->
                   let dec = Ogg_flac.Decoder.create packet os dummy_c in
                   ogg_dec := Some dec ;
                   dec
               | Some dec -> dec
           in
           let dec,info,meta = 
             Flac.Decoder.init ogg_dec dummy_c 
           in
           let samplerate = info.Flac.Decoder.sample_rate in
           decoder := Some (dec,samplerate,meta);
           dec,samplerate,meta
        | Some d -> d
    in
    let m = 
      if !is_first then
        ( is_first := false ; meta )
      else
       None
    in
    let c = 
      Ogg_flac.Decoder.get_callbacks 
       (fun ret -> feed ((ret,sample_freq),m)) 
    in
    Flac.Decoder.process decoder c
  in
  Ogg_demuxer.Audio fill

let () = Ogg_demuxer.ogg_decoders#register "flac" (check,decoder)

