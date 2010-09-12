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

(** AAC+ encoder *)

open Encoder
open Encoder.AACPlus

let create_encoder ~samplerate ~bitrate ~channels =
  Aacplus.init ();
  let bitrate = bitrate*1000 in
  Aacplus.create ~channels ~samplerate ~bitrate ()

let encoder aacplus =
  let channels = aacplus.channels in
  let enc = create_encoder ~samplerate:aacplus.samplerate 
                         ~bitrate:aacplus.bitrate 
                         ~channels 
  in
  (* Aacplus accepts data of a fixed length.. *)
  let len = Aacplus.data_length enc in
  let tmp = String.create len in
  let rem = Buffer.create 1024 in
  let encoded = Buffer.create 1024 in
  let encode frame start len =
    let start = Frame.audio_of_master start in
    let b = AFrame.content_of_type ~channels frame start in
    let len = Frame.audio_of_master len in
    let s = String.create (2 * len * channels) in
    ignore (Float_pcm.to_s16le b start len s 0) ;
    Buffer.add_string rem s ;
    let data_length = String.length tmp in
    let rec put data =
      let len = String.length data in
      if len > data_length then
       begin
        let b = String.sub data 0 data_length in
        let data =
          String.sub data data_length (len-data_length)
        in
        Buffer.add_string encoded (Aacplus.encode enc b) ;
        put data
       end
      else
       begin
        Buffer.reset rem;
        Buffer.add_string rem data
       end
    in
    if Buffer.length rem > data_length then
      put (Buffer.contents rem) ;
    let ret = Buffer.contents encoded in
    Buffer.reset encoded ;
    ret
  in
    {
      insert_metadata = (fun m -> "") ;
      encode = encode ;
      stop = (fun () -> "")
    }

let () =
  Encoder.plug#register "AAC+"
    (function
       | Encoder.AACPlus m -> Some (fun _ -> encoder m)
       | _ -> None)
