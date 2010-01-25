(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2009 Savonet team

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

(** MP3 encoder *)

open Encoder
open Encoder.MP3

let create_encoder ~samplerate ~bitrate ~quality ~stereo =
  let enc = Lame.create_encoder () in
    (* Input settings *)
    Lame.set_in_samplerate enc (Lazy.force Frame.audio_rate) ;
    Lame.set_num_channels enc (if stereo then 2 else 1) ;
    (* Output settings *)
    Lame.set_mode enc (if stereo then Lame.Stereo else Lame.Mono) ;
    Lame.set_quality enc quality ;
    Lame.set_out_samplerate enc samplerate ;
    Lame.set_brate enc bitrate ;
    Lame.init_params enc ;
    enc

let encoder mp3 =
  let channels = if mp3.stereo then 2 else 1 in
  let e = create_encoder ~samplerate:mp3.samplerate 
                         ~bitrate:mp3.bitrate 
                         ~quality:mp3.quality 
                         ~stereo:mp3.stereo 
  in
  let encode frame start len =
    let start = Frame.audio_of_master start in
    let b = AFrame.content_of_type ~channels frame start in
    let len = Frame.audio_of_master len in
    if channels = 1 then
      Lame.encode_buffer_float_part e b.(0) b.(0) start len
    else
      Lame.encode_buffer_float_part e b.(0) b.(1) start len
  in
    {
      reset = (fun m -> "") ;
      encode = encode ;
      stop = (fun () -> "")
    }

let () =
  Encoder.plug#register "MP3"
    (function
       | Encoder.MP3 m -> Some (fun _ -> encoder m)
       | _ -> None)
