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

(** Decode mp3 audio from files parsed by ocaml-natty. *)

let log = Dtools.Log.make ["format";"natty+nsv"]

let decoder_mp3 get_next position f =
  let openfile file = 
    let file = f file in
    let get_next = get_next file in
    let fd =
      Mad.openstream get_next
    in
    let abg = Float_pcm.Generator.create () in
    fd,abg
  in
  let close _ = () in
  let decode fd abg =
    let data = Mad.decode_frame_float fd in
    let sample_freq,_,_ = Mad.get_output_format fd in
    Float_pcm.Generator.feed abg ~sample_freq data
  in
  let position _ = !position in
  let decoder =
   {
    File_decoder.Float.
     log = log;
     openfile = openfile;
     decode = decode;
     position = position;
     close = close
   }
  in
  File_decoder.Float.decode decoder

let () =
  let decoder =
    let f = Natty.Nsv.open_f in
    let position = ref 0 in
    let get_next x n =
      let payload =
        snd (Natty.Nsv.get_frame x)
      in
      position := !position + String.length payload.Natty.Nsv.audio;
      payload.Natty.Nsv.audio,String.length payload.Natty.Nsv.audio
    in
    decoder_mp3 get_next position f
  in
  Decoder.formats#register "NSV"
    (fun name -> try Some (decoder name) with _ -> None)
