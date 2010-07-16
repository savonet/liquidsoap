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

(** WAV encoder *)

open Encoder
open Encoder.WAV

let encoder wav =
  let channels = wav.channels in
  let sample_rate = wav.samplerate in
  let ratio =
    (float sample_rate) /. (float (Lazy.force Frame.audio_rate))
  in
  let converter = Audio_converter.Samplerate.create channels in
  let header =
    Wav.header
      ~channels ~sample_rate
      ~sample_size:16 ~big_endian:false ~signed:true ()
  in
  let need_header = ref true in
  let encode frame start len =
    let start = Frame.audio_of_master start in
    let b = AFrame.content_of_type ~channels frame start in
    let len = Frame.audio_of_master len in
    (* Resample if needed. *)
    let b,start,len =
      if ratio = 1. then
        b,start,len
      else
        let b =
          Audio_converter.Samplerate.resample
                 converter ratio b start len
        in
        b,0,Array.length b.(0)
    in
    let s = String.create (2 * len * channels) in
    ignore (Float_pcm.to_s16le b start len s 0) ;
    if !need_header then begin
      need_header := false ;
      header ^ s
    end else
      s
  in
    {
      reset = (fun m -> "") ;
      encode = encode ;
      stop = (fun () -> "")
    }

let () =
  Encoder.plug#register "WAV"
    (function
       | Encoder.WAV w -> Some (fun _ -> encoder w)
       | _ -> None)
