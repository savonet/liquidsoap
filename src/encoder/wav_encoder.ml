(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2015 Savonet team

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

open Encoder.WAV

let encoder wav =
  let channels = wav.channels in
  let sample_rate = wav.samplerate in
  let sample_size = wav.samplesize in
  let ratio =
    (float sample_rate) /. (float (Lazy.force Frame.audio_rate))
  in
  let converter = Audio_converter.Samplerate.create channels in
  let len = 
    match wav.duration with
      | None -> None
      | Some d -> 
          Some (
            int_of_float (d *. (float channels) *. 
                               (float sample_rate) *.
                               (float sample_size) /. 8.))
  in
  let header =
    Wav_aiff.wav_header ?len ~channels ~sample_rate ~sample_size ()
  in
  let need_header = ref wav.header in
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
    let s = Bytes.create (sample_size / 8 * len * channels) in
    let of_audio =
      match sample_size with
        | 32 -> Audio.S32LE.of_audio
        | 24 -> Audio.S24LE.of_audio
        | 16 -> Audio.S16LE.of_audio
        | 8 -> Audio.U8.of_audio
        | _ -> failwith "unsupported sample size"
    in
    of_audio b start s 0 len;
    if !need_header then begin
      need_header := false ;
      header ^ s
    end else
      s
  in
    {
     Encoder.
      insert_metadata = (fun _ -> ()) ;
      encode = encode ;
      header = Some header ;
      stop = (fun () -> "")
    }

let () =
  Encoder.plug#register "WAV"
    (function
       | Encoder.WAV w -> Some (fun _ _ -> encoder w)
       | _ -> None)
