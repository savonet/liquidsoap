(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2006 Savonet team

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

(** Decode and read metadatas of mp3 files. *)

open Dtools

let decoder file =

  let format = {
    Mixer.channels = Mad.wav_output_channels ;
    Mixer.sample_freq = Mad.samplefreq file ;
    Mixer.sample_size = Mad.wav_output_sample_size ;
    Mixer.big_endian = Mad.wav_output_big_endian ;
    Mixer.signed = Mad.wav_output_signed ;
  } in
  let fd = 
    Log.logl ~label:"mp3" 4 (lazy (Log.f "open %S" file)) ; 
    Mad.openfile file
  in
  let abg = Mixer.Generator.create () in
  let buffer_size = Decoder.buffer_size () in
  let stats = Unix.stat file in
  let in_size = stats.Unix.st_size in
  let in_bytes = ref 0 in
  let out_bytes = ref 0 in
  let closed = ref false in
  let close () =
    assert (not !closed) ;
    closed := true ;
    Log.logl ~label:"mp3" 4 (lazy (Log.f "close %S" file)) ;
    Mad.close fd
  in
  let fill = 
    fun buf ->
      assert (not !closed) ;

      begin
	try
          while Mixer.Generator.length abg < buffer_size do
            Mixer.Generator.feed abg format (Mad.decode_frame fd)
          done
        with
          | _ -> ()
      end ;

      let offset = Mixer.Buffer.position buf in
	Mixer.Buffer.fill buf abg ;
        in_bytes := Mad.get_current_position fd ;
        out_bytes := !out_bytes + (Mixer.Buffer.position buf) - offset ;
        (* Compute an estimated number of remaining frames. *)
        let abglen = Mixer.Generator.length abg in
          assert (!in_bytes!=0) ;
          let compression = (float (!out_bytes+abglen)) /. (float !in_bytes) in
          let remaining_bytes =
            (float (in_size - !in_bytes)) *. compression
            +. (float abglen)
          in
          let remaining_frames =
            int_of_float (ceil (remaining_bytes /. (float Mixer.Buffer.size)))
          in
            (* I suspect that in_bytes in not accurate, since I don't
             * get an exact countdown after than in_size=in_bytes, but there
             * is a stall at the beginning after which the countdown starts. *)
            remaining_frames
  in
    { Decoder.fill = fill ; Decoder.close = close }

let _ = Decoder.formats#register "MP3"
	  (fun name -> try Some (decoder name) with _ -> None)
