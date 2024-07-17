(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2007 Savonet team

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

module Generator = Float_pcm.Generator

let decoder file =

  let fd =
    Log.logl ~label:"mp3" 4 (lazy (Log.f "open %S" file)) ; 
    Mad.openfile file
  in
  let abg = Generator.create () in
  let buffer_length = Decoder.buffer_length () in
  let stats = Unix.stat file in
  let file_size = stats.Unix.st_size in
  let in_bytes = ref 0 in
  let out_samples = ref 0 in
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
          while Generator.length abg < buffer_length do
            let sample_freq,_,_ = Mad.get_output_format fd in
              Generator.feed abg ~sample_freq (Mad.decode_frame_float fd)
          done
        with
          | _ -> ()
      end ;

      let offset = AFrame.position buf in
        AFrame.fill_frame abg buf ;
        in_bytes := Mad.get_current_position fd ;
        out_samples := !out_samples + AFrame.position buf - offset ;
        (* Compute an estimated number of remaining ticks. *)
        let abglen = Generator.length abg in
          assert (!in_bytes<>0) ;
          let compression =
            (float (!out_samples+abglen)) /. (float !in_bytes)
          in
          let remaining_samples =
            (float (file_size - !in_bytes)) *. compression
            +. (float abglen)
          in
            (* I suspect that in_bytes in not accurate, since I don't
             * get an exact countdown after than in_size=in_bytes, but there
             * is a stall at the beginning after which the countdown starts. *)
            Fmt.ticks_of_samples (int_of_float remaining_samples)
  in
    { Decoder.fill = fill ; Decoder.close = close }

let () = Decoder.formats#register "MP3"
	  (fun name -> try Some (decoder name) with _ -> None)
