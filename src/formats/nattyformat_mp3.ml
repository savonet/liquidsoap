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

let decoder_mp3 get_next file =
  let fd =
    Mad.openstream get_next
  in
  let abg = Float_pcm.Generator.create () in
  let buffer_length = Decoder.buffer_length () in
  let stats = Unix.stat file in
  let file_size = stats.Unix.st_size in
  let in_bytes = ref 0 in
  let out_samples = ref 0 in
  let closed = ref false in
  let close () =
    assert (not !closed) ;
    closed := true
  in
  let fill =
    fun buf ->
      assert (not !closed) ;

      begin
        try
          while Float_pcm.Generator.length abg < buffer_length do
            let data = Mad.decode_frame_float fd in
            let sample_freq,_,_ = Mad.get_output_format fd in
              Float_pcm.Generator.feed abg ~sample_freq data
          done
        with
          | _ -> ()
      end ;

      let offset = AFrame.position buf in
        AFrame.fill_frame abg buf ;
        in_bytes := 15 ;
        out_samples := !out_samples + AFrame.position buf - offset ;
        (* Compute an estimated number of remaining ticks. *)
        let abglen = Float_pcm.Generator.length abg in
          if !in_bytes = 0 then
            0
          else
            let compression =
              (float (!out_samples+abglen)) /. (float !in_bytes)
            in
            let remaining_samples =
              (float (file_size - !in_bytes)) *. compression
              +. (float abglen)
            in
              (* I suspect that in_bytes in not accurate, since I don't
               * get an exact countdown after that in_size=in_bytes.
               * Instead, there is a stall at the beginning
               * after which the countdown starts. *)
              Fmt.ticks_of_samples (int_of_float remaining_samples)
  in
    { Decoder.fill = fill ; Decoder.close = close }

let () =
  let decoder name =
    let f = Natty.Nsv.open_f name in
    let get_next n =
      let payload =
        snd (Natty.Nsv.get_frame f)
      in
      payload.Natty.Nsv.audio,String.length payload.Natty.Nsv.audio
    in
    decoder_mp3 get_next name
  in
  Decoder.formats#register "NSV"
    (fun name -> try Some (decoder name) with _ -> None)
